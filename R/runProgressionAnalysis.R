# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of PioneerTriptorelin
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Run progression analysis (landmark)
#'
#' Landmark Kaplan-Meier analysis of time to a first progression event after a
#' landmark time (default 183 days). For each target cohort x event cohort:
#'
#' \enumerate{
#'   \item Time-to-event is extracted \strong{from cohort entry} with
#'     \code{getTimeToEventChronic.sql} (the same extraction as the chronic
#'     safety endpoints): first event on/after entry or censoring at the earlier
#'     of the observation-period end and the cohort end date; prevalent cases
#'     (event before entry) excluded.
#'   \item The landmark is applied in R (Zabor method): keep only subjects still
#'     in follow-up and event-free at the landmark (\code{timeToEvent >=
#'     landmarkDays}), then reset the clock to the landmark
#'     (\code{lmTime = timeToEvent - landmarkDays}).
#'   \item Kaplan-Meier is estimated on \code{lmTime}, describing progression
#'     among landmark survivors.
#' }
#'
#' Events 1693 and 1694 (or whichever \code{eventCohortIds} are supplied) are
#' analysed as \strong{separate} endpoints; each target's landmark population is
#' defined by that specific event only. Individual-level time-to-event data is
#' held in memory only and never written to disk.
#'
#' @param connectionDetails    DatabaseConnector connection details object.
#' @param cdmDatabaseSchema    Schema containing the OMOP CDM tables.
#' @param cohortDatabaseSchema Schema where cohort tables are written.
#' @param cohortTable          Name of the cohort table.
#' @param targetCohortIds      Integer vector of target cohort definition IDs
#'   (e.g. the triptorelin cohort). \code{NA} entries are silently skipped. Names
#'   are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param eventCohortIds       Integer vector of progression event cohort IDs,
#'   each analysed against every target cohort. \code{NA} entries are silently
#'   skipped. Names are resolved from \code{inst/settings/CohortsToCreate.csv}.
#' @param landmarkDays         Landmark time in days. Subjects must be event-free
#'   and still in follow-up at this time to be included; the KM clock is reset to
#'   it. Default 183.
#' @param horizonDays          Horizon (days after the landmark) at which
#'   cumulative incidence of progression is reported. Default 1095 (3 years).
#' @param databaseId           Short site identifier used to label outputs.
#' @param minCellCount         Minimum number of events required to report the
#'   horizon / summary figures; smaller counts are suppressed. Default 5.
#' @param outputFolder         Path where result files will be written.
#'
#' @export
runProgressionAnalysis <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   targetCohortIds,
                                   eventCohortIds,
                                   landmarkDays = 183,
                                   horizonDays  = 1095,
                                   databaseId,
                                   minCellCount = 5,
                                   outputFolder) {

  progressionFolder <- file.path(outputFolder, "progressionAnalysis")
  if (!dir.exists(progressionFolder)) {
    dir.create(progressionFolder, recursive = TRUE)
  }

  # Resolve cohort names from CohortsToCreate.csv (IDs are the single source of truth)
  cohortsToCreate <- readr::read_csv(
    system.file("settings", "CohortsToCreate.csv", package = "PioneerTriptorelin"),
    col_types = readr::cols()
  )
  .cohortName <- function(id) {
    nm <- cohortsToCreate$cohort_name[cohortsToCreate$cohortId == id]
    if (length(nm) == 0 || is.na(nm[1])) as.character(id) else nm[1]
  }

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Containers for aggregated results across all target x event pairs
  allKmTables <- list()
  allSummary  <- list()

  for (ti in seq_along(targetCohortIds)) {
    targetCohortId <- targetCohortIds[[ti]]
    if (is.na(targetCohortId)) {
      ParallelLogger::logWarn("Skipping target at index ", ti, ": cohort ID is NA.")
      next
    }
    targetName <- .cohortName(targetCohortId)

    for (ei in seq_along(eventCohortIds)) {
      eventCohortId <- eventCohortIds[[ei]]
      if (is.na(eventCohortId)) {
        ParallelLogger::logWarn("Skipping event at index ", ei, ": cohort ID is NA.")
        next
      }
      eventName <- .cohortName(eventCohortId)
      key       <- paste0(targetCohortId, "_", eventCohortId)

      ParallelLogger::logInfo(
        "Progression analysis: target ", targetName, " (", targetCohortId, ") -> event ",
        eventName, " (", eventCohortId, "); landmark = ", landmarkDays, " days"
      )

      # ----------------------------------------------------------------------
      # 1. Extract time-to-event FROM INDEX (chronic extraction; memory only)
      #    time_to_event : days from cohort entry to first on/after-entry event
      #                    or censoring; prevalent cases excluded.
      # ----------------------------------------------------------------------
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename            = "getTimeToEventChronic.sql",
        packageName            = "PioneerTriptorelin",
        dbms                   = attr(connection, "dbms"),
        cohort_database_schema = cohortDatabaseSchema,
        cdm_database_schema    = cdmDatabaseSchema,
        cohort_table           = cohortTable,
        target_cohort_id       = targetCohortId,
        outcome_cohort_id      = eventCohortId
      )
      tteData <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)

      if (nrow(tteData) == 0) {
        ParallelLogger::logWarn(
          "No data for target ", targetCohortId, " / event ", eventCohortId, " — skipping."
        )
        next
      }

      # ----------------------------------------------------------------------
      # 2. Apply the landmark in R (Zabor method)
      #    - keep only subjects still in follow-up & event-free at the landmark
      #    - reset the clock to the landmark
      # ----------------------------------------------------------------------
      lmData <- tteData[!is.na(tteData$timeToEvent) & tteData$timeToEvent >= landmarkDays, ]
      landmarkN <- nrow(lmData)

      if (landmarkN == 0) {
        ParallelLogger::logWarn(
          "No subjects reach the ", landmarkDays, "-day landmark for target ",
          targetCohortId, " / event ", eventCohortId, " — skipping."
        )
        next
      }
      lmData$lmTime <- lmData$timeToEvent - landmarkDays

      # ----------------------------------------------------------------------
      # 3. Kaplan-Meier from the landmark
      # ----------------------------------------------------------------------
      kmFit <- survival::survfit(
        survival::Surv(lmTime, event) ~ 1,
        data = lmData
      )

      kmTable <- data.frame(
        time     = kmFit$time,
        n_risk   = kmFit$n.risk,
        n_event  = kmFit$n.event,
        n_censor = kmFit$n.censor,
        surv     = kmFit$surv,
        std_err  = kmFit$std.err,
        ci_lower = kmFit$lower,
        ci_upper = kmFit$upper
      )
      kmTable$cumulative_incidence <- 1 - kmTable$surv
      kmTable$target     <- targetName
      kmTable$event      <- eventName
      kmTable$databaseId <- databaseId
      allKmTables[[key]] <- kmTable

      nEvents <- sum(lmData$event)

      # Cumulative incidence at the horizon (days after the landmark)
      idxH <- which(kmTable$time <= horizonDays)
      if (length(idxH) > 0) {
        lastIdx        <- max(idxH)
        cumIncHorizon  <- 1 - kmTable$surv[lastIdx]
        ciLowerHorizon <- 1 - kmTable$ci_upper[lastIdx]
        ciUpperHorizon <- 1 - kmTable$ci_lower[lastIdx]
      } else {
        cumIncHorizon <- ciLowerHorizon <- ciUpperHorizon <- NA_real_
      }

      # Median time-to-progression from the KM fit (NA if not reached)
      medianProgression <- tryCatch(
        as.numeric(summary(kmFit)$table["median"]),
        error = function(e) NA_real_
      )

      # Suppress the horizon / summary figures on small event counts
      suppress <- is.na(nEvents) || nEvents < minCellCount

      allSummary[[key]] <- data.frame(
        target                     = targetName,
        event                      = eventName,
        landmarkDays               = landmarkDays,
        horizonDays                = horizonDays,
        landmarkN                  = landmarkN,
        nEvents                    = if (suppress) NA_integer_ else nEvents,
        cumulative_incidence_horizon = if (suppress) NA_real_ else round(cumIncHorizon, 4),
        ci_lower_horizon           = if (suppress) NA_real_ else round(ciLowerHorizon, 4),
        ci_upper_horizon           = if (suppress) NA_real_ else round(ciUpperHorizon, 4),
        median_days                = if (suppress) NA_real_ else medianProgression,
        databaseId                 = databaseId,
        stringsAsFactors           = FALSE
      )

      rm(tteData, lmData)
    }
  }

  # --------------------------------------------------------------------------
  # 4. Export aggregated results only
  # --------------------------------------------------------------------------
  if (length(allKmTables) > 0) {
    kmOut <- do.call(rbind, allKmTables)
    readr::write_excel_csv(kmOut, file.path(progressionFolder, "progressionKmTable.csv"), na = "")
    ParallelLogger::logInfo("Progression KM table written to progressionKmTable.csv")
  }
  if (length(allSummary) > 0) {
    summaryOut <- do.call(rbind, allSummary)
    readr::write_excel_csv(summaryOut, file.path(progressionFolder, "progressionSummary.csv"), na = "")
    ParallelLogger::logInfo("Progression summary written to progressionSummary.csv")
  }

  ParallelLogger::logInfo("Progression analysis complete. Results written to ", progressionFolder)
  invisible(NULL)
}
