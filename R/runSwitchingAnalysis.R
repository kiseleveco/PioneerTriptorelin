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

#' Run treatment switching analysis
#'
#' Estimates the 3-year Kaplan-Meier probability of switching from triptorelin
#' to an alternative androgen deprivation therapy: relugolix, degarelix,
#' leuprolide, goserelin, buserelin, or surgical castration.
#'
#' @param connectionDetails    DatabaseConnector connection details object.
#' @param cdmDatabaseSchema    Schema containing the OMOP CDM tables.
#' @param cohortDatabaseSchema Schema where cohort tables are written.
#' @param cohortTable          Name of the cohort table.
#' @param databaseId           Short site identifier used to label outputs.
#' @param outputFolder         Path where result files will be written.
#'
#' @export
runSwitchingAnalysis <- function(connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 databaseId,
                                 outputFolder) {

  stop(
    "runSwitchingAnalysis() is not yet implemented. ",
    "Set runSwitchingAnalysis = FALSE in execute() to skip this step."
  )
}
