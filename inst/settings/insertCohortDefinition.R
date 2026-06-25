library(ROhdsiWebApi)
base.url <- "https://pioneer.hzdr.de/WebAPI"
token <- "Bearer "

ROhdsiWebApi::setAuthHeader(base.url, token)
ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = "inst/settings/CohortsToCreate.csv",
                                                 baseUrl = base.url,
                                                 packageName = "PioneerTriptorelin")

cohort_json <- getCohortDefinition(cohortId = 262, baseUrl = base.url)

#next step: verify that cohort name and id is correct in csv file
CohortsToCreate <- read.csv("inst/settings/CohortsToCreate.csv", header = 1)

for (i in 1:nrow(CohortsToCreate)){
  cohort_json <- getCohortDefinition(cohortId = CohortsToCreate[i, "cohortId"], baseUrl = base.url)
  if (cohort_json$name != CohortsToCreate[i, "cohort_name"]){
    stop(paste0("ERROR: ", cohort_json$name, " has the name ", CohortsToCreate[i, "cohort_name"], " in a CohortsToCreate.csv"))
  }
}


