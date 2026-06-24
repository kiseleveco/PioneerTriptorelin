library(ROhdsiWebApi)
base.url <- "https://pioneer.hzdr.de/WebAPI"
token <- "Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJhbmRyZWkua2lzZWxldkBiZG1jb25zdWx0aW5nLmZyIiwiU2Vzc2lvbi1JRCI6bnVsbCwiZXhwIjoxNzc5ODU0MjI2fQ.Ql6nome9Qcf1raya21kdPt3UwkoTW1onuz57NXDCpwt8MP6HFhtUpE2adGB1jhlzCWdv_wxuikPQi4RQCQWf3w"

ROhdsiWebApi::setAuthHeader(base.url, token)
ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = "inst/settings/CohortsToCreate.csv",
                                                 baseUrl = base.url,
                                                 packageName = "demoPackage")

cohort_json <- getCohortDefinition(cohortId = 262, baseUrl = base.url)

#next step: verify that cohort name and id is correct in csv file
CohortsToCreate <- read.csv("inst/settings/CohortsToCreate.csv", header = 1)

for (i in 1:nrow(CohortsToCreate)){
  cohort_json <- getCohortDefinition(cohortId = CohortsToCreate[i, "cohortId"], baseUrl = base.url)
  if (cohort_json$name != CohortsToCreate[i, "cohort_name"]){
    stop(paste0("ERROR: ", cohort_json$name, " has the name ", CohortsToCreate[i, "cohort_name"], " in a CohortsToCreate.csv"))
  }
}


