# Problem explanation
# 3 Ranking hospitals by outcome in a state
# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have data on a particular outcome should
# be excluded from the set of hospitals when deciding the rankings.
# Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
# of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
# the hospitals with lowest 30-day mortality rate for heart failure are shown here.
# > head(texas)
# Hospital.Name Rate Rank
# 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
# 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
# 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
# 3954 DETAR HOSPITAL NAVARRO 8.7 4
# 4010 METHODIST HOSPITAL,THE 8.8 5
# 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6
# Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
# (8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
# scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
# manner (i.e. where one vector is used to break ties in another vector).
# The function should use the following template.
# rankhospital <- function(state, outcome, num = "best") {
#   ## Read outcome data
#   ## Check that state and outcome are valid
#   ## Return hospital name in that state with the given rank
#   ## 30-day death rate
# }
# The function should check the validity of its arguments. If an invalid state value is passed to best, the
# function should throw an error via the stop function with the exact message “invalid state”. If an invalid
# outcome value is passed to best, the function should throw an error via the stop function with the exact
# message “invalid outcome”.
# Here is some sample output from the function.
# 3
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
# Save your code for this function to a file named rankhospital.R.
# Use the submit script provided to submit your solution to this part
state<-"NC"
outcome<-"heart attack"
num<-"worst"
 dataHospital <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings =c("NA","Not Available"))
  dataHospital[, 11] <- as.numeric(dataHospital[, 11])
  heartAttackRate<-dataHospital$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  hist(heartAttackRate)
  if (sum(dataHospital$State==state)==0){
    stop("Invalid state")
  }else{
    DataForState<-dataHospital[dataHospital$State==state,]
  }                         
  if(outcome=="heart attack"){
    chosenData<-as.numeric(DataForState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  }else if  (outcome=="heart failure"){
    chosenData<-as.numeric(DataForState$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  }else if  (outcome=="pneumonia"){
    chosenData<-as.numeric(DataForState$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  }else{
    stop("Invalid Outcome")}                         
  chosenData_cleaned<-chosenData[!is.na(chosenData)]
  hospital_names<-DataForState$Hospital.Name[!is.na(chosenData)]
  #print(hospital_names)
  chosenData_cleaned_sorted<-chosenData_cleaned[order(chosenData_cleaned,hospital_names)]
  hospital_names_sorted<-hospital_names[order(chosenData_cleaned,hospital_names)]
  if (num=="best"){
    rank<-1
  }else if(num=="worst"){
    rank<-( end(chosenData_cleaned_sorted)[1])
  } else{
    rank<-as.numeric(num)}
 #ChosenHospital_rate<-chosenData_cleaned_sorted[rank]
 # bestHospital<-hospital_names[chosenData_cleaned==ChosenHospital_rate]
  if (rank>end(chosenData_cleaned_sorted)[1]){
    ChosenHospital<-NA
  }else{
    ChosenHospital<-hospital_names_sorted[rank]
  }
  result<-ChosenHospital
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name 0.in that state with lowest 30-day death
  ## rate
  source("rankhospital.R")
  result<-rankhospital("NC", "heart attack", "worst")
  print(result)
