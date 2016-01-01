
best <- function(state, outcome) {
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
  chosenData_min<-min(chosenData_cleaned)
  index_min<-chosenData==chosenData_min
  names<-DataForState$Hospital.Name[!is.na(index_min)]
  index_min<-index_min[!is.na(index_min)]
  bestHospital<-names[index_min]
  result<-min(bestHospital)
  print(result)
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name 0.in that state with lowest 30-day death
  ## rate
}