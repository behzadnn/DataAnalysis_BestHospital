rankall<-function(outcome, num = "best"){
  dataHospital <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings =c("NA","Not Available"))
  dataHospital[, 11] <- as.numeric(dataHospital[, 11])
  states<-dataHospital$State
  states_factor<-as.factor(states)
  names_states<-levels(states_factor)
  counter<-1
  stateData<-list()
  resultData<-list()
  for (state in names_states){
    DataForState<-dataHospital[dataHospital$State==state,]
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
    stateData[counter]<-state
    resultData[counter]<-result
    counter<-counter+1
  }
  FinalResult<-data.frame(hospital=as.character(resultData),state=as.character(stateData))
}