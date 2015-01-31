best <- function(state, outcome) {

  ## Read outcome data
  rawData <- read.csv("outcome-of-care-measures.csv")

  ##Force state/outcome to correct capitalization
  state<-toupper(state)
  outcome<-tolower(outcome)
  
  ## Check that state is valid
  if(!state %in% rawData$State){
    stop("invalid state")
  }
  
  #Vector of outcome options
  outOpts<-data.frame(c("heart attack", "heart failure", "pneumonia"),c(11,17,23))
  colnames(outOpts)<-c("option","num")
  
  ## Check that outcome is valid...
  if(is.na(match(outcome,outOpts$option))){
    stop("invalid outcome")
  }else{
    # and assign column number to 'outcome' based on
    # user-provided 'outcome' string
    outcome<-outOpts[outOpts$option==outcome,2]   
        ## figure out later why replacing '2' with 'outOpts$num'
        ## produces error "undefined columns selected".
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateSel<-rawData[rawData$State==state,]  # Pull out just hospitals from chosen state
  stateSel<-stateSel[!stateSel[outcome]=="Not Available",]  # Remove NAs
  ## Replace factors in 'outcome' column with numerals, to allow computation.
  stateSel[,outcome]<-as.numeric(levels(stateSel[,outcome])[stateSel[,outcome]])
  best<-stateSel[match(min(stateSel[,outcome]),stateSel[,outcome]),2]  #Hospital Name = col 2
  return(levels(best)[best])
}