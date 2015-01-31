rankhospital <- function(state, outcome, num = "best"){

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
  stateSel<-rawData[rawData$State==state,]  # Pull out hospitals from chosen state
  stateSel<-stateSel[!stateSel[outcome]=="Not Available",]  # Remove NAs
  
  ## Define "best" and "worst" for 'num'
  if(num=="best"){
    num <- 1
  }else if(num=="worst"){
    num <- length(stateSel[,1])
  }
  
  ## Replace factors in 'outcome' column with numerals, to allow computation.
  stateSel[,outcome]<-as.numeric(levels(stateSel[,outcome])[stateSel[,outcome]])
  
  # Order alphabetically by hospital name to break any future ties
  ord <- order(stateSel[,2])
  stateSel <- stateSel[ord,]
  # And order by 'outcome' in descending order
  ord <- order(stateSel[,outcome])
  stateSel <- stateSel[ord,]
  
  choice <- tryCatch(stateSel[num,2], finally = NA)
  return(levels(choice)[choice])
}