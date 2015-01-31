rankall <- function(outcome, num = "best"){

  ## Read outcome data
  rawData <- read.csv("outcome-of-care-measures.csv")

  ##Force state/outcome to correct capitalization
  outcome<-tolower(outcome)
  
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
  
  worstTrigger <- FALSE
  
  ## Define "best" and "worst" for 'num'
  if(num=="best"){
    num <- 1
  }else if(num=="worst"){
    worstTrigger <- TRUE
  }
  
  ## Create data frame to hold final data
  final <- data.frame(1:54,1)
  colnames(final) <- c("hospital","state")
  # Variable to keep track of the current row when adding data to 'final'
  n=1
  
#   Couldn't figure out how to get 'rawData' column of state abbreviations
#   into a vector of individual values.  No matter what I tried it remained
#   a vector of length one, which I could not correctly iterate over.  WHAT
#   THE FUCK!!!!!!!!!!!!!!
  iter <- c( "AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT","WA","WI","WV","WY")
  
  for(state in iter){
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    stateSel<-rawData[rawData$State==state,]  # Pull out hospitals from chosen state
    stateSel<-stateSel[!stateSel[outcome]=="Not Available",]  # Remove NAs
    
    # Order alphabetically by hospital name to break any future ties
    ord <- order(stateSel[,2])
    stateSel <- stateSel[ord,]
    # And order by 'outcome' in descending order
    ord <- order(as.numeric(levels(stateSel[,outcome])[stateSel[,outcome]]))
    stateSel <- stateSel[ord,]
    
    ## Replace factors in 'outcome' column with numerals, to allow computation.
    stateSel[,outcome]<-as.numeric(levels(stateSel[,outcome])[stateSel[,outcome]])
    
    # If looking for worst hospital in the state
    if(worstTrigger == TRUE){
      num <- length(stateSel[,1])
    }
    
    # Write final data to the 'final' data frame
    final[n,1] <- as.character(stateSel[num,2])
    final[n,2] <- as.character(state)
    # Increment by 1 to save next state's date on the next line
    n <- n+1
  }

    return(final)
}    