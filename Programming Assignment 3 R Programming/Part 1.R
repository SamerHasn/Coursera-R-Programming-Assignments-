table1 <- read.csv("outcome-of-care-measures.csv",
                   colClasses = "character",na.strings = "Not Available")

best <- function(state, outcome) {
  if (!is.data.frame(table1)) 
    {table1 <- read.csv("outcome-of-care-measures.csv",
               colClasses = "character",na.strings = "Not Available")}
  
  if (outcome=="heart attack") {outcome<- 11
    }else if (outcome=="heart failure") {outcome<- 17
    }else if (outcome== "pneumonia") {outcome<- 23
    }else stop(" invalid outcome") #Test for ivalid outcome error
     if (sum(state==table1$State)==0) stop("Invalid state") #Test for ivalid state error
  
  result<-split(table1,table1$State) #Splitting the data frame by states
    mini<-result[[state]] #selecting the state, then create a data frame of hospitals of the selected state
    mini1<-as.numeric(mini[,outcome])
  
  MIN<-min(mini1,na.rm = T) #The "min" of the selected outcome 
  
  h.name<-mini[mini1==MIN,][,2]
  h.name<-sort(h.name,decreasing = F)
  h.name<-as.character(h.name)
  return(h.name)
}

#Tests
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")