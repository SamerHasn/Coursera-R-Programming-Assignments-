table1 <- read.csv("outcome-of-care-measures.csv",
                   na.strings = "Not Available")

rankhospital <- function(state, outcome, num = "best") {
  if (!is.data.frame(table1)) 
  {table1 <- read.csv("outcome-of-care-measures.csv",
                      na.strings = "Not Available")}
  
  if (outcome=="heart attack") {outcome<- 11
    }else if (outcome=="heart failure") {outcome<- 17
    }else if (outcome== "pneumonia") {outcome<- 23
    }else stop(" invalid outcome") #Test for ivalid outcome error
    if (sum(state==table1$State)==0) stop("Invalid state") #Test for ivalid state error
  
  table2<-table1[,c(2,7,outcome)]
  result<-split(table2,table2$State) #Splitting the data frame by states
  mini<-result[[state]] #selecting the state, then create a data frame of hospitals of the selected state
  mini1<-mini[,3]
  
  if (num=="best") {ordering<-F
    }else if (num=="worst") {ordering<-T
      ordered.ha<-mini[order(mini[,3],mini[,1],decreasing = ordering),]
      ordered.ha<-as.character(ordered.ha[1,1])
    }else if (is.numeric(num)){
      ordered.ha<-mini[order(mini[,3],mini[,1],decreasing = F),]
      ordered.ha<-as.character(ordered.ha[num,1])
    }else stop("invalid num")
  
  return(ordered.ha)
}

#Tests
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

