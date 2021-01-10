table1 <- read.csv("outcome-of-care-measures.csv",
                   na.strings = "Not Available")

rankall <- function(outcome, num = "best") {
  if (!is.data.frame(table1)) 
  {table1 <- read.csv("outcome-of-care-measures.csv",
                      na.strings = "Not Available")}
  
  if (outcome=="heart attack") {outcome<- 11
  }else if (outcome=="heart failure") {outcome<- 17
  }else if (outcome== "pneumonia") {outcome<- 23
  }else stop(" invalid outcome") #Test for ivalid outcome error
  
  table2<-table1[,c(2,7,outcome)]
  result<-split(table2,table2$State)
  states<-levels(as.factor(table1$State))
  ordered.h<-data.frame("hospital" =0,"state"=0)
  
  if (num=="best") {
    for (i in seq(states)) {
      result2<-result[[i]]
      result3<-result2[order(as.numeric(result2[,3]),as.character(result2[,1]),decreasing = F),]
      name2<-as.character(result3[1,1])
      states<-as.character(result3[1,2])
      ordered.h[i,]<-c(name2,states)
    }
  }else if (num=="worst") {
    for (i in seq(states)) {
      result2<-result[[i]]
      result2<-na.omit(result2)
      result3<-result2[order(as.numeric(result2[,3]),as.character(result2[,1]),decreasing = T),]
      name2<-as.character(result3[1,1])
      states<-as.character(result3[1,2])
      ordered.h[i,]<-c(name2,states)
    }
  }else if(is.numeric(num)) {
    for (i in seq(states)) {
      result2<-result[[i]]
      result2<-na.omit(result2)
      result3<-result2[order(as.numeric(result2[,3]),as.character(result2[,1]),decreasing = F),]
      name2<-as.character(result3[num,1])
      states<-as.character(result3[1,2])
      ordered.h[i,]<-c(name2,states)
    }
  }else stop("invalid num")
  
  return(ordered.h)
}

#Test
head(rankall("heart attack", T), 10)
tail(rankall("pneumonia", "worst"), 3)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)


