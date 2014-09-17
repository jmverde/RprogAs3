rankhospital <- function(state, outcome,num = "best") {
## Read outcome data

	data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
## Check that state and outcome are valid

## Make a list of states

	states <- unique(data$State)
	

	if ((state%in%states)==FALSE) stop("invalid state")

## List of outcomes 

	outcomes <- c("heart attack","heart failure","pneumonia")

	if((outcome%in%outcomes)==FALSE) stop("invalid outcome")

## heart attack es el 11
##heart failure es el 17
## pneumonia es el 23

if (outcome==outcomes[1]) outcomeindex<-11
if (outcome==outcomes[2]) outcomeindex<-17
if (outcome==outcomes[3]) outcomeindex<-23

## Return hospital name in that state with lowest 30-day death
## rate

	splitOutcome<-split(data,data$State)
	dataState<-splitOutcome[[state]]

## preparo un dataset mas peque?o solo con las columnas que interesan
## el mombre de hospital 2 y el outcomeindex

## como se han leido como texto es necesario meter un numeric a saco en los
## valores numericos,  es importante hacerlo solo en esos por que si no podemos
## joder codigos
    
    dataAn<-dataState[c(2,outcomeindex)]
    dataAn[,2]<-as.numeric(dataAn[,2])
    
# me cargo los na
    
    dataAn<-dataAn[!is.na(dataAn[,2]),]
# los ordeno 
    
    dataAn<-dataAn[order(dataAn[,2],dataAn[,1]),]
    
    
    if (num == "best")  {
        num<-1
    }
    else if (num == "worst") {
        num<- nrow(dataAn)
    }
    
    else {}
    
    if (num>nrow(dataAn)) {
        resultado <- NA
    }
    else{
        
        resultado <- dataAn[num,1]    
    }
        resultado
}
