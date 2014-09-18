rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

    
    ## List of outcomes 
    
    outcomes <- c("heart attack","heart failure","pneumonia")
    
    if((outcome%in%outcomes)==FALSE) stop("invalid outcome")
    
    ## heart attack es el 11
    ##heart failure es el 17
    ## pneumonia es el 23
    
    if (outcome==outcomes[1]) outcomeindex<-11
    if (outcome==outcomes[2]) outcomeindex<-17
    if (outcome==outcomes[3]) outcomeindex<-23
    

    ## nos quedamos solo con lo que interesa  nombre, estado y outcome
    data<-data[,c(2,7,outcomeindex)]    
    data[,3]<-as.numeric(data[,3])
   
    salida<-sapply(split(data,data[,2]),rankgroup,num)

#   formatear la salida como dice el ejercicio
#       
    salida<-as.data.frame(salida)
    
# la segunda columna debe tener los nombres de fila  
    salida[2]<-row.names(salida)
    colnames(salida)<-c("hospital","state")
    salida

}


rankgroup <-function(dataAn,num){
    
    #ojo que vienne con el nombre en el 1 y el estado en el 3
    
    
    dataAn<-dataAn[!is.na(dataAn[,3]),]
    # los ordeno 
    
    dataAn<-dataAn[order(dataAn[,3],dataAn[,1]),]
    
    
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