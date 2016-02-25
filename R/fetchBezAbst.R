##' Fetch Bezirksdata
##'
##' 
##'
##' @export





fetchBezAbst <- function(user, dbname, pwd, was, from=NULL, to=NULL){
  
  if(!exists("user"))stop("Politan-MySQL-User nicht definiert")
  if(!exists("dbname"))stop("Politan-Datenbank nicht definiert")
  if(!exists("pwd"))stop("Passwort fehlerhaft oder nicht vorhanden")
  if(!exists("was"))stop("Was soll geholt werden?\n\nBeteiligung=BET\nAlles=ALLES\nJa-Stimmenanteil=JA")
  
  if("RMySQL" %in% rownames(installed.packages())){library(RMySQL)}else{
    install.packages("RMySQL")
    library(RMySQL)
  }
  
  USER <- user
  PWD <- pwd
  DBNAME <- dbname
  if(exists("from")) {
    FROM <-from
    if(exists("to")){
      TO <- to
      
    }else{stop("ENDE BFSNR fehlt")}
  }
  
  
  
  
  if(was=="ja") was <- "JA"
  if(was=="bet") was <- "BET"
  if(was=="all") was <- "ALLES"
  if(was=="ALLES"&&exists("from")) message("Es wird alles heruntergeladen... Keine Einschränkung, obwohl definiert.")
  
  v_db <- dbConnect(MySQL(),
                    user=USER, password=PWD,
                    dbname=DBNAME, host="localhost")
  message("Successfully connected...")
  
  
  all.data <- dbSendQuery(v_db, "SELECT * FROM `bez_abst`")
  all.data <- fetch(all.data, n = -1)
  message("Successfully fetched data...")
  suppressWarnings(dbDisconnect(v_db))
  
  if(was=="ALLES"){
    
    bezirksnummer <- grep("BEZNR", names(all.data))
    if(exists("FROM")) all.data <- all.data[,c(bezirksnummer, grep(FROM, names(all.data)):grep(TO, names(all.data)))]
    
    message("häve fön")
    return(all.data)
  }
  
  if(was=="JA"){
    yes <- all.data[,grep("YES|NO|VAL", names(all.data))]
    vorlagen <- gsub("[[:alpha:]]", "", names(yes))
    vorlagen <- gsub("[[:punct:]]", "", vorlagen)
    vorlagen <- unique(vorlagen)
    
    ja <- data.frame(BEZNR=all.data$BEZNR)
    for(i in 1:length(vorlagen)){
      tt <- all.data[, grep(vorlagen[i], names(all.data))]
      
      #Ja-Stimmenanteil
      if(vorlagen[i]!=5521|vorlagen[i]!=5522){
        umweg <- (tt[,grep("YES", names(tt))]/tt[,grep("VAL", names(tt))])*100
      }else{
        umweg <- (tt[,grep("YES", names(tt))]/(tt[,grep("YES", names(tt))]+tt[,grep("NO", names(tt))]))*100
      }
      
      umweg <- data.frame(umweg=umweg)
      names(umweg) <- vorlagen[i]
      ja <- cbind(ja, umweg)
    }
    
    bezirksnummer <- grep("BEZNR", names(ja))
    if(exists("FROM")) ja <- ja[,c(bezirksnummer, grep(FROM, names(ja)):grep(TO, names(ja)))]
    
    
    return(ja)
  }
  
  if(was=="BET"){
    yes <- all.data[,grep("BER|PART", names(all.data))]
    vorlagen <- gsub("[[:alpha:]]", "", names(yes))
    vorlagen <- gsub("[[:punct:]]", "", vorlagen)
    vorlagen <- unique(vorlagen)
    
    ja <- data.frame(BEZNR=all.data$BEZNR)
    for(i in 1:length(vorlagen)){
      tt <- yes[, grep(vorlagen[i], names(yes))]
      
      umweg <- (tt[,grep("PART", names(tt))]/tt[,grep("BER", names(tt))])*100
      umweg <- data.frame(umweg=umweg)
      names(umweg) <- vorlagen[i]
      ja <- cbind(ja, umweg)
      
    }
    
    bezirksnummer <- grep("BEZNR", names(ja))
    if(exists("FROM")) ja <- ja[,c(bezirksnummer, grep(FROM, names(ja)):grep(TO, names(ja)))]
    
    return(ja)
    
  }
  
  
  
}

