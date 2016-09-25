##' calculate corrs
##'
##' 
##'
##' @export


getCorrs <- function(user, dbname, pwd, vorlage, vergleich, level=NULL){
  
  if(!exists("user"))stop("Politan-MySQL-User nicht definiert")
  if(!exists("dbname"))stop("Politan-Datenbank nicht definiert")
  if(!exists("pwd"))stop("Passwort fehlerhaft oder nicht vorhanden")
  if(!exists("vorlage"))stop("\nFür welche Vorlage soll die Korrelation geholt werden?\n")
  if(!exists("vergleich"))stop("\nabst = Abstimmungen oder...\nrest = andere Bezirksdaten?\n")
  if(is.null(level)) {
    LEVEL <- 0.75
    message("Correlationsniveau nicht definiert, verwende 0.75\n")
  }else{
    if(!is.numeric(level)){
      stop("level muss nummerisch sein")
    }
    LEVEL <- level
  }
  
  
  oo <- c("RMySQL", "dplyr")
  o <- oo %in% rownames(installed.packages())
  if(length(which(o==FALSE))>0){
    install.packages(oo[which(o==FALSE)])
  }
  library(RMySQL)
  library(tidyr)
  library(dplyr)
  
  USER <- user
  PWD <- pwd
  DBNAME <- dbname
  if(class(vorlage)=="numeric"){
    if(nchar(vorlage)==4){}else{message("Die Vorlage bitte mit 4 digits eingeben.\n")}
    VORLAGE <- vorlage
  }else{
    stop("Die Vorlage ist nicht nummerisch.\n")
  }
  
  
  DBVerbindung <- dbConnect(MySQL(),
                            user=USER, password=PWD,
                            dbname=DBNAME, host="localhost")
  message("Successfully connected...")
  
  
  
  if(vergleich=="abstimmungen"){
    abst <- dbSendQuery(DBVerbindung, "SELECT * FROM `bez_abst_prov`;") %>%
      fetch(n = -1)
    
    perc <- grep("PERC", names(abst))
    if(length(perc)!=0){
      #wenn keine prozente vorhanden sind in bez_abst_prov, müssen diese berechnet werden
      yes <- abst[,grep("YES|NO|VAL", names(abst))]
      vorlagen <- gsub("[[:alpha:]]", "", names(yes))
      vorlagen <- gsub("[[:punct:]]", "", vorlagen)
      vorlagen <- unique(vorlagen)
      ja <- data.frame(BEZNR=abst$BEZNR)
      for(i in 1:length(vorlagen)){
        tt <- abst[, grep(vorlagen[i], names(abst))]
        
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
    }else{
      ja <- abst[,grep(VORLAGE, names(abst))]
      ja <- ja[,grep("PERC", names(ja))]
      ja <- cbind(BEZNR=abst[,"BEZNR"], ja) %>%
        as.data.frame
      
      names(ja)[2] <- VORLAGE
      
    }
    
    
    
    
    
    all.abst <- fetchBezAbst(user=USER, pwd=PWD,
                             dbname=DBNAME, was="ja")
    
    
    
    fuer.cor <- merge(all.abst, ja, by="BEZNR")
    
    fuer.cor <- fuer.cor %>%
      select(-BEZNR)
    message("aha")
    aa <- as.data.frame(cor(fuer.cor))
    aa$bfsnr <- row.names(aa)
    relevant <- aa[,c(grep("bfsnr", names(aa)), grep(VORLAGE, names(aa)))]
    relevant <- relevant[which(relevant[,2]>=LEVEL),]
    message("aha1")
    
    KEY <- suppressWarnings(dbSendQuery(DBVerbindung, "SELECT * FROM `nat_key`;"))%>%
      fetch(n = -1)%>%
      select(-ergebnis, -parlid)
    suppressWarnings(dbDisconnect(DBVerbindung))
    
    message("aha2")
    relevant <- merge(relevant, KEY, by="bfsnr")
    #relevant <- cbind(relevant, KEY[which(KEY$bfsnr %in% relevant$bfsnr),])
    message("aha3")
    row.names(relevant) <-  NULL
    for(i in 1:nrow(relevant)){
      print(relevant[i,"bfsnr"])
      Sys.sleep(1)
    }
    
    return(relevant)
    
  }
  
  
  message("Guets Kafisatzläse")
  
  
  if(vergleich=="rest"){
    abst <- dbSendQuery(DBVerbindung, "SELECT * FROM `bez_abst_prov`;") %>%
      fetch(n = -1)
    
    
    perc <- grep("PERC", names(abst))
    if(length(perc)!=0){
      #wenn keine prozente vorhanden sind in bez_abst_prov, müssen diese berechnet werden
      yes <- abst[,grep("YES|NO|VAL", names(abst))]
      vorlagen <- gsub("[[:alpha:]]", "", names(yes))
      vorlagen <- gsub("[[:punct:]]", "", vorlagen)
      vorlagen <- unique(vorlagen)
      ja <- data.frame(BEZNR=abst$BEZNR)
      for(i in 1:length(vorlagen)){
        tt <- abst[, grep(vorlagen[i], names(abst))]
        
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
    }else{
      ja <- abst[,grep(VORLAGE, names(abst))]
      ja <- ja[,grep("PERC", names(ja))]
      ja <- cbind(BEZNR=abst[,"BEZNR"], ja) %>%
        as.data.frame
      
      names(ja)[2] <- VORLAGE
      
    }
    
    
    referenz <- ja[,c(grep("BEZNR", names(ja)), grep(VORLAGE, names(ja)))]
    
    all.daten <- suppressWarnings(dbSendQuery(DBVerbindung, "SELECT * FROM `bez_daten`;")) %>%
      fetch(n = -1)
    suppressWarnings(dbDisconnect(DBVerbindung))
    
    all.daten[] <- suppressWarnings(sapply(all.daten, as.numeric))
    all.daten<-all.daten[, colSums(is.na(all.daten)) != nrow(all.daten)]
    
    
    
    oha <- merge(referenz, all.daten, by="BEZNR")
    
    
    
    all.cor <- oha[,-1]
    aa <- as.data.frame(cor(all.cor))
    aa$trulla <- row.names(aa)
    relevant <- aa[,c(grep("trulla", names(aa)), grep(VORLAGE, names(aa)))]
    row.names(relevant) <- NULL
    relevant2 <- relevant[which(relevant[,2]>=LEVEL),]
    
    
    
    return(relevant2)
    
  }
  
  
  
  
  
}

