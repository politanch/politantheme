##' Fetch Bezirksdata
##'
##' 
##'
##' @export




fetchBezAbst <- function (user, dbname, pwd, was, vorlagen = NULL) 
{
  if (!exists("user")) 
    stop("Politan-MySQL-User nicht definiert")
  if (!exists("dbname")) 
    stop("Politan-Datenbank nicht definiert")
  if (!exists("pwd")) 
    stop("Passwort fehlerhaft oder nicht vorhanden")
  if (!exists("was")) 
    stop("Was soll geholt werden?\n\nBeteiligung=BET\nAlles=ALLES\nJa-Stimmenanteil=JA")
  oo <- c("RMySQL", "tidyr", "dplyr")
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
  if (is.null(vorlagen)) {
  }
  else {
    if (class(vorlagen) == "integer") 
      vorlagen <- as.numeric(vorlagen)
    if (class(vorlagen) != "numeric") 
      stop("Vorlagen müssen nummerisch sein\n")
    if (length(vorlagen) > 1) {
      ids <- paste0(vorlagen, collapse = "|")
    }
    else {
      ids <- vorlagen
    }
    rm(vorlagen)
  }
  if (was == "ja") 
    was <- "JA"
  if (was == "bet") 
    was <- "BET"
  if (was == "all") 
    was <- "ALLES"
  if (was == "ALLES" && exists("from")) 
    message("Es wird alles heruntergeladen... Keine Einschränkung, obwohl definiert.")
  v_db <- dbConnect(MySQL(), user = USER, password = PWD, dbname = DBNAME, 
                    host = "localhost")
  message("Successfully connected...")
  all.data <- dbSendQuery(v_db, "SELECT * FROM `bez_abst`")
  all.data <- fetch(all.data, n = -1)
  message("Successfully fetched data...")
  suppressWarnings(dbDisconnect(v_db))
  
  test <- spread(all.data, variable, value)
  
  container <- data.frame()
  for(i in 1:length(unique(test$bfsnr))){
    u <- i
    zz <- subset(all.data, bfsnr==unique(test$bfsnr)[i])
    row.names(zz) <- NULL
    gut <- spread(zz, variable, value) %>% select(BEZNR, BER, PART, VAL, YES)
    names(gut)[2:length(gut)] <- paste0(unique(test$bfsnr)[i], "_", names(gut)[2:length(gut)])
    
    if(i==1){
      container <- gut
    }
    if(i>1){
      container <- merge(container, gut, by="BEZNR")
    }
    
  }
  all.data <- container
  
  if (was == "ALLES") {
    bezirksnummer <- grep("BEZNR", names(all.data))
    if (exists("ids")) 
      ja <- all.data[, c(bezirksnummer, grep(ids, names(all.data)))]
    message("häve fön")
    return(ja)
  }
  if (was == "JA") {
    yes <- all.data[, grep("YES|NO|VAL", names(all.data))]
    vorlagen <- gsub("[[:alpha:]]", "", names(yes))
    vorlagen <- gsub("[[:punct:]]", "", vorlagen)
    vorlagen <- unique(vorlagen)
    ja <- data.frame(BEZNR = all.data$BEZNR)
    for (i in 1:length(vorlagen)) {
      tt <- all.data[, grep(vorlagen[i], names(all.data))]
      if (vorlagen[i] != 5521 | vorlagen[i] != 5522) {
        umweg <- (tt[, grep("YES", names(tt))]/tt[, grep("VAL", 
                                                         names(tt))]) * 100
      }
      else {
        umweg <- (tt[, grep("YES", names(tt))]/(tt[, 
                                                   grep("YES", names(tt))] + tt[, grep("NO", names(tt))])) * 
          100
      }
      umweg <- data.frame(umweg = umweg)
      names(umweg) <- vorlagen[i]
      ja <- cbind(ja, umweg)
    }
    message("ok")
    bezirksnummer <- grep("BEZNR", names(ja))
    if (exists("ids")) 
      ja <- ja[, c(bezirksnummer, grep(ids, names(ja)))]
    return(ja)
  }
  if (was == "BET") {
    yes <- all.data[, grep("BER|PART", names(all.data))]
    vorlagen <- gsub("[[:alpha:]]", "", names(yes))
    vorlagen <- gsub("[[:punct:]]", "", vorlagen)
    vorlagen <- unique(vorlagen)
    ja <- data.frame(BEZNR = all.data$BEZNR)
    for (i in 1:length(vorlagen)) {
      tt <- yes[, grep(vorlagen[i], names(yes))]
      umweg <- (tt[, grep("PART", names(tt))]/tt[, grep("BER", 
                                                        names(tt))]) * 100
      umweg <- data.frame(umweg = umweg)
      names(umweg) <- vorlagen[i]
      ja <- cbind(ja, umweg)
    }
    bezirksnummer <- grep("BEZNR", names(ja))
    if (exists("ids")) 
      ja <- ja[, c(bezirksnummer, grep(ids, names(ja)))]
    return(ja)
  }
}

