##' Politan Color Theme
#' Calculate mean of measuring like government proposes.
#' 
#' @param user A username.
#' @param password A password.
#' @param dbname Name of Database
#' @param start Number of ballot 
#' @param end Number of ballot
#' @return List of Swiss districts with a value.
#' @examples
#' bez_br_index(user="Otto", password="Pumuckl", dbname="Test", start=3060, end=3090)



bez_br_index <- function(user=NULL, password=NULL, dbname=NULL, start=NULL, end=NULL){
  
  want <- "RMySQL" %in% rownames(installed.packages())
  if(want!=T){
    cat("Installing Package «RMySQL» first...\n")
    install.packages("RMySQL")
  }
  require(RMySQL)
  
  userr <- user
  passwd <- password
  dbna <- dbname
  v_start <- start
  v_end <- end
  
  if(!exists("userr")) 
    stop("No user defined")
  if(!exists("passwd")) 
    stop("No user defined")
  if(!exists("dbna")) 
    stop("No name of databank defined")
  if(!exists("v_start")) 
    print("Von Abstimmung 3060... ")
  
  
  
  v_db <- dbConnect(MySQL(),
                    user=userr, password=passwd,
                    dbname=dbna, host="localhost")
  on.exit(dbDisconnect(v_db))
  
  dbq <- dbSendQuery(v_db, "SELECT `bfsnr` AS bfsnr, `swbrpos` AS swbrpos FROM `nat_det`")
  dbq <- fetch(dbq, n = -1)
  
  bez <- dbSendQuery(v_db, "SELECT * FROM `bez_jaanteile`")
  bez <- fetch(bez, n = -1)
  
  #leeren Datensatz erstellen, wo der Durchschnittswert eingesetzt werden kann
  bez.index <- as.data.frame(bez[,"BEZNR"])
  bez.index$value <- NA
  names(bez.index) <- c("BEZNR", "value")
  
  for(i in 1:length(bez.index$BEZNR)){
    
    sub <- subset(bez, BEZNR==bez.index$BEZNR[i])
    
    oho <- as.data.frame(t(sub))
    rowss <- rownames(oho)
    
    oho <- cbind(rowss, oho)
    rownames(oho) <- NULL
    oho <- oho[-1,]
    names(oho) <- c("bfsnr", "ja_perc")
    oho$bfsnr <- gsub("ID_", "", oho$bfsnr)
    
    if(exists("v_start")) oho <- subset(oho, bfsnr>=v_start)
    if(exists("v_end")) oho <- subset(oho, bfsnr<=v_end)
    
    oho <- plyr::join(oho, dbq, by="bfsnr", type="left")
    
    oho$ja_perc <- as.numeric(oho$ja_perc)
    oho$posvalue <- NA
    oho <- oho[complete.cases(oho[,3]),]
    oho$posvalue[oho$swbrpos==1] <- oho$ja_perc[oho$swbrpos==1]
    oho$posvalue[oho$swbrpos==2] <- 100-oho$ja_perc[oho$swbrpos==2]
    
    value <- mean(oho$posvalue)
    
    bez.index$value[i] <- value
    
  }
  return(bez.index)
}
