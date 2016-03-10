library(XML)
library(stringr)
library(RCurl)
library(jsonlite)

lgTbl <- data.frame()

for(i in 2016:2016){
  
  theUrl <- paste("http://football99.myfantasyleague.com/",i,"/index?YEAR=",i,"&SEARCH=dynasty", sep="")
  
  doc <- htmlParse(theUrl,encoding="UTF-8")
  
  urls <- xpathSApply(doc, "//a/@href")
  
  urlTbl <- data.frame(URL=urls[str_count(urls,"[0-9]{5}$")>0])
  
  urlTbl$URL <- as.character(urlTbl$URL)
  
  urlTbl$LID <- str_extract(urlTbl$URL,"[0-9]+$")
  
  colnames(urlTbl) <- c("URL","LID")
  
  urlTbl$SEAS <- str_extract(urlTbl$URL,"[0-9]{4}")
  
  lgTbl <- rbind(lgTbl,urlTbl)
  
}

for(i in 1:nrow(lgTbl)){
  
  url <- lgTbl$URL[i]
  
  h <- basicHeaderGatherer()
  doc <- getURI(url, headerfunction = h$update)
  loc <- h$value()["Location"]
  
  lgTbl$LOC[i] <- loc
  
}

lgTbl$DOM <- str_extract(lgTbl$LOC,"[0-9]+")

lgTbl <- lgTbl[!is.na(lgTbl$DOM),]

lgTbl$SEAS <- as.numeric(as.character(lgTbl$SEAS))

ruleTbl <- data.frame()

for(i in 1:nrow(lgTbl)){
  
  url <- paste("http://www",lgTbl$DOM[i],".myfantasyleague.com/",lgTbl$SEAS[i],"/export?TYPE=league&L=",lgTbl$LID[i],"&W=&JSON=1",sep="")
  
  k <- try(fromJSON(readLines(url)[1]),silent=T)
  
  if(class(k)!="try-error"){
    
    pList <- k$league$starters$position
    
    pList$SEAS <- lgTbl$SEAS[i]
    
    pList$LID <- lgTbl$LID[i]
    
    ruleTbl <- rbind(ruleTbl,pList)
    
  }
  
}

drafts <- data.frame()

for(i in 1:nrow(lgTbl)){
  
  url <- paste("http://www",lgTbl$DOM[i],".myfantasyleague.com/",lgTbl$SEAS[i],"/export?TYPE=draftResults&L=",lgTbl$LID[i],"&W=&JSON=1",sep="")
  
  k <- try(fromJSON(readLines(url)[1]),silent=T)
  
  if(class(k)!="try-error"){
    
    pickTbl <- k$draftResults$draftUnit$draftPick
    
    if(length(pickTbl)>0){
      
    pickTbl$SEAS <- lgTbl$SEAS[i]
    
    pickTbl$LID <- lgTbl$LID[i]
    
    drafts <- rbind(drafts,pickTbl)
    
    }
    
  }
  
}

url <- paste("http://football.myfantasyleague.com/",i,"/export?TYPE=players&DETAILS=1&JSON=1",sep="")
  
k <- try(fromJSON(readLines(url)[1]),silent=T)

plyrTbl <- k$players$player
