library(stringr)
library(XML)

#We're going to create our first function. The following creates a function that will standardize some text replacement and clean up. It goes through a number of steps to strip out odd characters, capitalize all words, and strip JRs and some other things like leading spaces. We can re-use this function by calling it like any function textClean()

textClean <- function(x){
  
  x <- toupper(str_replace(x," \\|([A-Za-z\\ \\/])*$",""))
  
  x <- (str_replace_all(x,"-"," "))
  
  x <- (str_replace_all(x,"[\\.\\']",""))
  
  x <- (str_replace_all(x,"[^A-Z]"," "))
  
  x <- (str_replace_all(x," JR",""))
  
  x <- (str_replace_all(x,"[ ]+"," "))
  
  x <- (str_replace_all(x,"^ ",""))
  
  x <- (str_replace_all(x,"^DS ",""))
  
}

#nfl draft scout has summary pages for the prospects which rank them by position. We're going to pull the player page urls out of the summary pages. But first we have to iterate through all of the summary pages which are broken out by draft year and position. So first we have to create a list of positions that we'll iterate through

posList <- list("QB","WR","TE","RB","FB")

# then we need to create a list of offsets to iterate through. the Draft Scout url will list 10 prospects start with an offset. so we'll iterate through this list and plug it into a url.

offList <- c(0,10,20,30,40,50,60,70,80,90)

#if we want to see how the loop will work, but without committing to actually running the loop, we can just set the variables that the loop will use and then run each line of the code within the loop on its own

posCntr <- 1

offCntr <- 1

dy <- 2014

urlDF <- data.frame()

#now that we've set the variables that the loop would have otherwise used, you could just jump down to the dsUrl line in order to see how that works. Or, if you just want to run the loop then you can highlight the entire thing and click run

for(posCntr in 1:5){
  
  for(offCntr in 1:10){
    
    for(dy in 2014:2015){
      
      #here's the nested loop where all of the iteration will take place. it will go through all 5 positions, for each year 2000-2013, and then for each offset to ensure you get the top 100 prospects at each position.
      
      #this line will just substitute all of those changing variables into the url
      
      dsUrl <- paste("http://www.nfldraftscout.com/ratings/probe.php?genpos=",posList[posCntr],"&draftyear=",dy,"&sortby=tsxpos&order=ASC&startspot=",offList[offCntr], sep="")
      
      #then we'll parse the page
      
      doc <- htmlParse(dsUrl)
      
      #once we've parsed the page we'll pull out of href attribute of every link on the page. this is done using a powerful tool called XPATH. so it applies this search for every anchor href throughout the entire page
      
      tempDF <- data.frame(href=as.character(xpathSApply(doc, "//a/@href")))
      
      #then we have to concatenate the lists of links.  this is done with rbind() which binds rows. if we wanted to bind columns together we'd use cbind instead of rbind
      
      urlDF <- rbind(urlDF,tempDF)
      
    }
    
  }
  
}

#once we've downloaded all of the href/urls, we can eliminate any duplicates with one simple call

urlDF <- unique(urlDF)

#don't ask me why this following step is necessary

urlDF <- as.character(urlDF$href)

urlDF <- data.frame(href=urlDF)

#because our loop pulled every single url from every page, the list contains a lot of urls that we don't need. we just want the ones that include the text 'dsprofile' as those are the ones related to players. this step will throw out the entries that don't contain that text

urlDF <- data.frame(href=urlDF[grep("dsprofile",urlDF$href,fixed=T),])

#the following step will throw out partial urls, which don't contain http

urlDF <- data.frame(href=urlDF[grep("http",urlDF$href,fixed=T),])

#Now we have a full list of the player pages that we can iteratively download and pull out the workout information from each. So we'll create a for loop that will loop once for each row of the urlDF table

#in order to see how each piece of this code works, let's just set i = 1 so that we can go line by line through the code without requiring that the full loop run

i <- 1

#For your first trip through the following code, don't run the for loop part of it. just start running the code on the line that starts "plUrl" as that will let you see how each line works. Later you can come back and highlight/run the entire for loop and it will download all of the pages.

#Note also that if you don't have time to run the entire loop, you can change the call so that if you've downloaded say the first 1,000 already, you could just change the call so that it's for(i in 1001:nrow(urlDF)) - that way you can download some data, then quite for awhile and then come back later.

for(i in 1:nrow(urlDF)){
  
  #we tell R to set the plUrl variable so that it is the line number equal to i, which is the variable we're using for iteration.
  
  plUrl <- as.character(urlDF$href)[i]
  
  # then we parse the page at the url
  
  doc <- htmlParse(plUrl)
  
  #this line uses XPATH to pull out each node in the HTML that is a font tag inside a td tag. It will dump each instance to a list
  
  fnts <- xpathSApply(doc, "//td/font", xmlValue)
  
  # you can inspect the list that was returned by running the following line of code
  
  fnts
  
  #you can access individual list items by calling their index numbers. but note that this is just a one dimensional list, not a 2 dimensional data frame. so accessing a list item doesn't require a comma.
  
  fnts[4]
  
  # we need to find the lines where the combine and pro day entries start. this line creates a variable that is the first instance in the list where the list item is Height: - height is the first standard entry for measurements.
  
  start1 <- min(which(fnts=="Height:"))
  
  #each workout data set ends with 3 cone drill, so we're just creating a variable that we'll use later to find the list item that the combine information ends on. we're using min and max to say whether we want the first occurrence of the text we're looking for, or the last occurrence.
  
  end1 <- min(which(fnts=="3-Cone Drill:"))
  
  #then we create variables for the beginning and ending list items for the pro day workouts. we've switched to max because the pro day workouts are the last occurrence of the entries we're looking at.
  
  start2 <- max(which(fnts=="Height:"))
  
  end2 <- max(which(fnts=="3-Cone Drill:"))
  
  #this line takes our list that we created, fnts, and then makes a subset of that list based on what we know about the index for where the combine information is located. we want every other list item (that's the 2 argument in the seq() function - if we wanted every third item we would use 3 instead of 2) starting with the item after start1 and then we go until we hit the item after end1. after you run this line take a look at the results using the data browser in the upper right in RStudio. note that when we first made the subset of the list using the seq function, it came out with 10 rows and 1 column. in order to switch that around we use the t() function which transposes it to 10 columns and 1 row.
  
  cmbResults <- data.frame(t(fnts[seq(start1+1,end1+1,2)]))
  
  #We can create column names for our newly created dataframe using a similar process. Except whereas in the previous line we went start1+1 in this line we get rid of the +1 because we just want the list item that contains the field names. those list items are every other item starting with start1 and ending with end1
  
  cn <- fnts[seq(start1,end1,2)]
  
  #then we'll go through some steps to shorten up and clean up the field names. First we'll delete Yrd because it's not necessary
  
  cn <- str_replace_all(cn,"Yrd","")
  
  #this removes the vowels
  
  cn <- str_replace_all(cn,"[aeiouAEIOU]","")
  
  #this removes some pesky punctuation.  the \\ are telling R that the character that follows is a literal character and not an operator
  
  cn <- str_replace_all(cn,"[\\:\\.\\-]","")
  
  #this removes spaces
  
  cn <- str_replace_all(cn," ","")
  
  #then we'll convert to uppercase
  
  cn <- toupper(cn)
  
  #then we'll shorten so that when in the future we reference these fields/columns we won't be typing more than we have to. this just uses the first 4 letters or numbers
  
  cn <- str_extract(cn,"[A-Z0-9]{4}")
  
  #this line adds a c_ to the front of each list item. that c_ will stand for combine. we'll do this because we're going to create pro day fields as well and we need to be able to distinguish between them. we'll use pd_ for pro day
  
  cn <- paste("c_",cn,sep="")
  
  #now we assign these field names to the column headers of our data frame.
  
  colnames(cmbResults) <- cn
  
  #then we essentially go through the same steps for the pro day information.
  
  pdResults <- data.frame(t(fnts[seq(start2+1,end2+1,2)]))
  
  cn <- fnts[seq(start2,end2,2)]
  
  cn <- str_replace_all(cn,"Yrd","")
  
  cn <- str_replace_all(cn,"[aeiouAEIOU]","")
  
  cn <- str_replace_all(cn,"[\\:\\.\\-]","")
  
  cn <- str_replace_all(cn," ","")
  
  cn <- toupper(cn)
  
  cn <- str_extract(cn,"[A-Z0-9]{4}")
  
  cn <- paste("pd_",cn,sep="")
  
  colnames(pdResults) <- cn
  
  #Now we have two tables that contain the pro day information and combine information. we can use cbind in order to bind the two tables together width-wise. If we wanted to bind the tables length wise then we would use rbind()
  
  woResults <- cbind(cmbResults,pdResults)
  
  #the 4th item of the fnts list always contains some name and position information. so we'll create a new variable name called pName to hold that information
  
  pName <- fnts[5]
  
  #then it will make it easier to work with if we split it by comma. this line splits pName at each comma and then returns a list
  
  pName <- unlist(strsplit(pName,","))
  
  #then we'll use the helper function that we defined at the top of this code. this will clean things up quite a bit and give us more uniform information.
  
  pName <- textClean(pName)
  
  #the pName list contains 3 items based on how it was split using the commas. after our helper function does it's magic we have three pretty clean pieces of information that we can add to our table. we just create a new column in the woResults table for each piece of information. we're referencing each item in the pName list by its index
  
  woResults$NAME <- pName[1]
  
  woResults$POS <- pName[2]
  
  woResults$SCHOOL <- pName[3]
  
  #it's also probably helpful to know the player's draft year. that information is contained in the 12th item of our fnts list.
  
  dy <- fnts[13]
  
  # we can really easily pull out the only 4 consecutive numeric digits using the following line of code. this gets rid of the other less useful information
  
  dy <- as.numeric(str_extract(dy,"[0-9]{4}"))
  
  #then we can add this column to our woResults table
  
  woResults$DRAFTYEAR <- dy
  
  #this line just re-arranges the table to take the last three columns (columns 21-24) and put them furthest to the left
  
  woResults <- woResults[,c(21:24,1:20)]
  
  #some of the fields have quotes or double quotes that will screw with things when we write to a csv. so we'll remove those quotes and also change the notation 1/2 so that it's .5 - the following line uses lapply to iteratively apply the same text cleaning functions to a list of columns.
  
  woResults[,2:24] <- lapply(woResults[,2:24],function(x) str_replace(x,"\\'"," "))
  woResults[,2:24] <- lapply(woResults[,2:24],function(x) str_replace(x,'\\"',""))
  woResults[,2:24] <- lapply(woResults[,2:24],function(x) str_replace(x,' 1/2',".5"))
  woResults[,2:24] <- lapply(woResults[,2:24],function(x) str_replace(x,'1/2',".5"))
  
  #we can write the information to a csv at each pass through the loop. this will be helpful in the event that we want to run i from 1:1000 and then take a break, and then come back later and run i from 1001:2000. each pass through the loop will just write the player's data to the csv. also, the two if statements that follow tell R to do something different depending on whether the dsTable file already exists or not. if the file already exists then we're just appending rows on the end.
  
  
  if(!file.exists("~/dsTable.csv")){
    
  write.table(woResults,file="~/dsTable.csv",sep=",",row.names=F)
  
  }
  
  if(file.exists("~/dsTable.csv")){
    
    write.table(woResults,file="~/dsTable.csv",sep=",",row.names=F,col.names=F,append=T)
    
  }

}


