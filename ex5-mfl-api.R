library(XML)
library(stringr)
library(RCurl)
library(jsonlite)

#in general it's bad practice in R to rely on loops because there are other ways to handle your calculations through vectors. However, in scraping data from the web I often rely on loops to iterate through a list of urls that I need to visit for instance. So in this exercise I'll use a few loops.

#first we declare an empty data frame that we're going to use to append the results of our scraping to. 

lgTbl <- data.frame()

#before we run the loop that's going to pull in the MFL pages, let's go through each line of code in the loop first to see how each line works. then we'll put it all in a loop. so first i'm just going to define i (which i'll later use as a looping variable) as 2015.

i <- 2015

#now i'm going to define a url to scrape. using a paste function i'm going to pass it the value of i, which is 2015. this sets the url to the league search page for MFL with the string search of dynasty

theUrl <- paste("http://football99.myfantasyleague.com/",i,"/index?YEAR=",i,"&SEARCH=dynasty", sep="")

#now we use htmlParse from the XML library to parse the url

doc <- htmlParse(theUrl,encoding="UTF-8")

#below is one of the powerful abilities of coding. using just one line i can get the href property of every link in the page. if you know anything about html you know that href holds the destination url for a link. xpathSApply takes the object we created in the step above and then uses xpath to find all of the hrefs of the link. xpath is extremely powerful for scraping data so it will be good to become familiar with it. you could also use it for instance to pull in the 7th column of a table. but in this case we'll just get the hrefs out of the page

urls <- xpathSApply(doc, "//a/@href")

#now that i have a list of urls i want to discard the ones that don't have 5 consecutive numeric values in the url. this uses str_count and then says if the count isn't greater than 0 throw it out. what we're left with should be just urls that are links to leagues (there may have been other urls in the page that we don't need).

urlTbl <- data.frame(URL=urls[str_count(urls,"[0-9]{5}$")>0])

#the values in the URL column of the data frame we've just created are a factor. i need them to be in character format to do more str_extract on them. so i just convert them to character here.

urlTbl$URL <- as.character(urlTbl$URL)

#the league id is located in a 5 digit number at the end of the url. i tell r to extract the values that match 0-9 up to an unlimited number of times, at the end of the string. the $ operator is the regex operator to say start at the end. regex can be a steep learning curve but it's really valuable for parsing text.

urlTbl$LID <- str_extract(urlTbl$URL,"[0-9]+$")

#later we're going to want to refer to leagues by league id and by season since MFL recycles league ids each season. so let's get a column in our data frame that reflects the season of the league we're looking at. we can do this by setting it equal to our i variable since that will always be the same as the season we're searching.

urlTbl$SEAS <- i

#now we'll rbind the urlTbl we just made to the empty lgTbl we declared earlier.

lgTbl <- rbind(lgTbl,urlTbl)

#that's the entirety of the functionality of our first loop. we wrote everything so that it would use the iterating variable i. now all we need to do is wrap it in a loop. here's a loop in R so that you can see how it works. in this exercise all we'll be doing is passing variables through loops to scrape data. but you can also use those variables to do calculations as i've done below. note that to run a loop in R you need to click Run on every line until you've gone through each line. if you ever just want to run a single line of code inside the loop then just don't click Run on the for() line of code. simply drop your cursor on the line you want to run.

for(j in 1:10){
  
  print(j)
  
}

for(j in 1:10){
  
  print(j + 10)
  
}

#let's clear out that lgTbl by setting it as a new data frame.

lgTbl <- data.frame()

#next is the call that contains the general structure of the loop. we're going to pull in all of the league links for leagues in the 2015 and 2016 season that contain the word dynasty in the title. this code is the same as the code above but it's wrapped by the loop. again, all of the code interior to the loop you've already seen above. you need to click Run on each line of the loop, or highlight the entire loop and then click Run

for(i in 2015:2016){
  
  theUrl <- paste("http://football99.myfantasyleague.com/",i,"/index?YEAR=",i,"&SEARCH=dynasty", sep="")
  
  doc <- htmlParse(theUrl,encoding="UTF-8")
  
  urls <- xpathSApply(doc, "//a/@href")
  
  urlTbl <- data.frame(URL=urls[str_count(urls,"[0-9]{5}$")>0])
  
  urlTbl$URL <- as.character(urlTbl$URL)
  
  urlTbl$LID <- str_extract(urlTbl$URL,"[0-9]+$")
  
  colnames(urlTbl) <- c("URL","LID")
  
  urlTbl$SEAS <- str_extract(urlTbl$URL,"[0-9]{4}")
  
  lgTbl <- rbind(lgTbl,urlTbl)
  
  #the bracket below ends the loop
  
}

#MFL urls work by taking the public url and then using a redirect to get to the actual league pages. it's a pain the ass. MFL also asks that you record the actual location of the pages (the redirect) and use that location. so we need to go through an intermediate step that wouldn't be necessary in most scraping exercises. we need to use RCurl to get HTTP header information. Before we get that header information though, let's actually trim our lgTbl a little. You now know how you would go about getting old league urls from past years, but in this exercise let's just use 100 or so of them from 2016 only.

lgTbl <- lgTbl[lgTbl$SEAS==2016,]

lgTbl <- lgTbl[1:100,]

#in this loop I'm going to loop from 1 to however many rows are in the lgTbl data frame. i do that by using a function on the right hand side of the :

for(i in 1:nrow(lgTbl)){
  
  #set the url to be lgTbl$URL at the row number equal to the iterating variable i
  
  url <- lgTbl$URL[i]
  
  #do some RCurl magic that I barely understand, but which basically gets the HTTP header information for the url in our lgTbl data frame and figures out the redirect that MFL actually wants you to visit for that league. I rarely use RCurl because there are other libraries built on top of it that do things like access apis. So I really don't know much beyond the fact that this code works.
  
  h <- basicHeaderGatherer()
  doc <- getURI(url, headerfunction = h$update)
  loc <- h$value()["Location"]
  
  #append the location as a new column in our data frame. note that i'm using i again to tell R which row to set the value of the location
  
  lgTbl$LOC[i] <- loc
  
}

#if you look at lgTbl now you'll see the urls that MFL shows in their search results page - those are in the URL column - and you'll see the redirect locations - those are in the LOC column

#the important information in MFL's redirect is the number that precedes www - so it will look something like 55www - we need to pull that out and put it in its own column for ease of reference because our future API calls will use it. we'll use a str_extract to tell R that we're looking for the first occurrence of two numeric digits.

lgTbl$DOM <- str_extract(lgTbl$LOC,"[0-9]{2}+")

#then we'll get rid of the table rows that don't have a valid DOM value.

lgTbl <- lgTbl[!is.na(lgTbl$DOM),]

#now we can pull in each league's starting position rules. MFL has a method in their API which lets you get league rules as a nested list. first we'll call out an empty data frame that we'll use for rbind-ing later.

ruleTbl <- data.frame()

#now we start looping

for(i in 1:nrow(lgTbl)){
  
  #using our iterating variable we can piece together the url for the league. in this case we're specifying that we want the data format to come back as JSON. if you're not familiar with JSON it's basically a format that allows for nested lists and it powers lots of pages on the web. in fact often you can find a page's hidden JSON feed and get the data powering that page. this is possible on nba.com for instance where you can even get shot charts from the JSON feed
  
  url <- paste("http://www",lgTbl$DOM[i],".myfantasyleague.com/",lgTbl$SEAS[i],"/export?TYPE=league&L=",lgTbl$LID[i],"&W=&JSON=1",sep="")
  
  #here we're using the jsonlite function of fromJSON - we wrap it in a try function which i do quite a bit when running scraping loops. i do that because a scraping loop can often hit error snags along the way (404 errors for instance). when an error happens inside a loop it stops the loop.  i don't want to have to restart things each time it hits an error so i wrap it in try. if an error does occur then instead of storing k as a list it will be of class try-error and i can tell the loop to not run certain lines of code when that happens.
  
  k <- try(fromJSON(readLines(url)[1]),silent=T)
  
  #you can inspect the nested list that comes out of the line above. you'll see that there's a lot more information in that list than just rosters. next we'll test to see if the try function resulted in an error. if it doesn't result in an error then we proceed to the next few lines. otherwise it's basically going to go back to the top of the loop and try a new url.
  
  if(class(k)!="try-error"){
    
    # the fromJSON function is really great in that it generates nested lists which in some cases contain entire data frames. we can access one of those data frames by just moving through each nested list. we start with the list k and then using $ we select the league list below that and so on. in rf we'll store this data frame we're pulling out of the nested list. that should contain the starter positions for the league
    
    rf <- k$league$starters$position
    
    #later we'll want to join this data to other information we'll collect about the league so we need to add league id and season information to this data frame. we'll get the right league and season info by referencing the row of lgTbl we're currently hitting with our loop.
    
    rf$SEAS <- lgTbl$SEAS[i]
    
    rf$LID <- lgTbl$LID[i]
    
    #now we just use rbind to store these results on top of each other as we loop through and collect more league data.
    
    ruleTbl <- rbind(ruleTbl,rf)
    
  }
  
}

#now we can do a new loop to collect information about drafts. we'll declare a new data frame to hold the draft information

drafts <- data.frame()

for(i in 1:nrow(lgTbl)){
  
  #this should look similar to the url from the loop we ran to collect rules. but in this case the url is changed slightly to use MFL's draftResults method
  
  url <- paste("http://www",lgTbl$DOM[i],".myfantasyleague.com/",lgTbl$SEAS[i],"/export?TYPE=draftResults&L=",lgTbl$LID[i],"&W=&JSON=1",sep="")
  
  k <- try(fromJSON(readLines(url)[1]),silent=T)
  
  if(class(k)!="try-error"){
    
    #note that we're just traversing down the nested list to pull out that data frame again.
    
    pickTbl <- k$draftResults$draftUnit$draftPick
    
    if(length(pickTbl)>0){
      
    pickTbl$SEAS <- lgTbl$SEAS[i]
    
    pickTbl$LID <- lgTbl$LID[i]
    
    drafts <- rbind(drafts,pickTbl)
    
    }
    
  }
  
}

#you'll notice that when we pulled in drafts in the above code, the players came in as just numeric pids. we can hit MFL's players method of the API in order to get more info about those player ids.

url <- "http://football.myfantasyleague.com/2016/export?TYPE=players&DETAILS=1&JSON=1"
  
k <- try(fromJSON(readLines(url)[1]),silent=T)

plyrTbl <- k$players$player

#now you have a bunch of data from MFL leagues including rules and draft picks. You could do a bunch of different things with this data like plot distribution of number of starting WRs.

#we'll do a little bit of work with this data just to illustrate the potential. let's start with a list of 2 QB leagues.

twoQbRules <- ruleTbl[ruleTbl$name=="QB"&ruleTbl$limit==2,]

#now we can get the drafts for those leagues - note that we only have 2016 data here. if we had more years in the data we would have to also be sure to reference the season in addition to the LID

twoQbDraft <- drafts[drafts$LID %in% twoQbRules$LID,]

#some of these draft picks haven't been made yet. we can throw out the picks that haven't been made because they have a blank timestamp

twoQbDraft <- twoQbDraft[twoQbDraft$timestamp!="",]

#now we could merge the draft table with the player table.

twoQbDraft <- merge(twoQbDraft,plyrTbl,by.x="player",by.y="id")

#to make some sense of the data we can order the data frame first by timestamp, then by league id. then you should be able to scroll the data frame to see an individual league.

twoQbDraft <- twoQbDraft[order(twoQbDraft$LID,twoQbDraft$timestamp),]
