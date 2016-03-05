#load the packages we'll need for this exercise. if you don't have them installed you can always install them in RStudio using the packages tab in the lower right.

library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)

#First go to the Github repo - https://github.com/rotoviz/team-R-code - and click "Download ZIP" which will give you all of the files from that repo in a zip file. Then you'll need to unzip the file and drag the data subfolder of that zip into your R working directory. You can find out what your R working directory is with this line of code. Your working dir should print in the console below

getwd()

#drag the data folder of the zip file into the working directory and then we'll be able to refer to that folder and its files using the ~ relative path convention. Here we'll import the csv files in the folder and make each of them a separate table. These files are from the Armchair dataset. Feel free to inspect the tables once you've imported them. There's also a PDF in the folder that has the table metadata.

#The core PLAY table is 650k records so I had to break it up to get it to upload to Github. We can put it back together using rbind. rbind will just combine tables so that their rows are stacked on top of each other.

playTbl <- rbind(read.csv("~/data/PLAY_1.csv",stringsAsFactors = F),
                 read.csv("~/data/PLAY_2.csv",stringsAsFactors = F),
                 read.csv("~/data/PLAY_3.csv",stringsAsFactors = F))
                 
plyrTbl <- read.csv("~/data/PLAYER.csv",stringsAsFactors = F)
passTbl <- read.csv("~/data/PASS.csv",stringsAsFactors = F)
rushTbl <- read.csv("~/data/RUSH.csv",stringsAsFactors = F)
gameTbl <- read.csv("~/data/GAME.csv",stringsAsFactors = F)

#we're going to merge both the rush table and pass table with the play table so we need to make sure there are no duplicate field names. these two lines of code make explicit which yards/success are rush vs. receiving. in the first line we specify the column numbers with a list. in the 2nd line we specify the column numbers with a range.

colnames(passTbl)[c(5,7)] <- c("recyds","recsucc")

colnames(rushTbl)[4:5] <- c("ruyds","rusucc")

#now we'll merge the playTbl with the passTbl. But because we're also going to merge the rushTbl with the pass table we need to make sure that all of the rows of the playTbl remain. Without the all.x=T argument below then R would drop any row from the playTbl that isn't in the pass table and we wouldn't be able to later merge the rushTbl.

playTbl <- merge(playTbl,passTbl,by.x="pid",by.y="pid",all.x=T)

#now let's merge the rushTbl with the playTbl as well.

playTbl <- merge(playTbl,rushTbl,by.x="pid",by.y="pid",all.x=T)

gameTbl <- gameTbl[,c("gid","wk","seas")]

#now we're also going to merge the play table with the game table. note that we're not passing it explicit parameters (the by.x argument) for the column that we want to join the table by. that's because R will attempt to do it automatically if no key is given. it's probably better when you're starting to do merges to pass it explicit parameters like we did above using by.x but this method also works.

playTbl <- merge(playTbl,gameTbl)

#to make our table slightly smaller we can now throw out rows that aren't rush or pass. Here I'm telling it to only keep rows where the type column has a value that's also in the list that I specify.

playTbl <- playTbl[playTbl$type %in% c("RUSH","PASS"),]

#now we subset the data frame by redefining it as only the rows where wk is less than 18. that gets rid of the playoffs

playTbl <- playTbl[playTbl$wk<18,]

#we need a new column to hold data about tds. we first start by making every value in the column equal to 0

playTbl$td <- 0

#now we set the td column equal to 1 only in rows where pts is greater than 5

playTbl$td[playTbl$pts>5] <- 1

#when we merged the rushing and passing tables it created NA values in the recyds and ruyds for some rows. in order to do some calculations like fantasy points it's easier if those values are zero instead of NA. so here we set ruyds equal to zero in rows where ruyds is na

playTbl$ruyds[is.na(playTbl$ruyds)] <- 0

#now we do the same for recyds

playTbl$recyds[is.na(playTbl$recyds)] <- 0

#and then we do the same for comp

playTbl$comp[is.na(playTbl$comp)] <- 0

#now we can calculate ppr points

playTbl$fpts <- playTbl$ruyds/10 + playTbl$td*6 + playTbl$comp + playTbl$recyds/10

#i want to consolidate the information in the trg (target) and bc (ball carrier) columns in order to be able to work on the table in a way where running backs for instance can be summed up to include both their rushing and receiving production. so i'll create a new column called player and then in rows where bc is not NA i'll set player equal to bc. in rows where trg is not NA then i'll set player equal to trg

playTbl$player[!is.na(playTbl$bc)] <- playTbl$bc[!is.na(playTbl$bc)]

playTbl$player[!is.na(playTbl$trg)] <- playTbl$trg[!is.na(playTbl$trg)]

#now you should have a player column that is essentially just bc and trg consolidated. now we can use the match function to also associate the player's position. we tell R that the pos column will be the pos1 column from the plyrTbl data frame  - where the player fields from each data frame match. in instances where i'm just pulling in a single column from another data frame then i'll use this match method. in instances where more than 1 column is being related then i'll use a merge function

playTbl$pos <- plyrTbl$pos1[match(playTbl$player,plyrTbl$player)]

#now i'd like to throw out playTbl rows that aren't for RB, WR, or TE as the player (remember that player is either ball carrier or target). i do that by using %in% and a list

playTbl <- playTbl[playTbl$pos %in% c("RB","WR","TE"),]

#we're going to create an efficiency metric, but before we do that i want to illustrate how much work can go into something like that. first let's summarize average fantasy points based on position, play type, and yards from own goal.

sumTbl <- playTbl %>% group_by(pos,type,yfog) %>% summarize(exp.fpts = mean(fpts))

#now let's graph the values that we just summarized. you'll notice that the values are pretty unstable from yard line to yard line when the sample sizes are small - like TE rushes for instance. well that same problem is going to become even more pronounced when we also group by down and distance (because we're making each sample smaller). there won't be many 4th and 13 plays to create the expected value. all of this is only to say that while the exercise we do today will get us close, there are probably more steps we could go through like smoothing values to make them more logical.

ggplot(sumTbl,aes(yfog,exp.fpts,color=type)) + geom_line() + facet_wrap(~pos)

#now let's create a new column in our playTbl using dplyr pipes and the mutate function. remember that mutate creates a new column in a table, and summarize creates a new table. the line below creates an expected fantasy points number that is unique for each position, play type, down, distance, etc.

playTbl <- playTbl %>% group_by(pos,type,ytg,dwn,yfog) %>% mutate(exp.fpts = mean(fpts))

#now to get efficiency we can subtract the expected points from the actual points

playTbl$fpoe <- playTbl$fpts - playTbl$exp.fpts

#now that we have fpoe for every single play we can summarize fpoe by player, and then also break it out by whether it's receiving or rushing. in the line below i'm doing a sum function on fpoe where type equals rush, and then separately for pass plays. also i'm calculating rushes and targets using a length function. length is like a simple count.

sumTbl <- playTbl %>% group_by(player,off,pos,seas) %>% summarize(rush.fpoe = sum(fpoe[type=="RUSH"]),atts = length(fpoe[type=="RUSH"]),rec.fpoe = sum(fpoe[type=="PASS"]),trgs = length(fpoe[type=="PASS"]))

#it's kind of hard to read that table since the player ids are all still coded. we'll match those up with some names from the plyrTbl, but first let's create a full name in that table by pasting the fname and lname columns together.

plyrTbl$name <- paste(plyrTbl$fname,plyrTbl$lname)

#now we'll use a match function to add the name to the summary table we just created

sumTbl$name <- plyrTbl$name[match(sumTbl$player,plyrTbl$player)]

#Rstudio makes it easy to sort data by clicking, but if you wanted to re-order your data frame you could use the order function. in this case i'm saying first order by rec.fpoe descending, then order by season descending

sumTbl <- sumTbl[order(-sumTbl$seas,-sumTbl$rec.fpoe),]

#if you're writing an article and want to pull that data into excel to clean it up then you could use the write.table function to generate a csv. this will place the csv in your working directory. i'm passing it the table name i want to export, along with a file name, and then i'm explicitly saying that the separator for the csv will be a comma. the last argument just ensures that you don't have a row names column.

write.table(sumTbl,file="~/fpoe-summary.csv",sep=",",row.names=F)

#now let's use some of what we're learned to generate a summary table for RB stats. in this call we're using dplyr pipes again. we're starting with the playTbl but I'm telling R that i only want the rows of the playTbl where pos is equal to RB. then i'm grouping by player and seas. after that i have a summarize function that generates all of the summary stats per season.

sumTbl <- playTbl[playTbl$pos=="RB",] %>% group_by(player,seas) %>% summarize(gms = length(unique(gid)),atts = length(ruyds[type=="RUSH"]),ruyds = sum(ruyds),rutds = sum(td[type=="RUSH"]),trgs = length(recyds[type=="PASS"]),recs = sum(comp),recyds = sum(recyds),rectds = sum(td[type=="PASS"]),fpts = round(sum(fpts),2),fpoe = sum(fpoe))

#we're going to do a simple projection for the 2015 RBs so first we'll create a separate table that only contains those players. we do that by subsetting the sumTbl with a row index such that it only keeps rows where seas == 2015

cyTbl <- sumTbl[sumTbl$seas==2015,]

#now we want to relate each player season to the player's season that came next. So to create our model we want to be able to have Larry Johnson's 2006 and 2007 season on the same row. we'll do that by first creating a new table that we'll call n1Tbl and it will start out the same as sumTbl

n1Tbl <- sumTbl

#Then because we're eventually going to merge the n1Tbl back with the sumTbl we want to advance the seas in the n1Tbl by 1. this will make it so that Larry Johnson's 2006 and 2007 seasons can be easily merged to each other

n1Tbl$seas <- n1Tbl$seas+1

#when we try to merge those tables back together we're going to have a lot of duplicate column names. to keep things straight i always like to rename the columns in the n1Tbl so that i know those numbers are for the n+1 season. to do that i use the colnames function to set the column names. but on the right hand side of the call i'm just re-using the existing column names and then pasting .n1 onto the end. also, because i don't want to alter either the player field name or the seas field name, i'm starting with column number 3 and altering every column between 3 and the number of columns in the data frame. using a relative reference to a data frame's columns will be really useful for your code when you decide that you want to add a column. then you won't have to keep changing a range that looks like 3:12 to 3:13 when you add a new column. because the ending value in your range is referenced based on ncol() the end of your range will always be the last column in the table

colnames(n1Tbl)[3:ncol(n1Tbl)] <- paste(colnames(n1Tbl)[3:ncol(n1Tbl)],".n1",sep="")

#now we have a n1Tbl that can be easily merged back with the sumTbl

sumTbl <- merge(sumTbl,n1Tbl)

#maybe we now want to create a linear model so that we can see the relationship that various variables have with n+1 fantasy points. here's a line that creates an object which is a linear model. and then it defines a formula for that model where fpts.n1 should be explained by atts and targs. remember that the variables that don't have n1 on the end are the first year numbers.

mdl <- lm(fpts.n1 ~ atts + trgs,sumTbl)

#now we can summarize this model. you could experiment with adding or removing variables in order to see what impact that has on the model's metrics like adjusted r squared

summary(mdl)

#once you have a model object in R it's easy to apply that model to data. in this case we can apply the model we just made to the 2015 data that we reserved in the cyTbl. the function for applying a model is predict(model object,data to apply the model to)

cyTbl$pred.fpts <- predict(mdl,cyTbl)

#that table would benefit from having a name column. this match method is what we've been using when we don't need to merge a full data frame

cyTbl$name <- plyrTbl$name[match(cyTbl$player,plyrTbl$player)]

#when you're making models it's sometimes helpful to do some visualization to see how variables are correlated to each other. one way you can do that is to create a correlation matrix. we'll go through the steps to creating a correlation matrix in R. the first thing we want is to get a list of which columns in the sumTbl are numeric. we won't be able to use a correlation function on a non-numeric column anyway. this uses sapply to apply the function is.numeric to each column

nums <- sapply(sumTbl, is.numeric)

#now look at the values that were generated by that line by running this line and inspecting the console below

nums

#now we can actually subset sumTbl and keep only the columns where is.numeric was true. you should be starting to notice that when we refer to a data frame by the index [,] convention, when we're making references to rows the logic goes on the left of the comma and when we refer to columns the logic goes on the right of the comma

corMat <- sumTbl[ ,nums]

#now that we have just the numeric columns we can run the cor function on them

corMat <- cor(corMat)

#the data is fine in the format it's in, but to plot it in ggplot we want a long table instead of a wide table. easily transforming data between wide and long format is something that R is much better at than Excel. run this line and then look at how the data has changed. we won't get too detailed on what melt is in this lesson because transforming data is probably its own lesson

corMat <- melt(corMat)

#now let's round the values in the value columns

corMat$value <- round(corMat$value,2)

#now you can plot the correlation matrix in ggplot. we're saying that the data will be in corMat, Var1 should be on the x axis, Var2 should be on the y axis, and then if we need to label any values the label will be in the value column. then geom_tile defines the kind of plot as we set the aesthetic value of the fill of the tiles equal to the value column.

ggplot(corMat,aes(Var1,Var2,label=value)) + geom_tile(aes(fill=value)) + geom_text() + theme_bw()

#in this exercise we started to transition from simply arranging data in R to arranging it in a way that can be useful for doing football analysis. based on the data you've generated you could probably start to create position specific models for fantasy scoring just by slightly modifying some of the lines above. you could change this text on line 129 - playTbl[playTbl$pos=="RB",] - by deleting RB and replacing it with WR. then you could change the formula in lm() to experiment with other predictors of fpts.n1

#things we've done that would be difficult in Excel - instantly summarize 400k row tables, reshaping data between wide and long format, applying a linear model using just a single line of code rather than having to create a formula in Excel that contains all of your model's coefficients (which would need to be changed any time your model changed)

