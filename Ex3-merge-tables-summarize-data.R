# Load the packages we'll need for this exercise. if you don't have them
# installed you can always install them in RStudio using the packages tab in the
# lower right.

library(dplyr)
library(ggplot2)

# First go to the Github repo - https://github.com/rotoviz/team-R-code - and
# click "Download ZIP" which will give you all of the files from that repo in a
# zip file. Then you'll need to unzip the file and drag the data subfolder of
# that zip into your R working directory. You can find out what your R working
# directory is with this line of code. Your working dir should print in the
# console below

getwd()

# So drag the data folder of the zip file into the working directory and then
# we'll be able to refer to that folder and its files using the ~ relative path
# convention. Here we'll import the csv files in the folder and make each of
# them a separate table. These files are from the Armchair dataset. Feel free to
# inspect the tables once you've imported them. There's also a PDF in the folder
# that has the table metadata.

# The core PLAY table is 650k records so I had to break it up to get it to
# upload to Github. We can put it back together using rbind. rbind will just
# combine tables so that their rows are stacked on top of each other.

playTbl <- rbind(read.csv("~/data/PLAY_1.csv",stringsAsFactors = F),
                 read.csv("~/data/PLAY_2.csv",stringsAsFactors = F),
                 read.csv("~/data/PLAY_3.csv",stringsAsFactors = F))
                 
plyrTbl <- read.csv("~/data/PLAYER.csv",stringsAsFactors = F)
passTbl <- read.csv("~/data/PASS.csv",stringsAsFactors = F)
gameTbl <- read.csv("~/data/GAME.csv",stringsAsFactors = F)

# The play table contains information available for individual plays including
# time left on the clock, how many points the offense has, the down, the
# distance, etc. We can join that table to the pass table using the merge
# function. The pass table contains pass play specific information like who the
# target was, how many yards were gained, and who the passer was. I have two
# lines of code here to illustrate two ways to make this join. In the first call
# I'm going to keep all of the rows from playTbl (the x table) even if there is
# no match in the pass table. in the 2nd call I'm just keeping the rows from
# playTbl if there is a match in passTbl. The first call would be useful if you
# also intended on adding in rushing plays for instance. Be sure to inspect the
# resulting tables to see how they differ. the by.x argument tells it which
# field to join on. i'm saying that both tables contain a pid field and that's
# how R will know which rows should be joined.

playTblAll <- merge(playTbl,passTbl,by.x="pid",by.y="pid",all.x=T)

playTbl <- merge(playTbl,passTbl,by.x="pid",by.y="pid")

# Now let's throw out some columns we're not going to use from the game table.
# we'll keep just the columns named by the list in the brackets below. ou is
# over/under. gid is a unique game id, wk is the week of the season.

gameTbl <- gameTbl[,c("gid","wk","seas","ou","day")]

# Now we're also going to merge the play table with the game table. note that
# we're not passing it explicit parameters (the by.x argument) for the column
# that we want to join the table by. that's because R will attempt to do it
# automatically if no key is given. it's probably better when you're starting to
# do merges to pass it explicit parameters like we did above using by.x but this
# method also works.

playTbl <- merge(playTbl,gameTbl)

# Now we're going to just keep rows from the table where wk is less than 18.
# we're throwing out the playoffs when we do this.

playTbl <- playTbl[playTbl$wk<18,]

# Where's a pts column in the table but we need something a little more exact
# because we're going to calculate fantasy points. the way the table is
# currently set up some pass plays yield negative points for a pick six. so the
# first thing we'll do is just create a column where td is equal to zero on
# every row.

playTbl$td <- 0

# Now we'll tell R that where pts is greater than 5 we want to set td equal to 1

playTbl$td[playTbl$pts>5] <- 1

# Now we can calculate fantasy points using the PPR formula.

playTbl$fpts <- playTbl$yds/10 + playTbl$comp + playTbl$td*6

# Doing fast calculations of group summaries is probably one of the more useful
# things you can do with R. it's like doing a pivot table, except it's just a
# line of code where you can be really specific as to what you want, and then it
# can be run quickly whereas pivot tables are a pain to setup and format. they
# also have limited calculating abilities. in this case we'll use the piping (%>%) 
# ability of the dplyr package to first tell R to start with the play
# table. then group the table by gid and off. then we pass it through another
# pipe at which point we tell R to summarize the data in a new table with a
# column for tot.fpts as the sum of all fpts. this will basically create a table
# where each offense's passing fantasy points are summed for each game. this is
# actually a good illustration of where R really outpaces Excel. You'll have
# some lag in your system when working with 600k row tables in R. But those
# tables would probably crash your system in Excel once you add in any kind of
# complex calculation like a SUMIFS

sumTbl <- playTbl %>% group_by(gid,off) %>% summarize(tot.fpts = sum(fpts))

# Now we can merge that newly created table (sumTbl) with the game table which
# will give those summed up fantasy points some context because we'll also have
# over/under and day of the week for them.

sumTbl <- merge(sumTbl,gameTbl)

# Then you might want to visualize total passing fantasy points vs. the game
# over/under. here's a ggplot call which does just that. note one way to deal
# with overlapping points on a graph is to specify a semi-transparent value for
# the points. i do that with the alpha argument in the geom_point() function.

ggplot(sumTbl,aes(ou,tot.fpts)) + geom_point(alpha=.25) + geom_smooth() + xlab("Over/Under") + ylab("Total Receiving Fantasy Points")

# Then you might be interested to know what the correlation is between
# over/under and passing fantasy points. this line will give you that number in
# the console below.

cor(sumTbl$ou,sumTbl$tot.fpts)

# Maybe now you want to know how many fantasy points are scored on average per
# team, based on the day of the week - do Thursdays really yield fewer fantasy
# points?. this line uses dplyr again and uses the same number of pipes we used
# above. the difference is that in this case we're grouping the data by day. to
# show you how customizable the calculations are I've also added a max
# calculation. almost any number you want to calculate could be added to this.
# You may note after you've run this code that the data contains a few typos in
# the day field. one thing you always have to be doing with any analysis is
# cleaning bad data.

dayTbl <- sumTbl %>% group_by(day) %>% summarize(avg.fpts = mean(tot.fpts),max.fpts = max(tot.fpts))

# Sometimes when you are summarizing numbers you don't want to create a new
# summary table, you want to append a new column to the data frame you already
# have. you can do that in dplyr using pipes and just changing the summarize
# function to a mutate function. this calculates the average fantasy points
# scored for each down and distance on the field, and then appends that as a new
# column. we'll call it expected points. yfog is yards from own goal. look at
# the playTbl when you're done and check out the new column it created with the
# mutate function

playTbl <- playTbl %>% group_by(dwn,ytg,yfog) %>% mutate(exp.fpts = mean(fpts))

# Maybe now we want to figure out how many fantasy points a play generated
# compared to the average for the down/distance. we can take the actual fantasy
# points, subtract out the expected, and then what we have left over is fantasy
# points over expectation. this is sort of similar to the numbers in the fantasy
# efficiency app although that app is position specific and here we've just
# lumped every position together.

playTbl$fpoe <- playTbl$fpts - playTbl$exp.fpts

# Now we can calculate some offensive and defensive strength numbers by doing a
# summary table where we group by def and calculate the average fpoe that they
# gave up per pass play. i've also added a round function in order to make the
# numbers easier on the eyes. EDIT - once I added in the past seasons of
# Armchair data I also added seas to the group_by so that you get each defenses
# numbers per season rather than across all 16 years.

defTbl <- playTbl %>% group_by(def,seas) %>% summarize(avg.fpoe = round(mean(fpoe),2))

# We can do the same for offenses.

offTbl <- playTbl %>% group_by(off,seas) %>% summarize(avg.fpoe = round(mean(fpoe),2))

# Now maybe we want to summarize average fpoe by receiver instead of by team. in
# this call we don't need to also group by off but i like to do that because it
# adds some context when you're looking at the data. note that i've also asked R
# to calculate the length of the yds field. that's a way to get a simple count,
# or in this case total targets. also I added seas to group_by because we now
# have the historical data.

recTbl <- playTbl %>% group_by(trg,off,seas) %>% summarize(avg.fpoe = round(mean(fpoe),2),targets = length(yds))

# All of the receivers are coded by their unique id in the plyrTbl data frame.
# so let's now join that plyrTbl with the recTbl we just created. in this case
# we'll have to merge two data frames where the field that we're going to merge
# on doesn't have the same name in each table. we do this by specifying the by.x
# and by.y parameters. in the recTbl the field is called trg and in the plyrTbl
# it's called player. check out the fields in those tables before you run the
# code so you can see that they contain the same kind of info

recTbl <- merge(recTbl,plyrTbl,by.x="trg",by.y="player")

# If we wanted a data frame that's easier on the eyes we could drop some of
# noise from the table by subsetting it.

recTbl <- recTbl[,c("pname","off","seas","pos1","height","targets","avg.fpoe")]

# In this exercise i've tried to illustrate some things that would be fairly
# time consuming in Excel, like joining of 45,000 row tables, then calculating
# summary numbers. To replicate the mutate function above where we averaged the
# fantasy points by down and distance it would require an AVERAGEIFS function in
# Excel and then pasting that formula down 18,000 rows. now that you have some
# of the Armchair data for 2015


