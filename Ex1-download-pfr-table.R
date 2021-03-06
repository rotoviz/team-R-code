#We'll use two simple packages for this short exercise. You can install them by running the two lines of code below. After you've installed them you don't need to install them again, you can simply load them later. A package/library is just a set of functions that extend R's base code.

install.packages("XML")
install.packages("ggplot2")

#Start by loading the necessary R packages/libraries. You'll need to load the requisite packages each time you want to use the functions contained in a package. 

library("XML")
library("ggplot2")

#Next run this readHTMLTable function which will take a URL along with a few other parameters, go to the web page, and download the table as specified by the 'which' parameter. If you changed 'which' to 3 it would grab a different table in the page. In this case I'm passing it a URL which grabs players from 2015 that saw at least 50 targets and fewer than 20 rushing attempts. I like to use usage proxies instead of specifying position because PFR sometimes miscodes positions. Run the line of code and then it will create an object that you'll be able to click and inspect in the Environment on the right. It should say something like 108 obs of 22 variables when you're done.


pfrTbl <- readHTMLTable("http://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=2015&year_max=2015&season_start=1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&is_active=&is_hof=&pos_is_qb=Y&pos_is_rb=Y&pos_is_wr=Y&pos_is_te=Y&pos_is_e=Y&pos_is_t=Y&pos_is_g=Y&pos_is_c=Y&pos_is_ol=Y&pos_is_dt=Y&pos_is_de=Y&pos_is_dl=Y&pos_is_ilb=Y&pos_is_olb=Y&pos_is_lb=Y&pos_is_cb=Y&pos_is_s=Y&pos_is_db=Y&pos_is_k=Y&pos_is_p=Y&c1stat=targets&c1comp=gt&c1val=50&c2stat=rush_att&c2comp=lt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5comp=&c5gtlt=lt&c6mult=1.0&c6comp=&order_by=targets&draft=0&draft_year_min=1936&draft_year_max=2015&type=&draft_round_min=0&draft_round_max=99&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=0&draft_league_id=&draft_team_id=&college_id=all&conference=any&draft_pos_is_qb=Y&draft_pos_is_rb=Y&draft_pos_is_wr=Y&draft_pos_is_te=Y&draft_pos_is_e=Y&draft_pos_is_t=Y&draft_pos_is_g=Y&draft_pos_is_c=Y&draft_pos_is_ol=Y&draft_pos_is_dt=Y&draft_pos_is_de=Y&draft_pos_is_dl=Y&draft_pos_is_ilb=Y&draft_pos_is_olb=Y&draft_pos_is_lb=Y&draft_pos_is_cb=Y&draft_pos_is_s=Y&draft_pos_is_db=Y&draft_pos_is_k=Y&draft_pos_is_p=Y",which=4,stringsAsFactors=FALSE)

#Now that we've pulled the table in we need to replace some of the column names. Note that the 2nd column is blank so let's give it a name. We use the function colnames() and then specify a column index number - 2, or the 2nd column - and then tell it what name to give that column.

colnames(pfrTbl)[2] <- "Player"

#There are still a few columns that are problematic. For instance, rushing yards and receiving yards aren't differentiated. So we need to fix that. We could keep running the function above over individual columns, but there's an easier way to do that. Instead of specifying a single column to run the function on, we can pass a list or vector of columns to run the function on. Placing items inside c() creates a vector or a list.

colnames(pfrTbl)[c(11,13)] <- c("RuYds","RuTD")

#Next we need to fix some of the data types in the data. When it comes into R from PFR, R doesn't know whether you want to it be a string, or a factor, or a numeric data type. So we have to tell it to cast the data as a type numeric. In the line below we're telling it to run the function as.numeric() on the Year columnd of the pfrTbl and then to take the results of that function and replace the pfrTbl$Year column

pfrTbl$Year <- as.numeric(pfrTbl$Year)

#Again, there are other columns where fixing the data type is necessary. But instead of running that line of code on over 10 columns, we can again pass R a list of columns that we want to alter. In this case we're using the sapply function to tell R to run the function as.numeric on columns 4, and then 8 through 22 of the pfrTbl. sapply is a poweful function that I use a lot when cleaning up wide tables. After you run this function you'll notice that some NA values are created when R tries to convert a character into a number but the character isn't actually a number.

pfrTbl[c(4,8:22)] <- sapply(pfrTbl[c(4,8:22)],as.numeric)

#When the table came over from PFR it had some column headers midway through the table. Now that the values in those columns have been converted to numbers and have thrown NA values, it makes for an easy way to remove those bad rows. The call below is an example of using the idex properties of an R data.frame. Every row and column in an R data.frame can be referenced in the following way - dataFrame[row,column] - so the call below is specifying the pfrTbl rows where the Year column is not (!) NA. ! generally means IS NOT.

pfrTbl <- pfrTbl[!is.na(pfrTbl$Year),]

#Now that we have the data in numeric format and we've removed some bad rows we can run some calculations on the data. This line of code calculates PPR fantasy points just by referring to the data.frame name, and the column in the data.frame

pfrTbl$FP <- pfrTbl$Rec + pfrTbl$Yds/10 + pfrTbl$TD*6

#Now that we have calculated fantasy points let's plot targets vs. fantasy points. We'll use the ggplot library to do this. This line of code says create a ggplot object where the data comes from pfrTbl and then the aesthetics, or aes(), will be Tgt on the x axis and FP on the y axis. Then we tell it to make a plot layer with that data using the geom_point() function. 

ggplot(data=pfrTbl,aes(Tgt,FP)) + geom_point()

#We can add a smoothed trendline to the plot by just adding the function geom_smooth() to that line of code

ggplot(pfrTbl,aes(Tgt,FP)) + geom_point() + geom_smooth()

#ggplot defaults to use loess smoothing but you can specify other methods for your trendline. Here's a trendline using a linear model.

ggplot(pfrTbl,aes(Tgt,FP)) + geom_point() + geom_smooth(method="lm")

#we can also create a separate linear model object by using the function lm() and then passing it a formula to use for the model. In this case we'll use FP as the dependent variable and Tgt as the independent variable. Formulas look like this: Dependent Variable ~ Independent Variable 1 + Independent Variable 2 ........ since you can do multiple regression in this same function you can specify more than one independent variable if you want.

mdl <- lm(FP ~ Tgt,pfrTbl)

#The above line of code creates the object for the linear model. But if we want to inspect the model the easiest way to do it is to just run the summary function on the model. That will spit out the model results into the Console below.

summary(mdl)

#When the model object is created it has a number of features that you can access by attaching the $ operator to the reference to the object. Below I'm saying that I want to take the r.squared of the model summary, and then assign it to a new value that I'm creating called rsq

rsq <- summary(mdl)$r.squared

#the rsq value would be easier to read if we round it to the hundredths place. We'll just use the round function for that. If you use Excel the round function will probably make sense.

rsq <- round(rsq,2)

#I want to print the rsq on the actual plot and I want it to be clear what the number means. So I'm going to convert it to a character and paste 'r^2' in front of the actual value. paste() is a pretty useful function for working with text because you can use it to do things like append a player's team to the player's name.

rsq <- paste("r^2 =",rsq)

#Now let's run our ggplot line again, but now we'll tell it to create a text layer that's labeled with the rsq value we've created.

ggplot(pfrTbl,aes(Tgt,FP)) + geom_point() + geom_smooth(method="lm") + geom_text(x=100,y=350,label=rsq)
