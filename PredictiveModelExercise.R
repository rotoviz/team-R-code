#### PURPOSE ###################################################################
# Demo a few predictive modeling techniques using data from the 'GAME.csv' file
#  from ArmchairAnalysis.com available github.com/rotoviz/team-R-code/data. The
#  end result from this exercise will be creation of models that can predict the
#  outcome of NFL games...hopefully! 
#
# Author: Jim Kloet (@jimkloet , jim.kloet@gmail.com)
#### LIBRARIES #################################################################
library(caret)
library(earth)
library(glmnet)
library(mice)
library(randomForest)
library(sqldf)

#### READ IN DATA ##############################################################
# make sure your working directory is set to location of GAME.csv
# setwd("~/Desktop/team-R-code-master/data")

# read in the csv
gameRaw <- read.csv('GAME.csv' ,
                    header = TRUE ,
                    na.strings = c(" ", "")) #specify strings we know are NAs

#### CLEANSE/CODE DATA #########################################################
# inspect your data so you know what you're working with here
summary(gameRaw)
# Looks like a mixed dataset; lots of factors with multiple levels, factors that 
#  should be numerics, NAs, etc.

# To make interpretable models and satisfy assumptions, we will have to make 
#  some decisions about which data to use

# Look only at regular season
gameClean <- gameRaw[-which(gameRaw$wk > 17), ]

# Interrogate patterns of NAs in weather-related vars; possibly dome games? 
#  The cond var has info on this
#generates table of response counts for cond when temp is NA
table(gameClean$cond[is.na(gameClean$temp)]) 
#same for humd-cond is NA
table(gameClean$cond[is.na(gameClean$humd)]) 
table(gameClean$cond[is.na(gameClean$wspd)])
table(gameClean$cond[is.na(gameClean$wdir)])

# cond column indicates "Closed Roof", "Covered Roof", "Dome" which covers a lot 
#  of NAs in weather vars
#  DECISION POINT: what to do with NAs? Can exclude (conservative strategy) or 
#  replace (often reasonable). We'll replace

# replace missing temps in domes with a random integer between 70 and 75 which 
#  was (google's answer for average dome temp)
gameClean$temp[is.na(gameClean$temp) & 
               gameClean$cond %in% c("Closed Roof" , 
                                     "Covered Roof" , 
                                     "Dome")] <- sample(70:75 , 
                                                        size = 1)

# replace missing humd in domes with average
gameClean$humd[is.na(gameClean$humd) & 
              gameClean$cond %in% c("Closed Roof" ,
                                    "Covered Roof",
                                    "Dome")] <- mean(gameClean$humd , 
                                                     na.rm = TRUE)

# wspd shouldn't be factor, but it's treated one due to "SSE" value; fix that!
gameClean$wspd <- as.numeric(as.character(gameClean$wspd))
# NAs introduced by coercion, this is ok

# replace wspd in domes with 0
gameClean$wspd[is.na(gameClean$wspd) & 
              gameClean$cond %in% c("Closed Roof" ,
                                    "Covered Roof" ,
                                    "Dome")] <- 0

# wdir isn't going to be useful for these models, so discard it
gameClean$wdir <- NULL

# inspect data again
summary(gameClean)
# we still have NAs remaining, but they don't appear to follow a pattern. we'll
#  deal with them in a bit

### Recode categorical variables; this isn't always necessary, but is good 
#  practice when you have:
#    factors with too many levels for your model to run (usually > 50);
#    factors with levels that don't make a lot of sense to include;
#    factors with a lot of similar levels that should be collapsed

# start with day; only care if sunday vs. non
gameClean$Sunday <- ifelse(grepl('SUN|SN' , gameClean$day) , 1 , 0)
gameClean$day <- NULL

# we're not going to mess with specific stadiums; dome/covered is enough
gameClean$stadDome <- ifelse(grepl('[Dd]ome' , gameClean$stad) , 1 , 0)
gameClean$stad <- NULL

# we'll pull out dome/covered games from the cond variable as well
gameClean$condRoofDome <- ifelse(grepl('Roof|Dome' , gameClean$cond) , 1 , 0)
gameClean$Indoors <- ifelse(gameClean$stadDome == 1 , 1 , 
                            ifelse(gameClean$condRoofDome == 1 , 1 , 0))
gameClean$stadDome <- NULL
gameClean$condRoofDome <- NULL

# code precipitation
# NOTE: if we want to use this weather as a predictor, we have to assume that 
#  this info is available pre-game (probably true in most cases)
gameClean$Precipitation <- ifelse(grepl('Rain|Shower|Thunder|Snow|Flurries' , 
                                        gameClean$cond) , 1 , 0)

# code especially good conditions
gameClean$GoodConditions <- 
  ifelse(gameClean$Indoors == 1 , 1 , 
         ifelse(grepl('Sun|Clear' , gameClean$cond) , 1 , 0))

# code really hot days
gameClean$VeryHot <- 
  ifelse(gameClean$temp > quantile(gameClean$temp , .8 , na.rm = TRUE) , 1 , 0)

# code really cold days
gameClean$VeryCold <- 
  ifelse(gameClean$temp < quantile(gameClean$temp , .2 , na.rm = TRUE) , 1 , 0)

# code really humid days
gameClean$VeryHumid <- 
  ifelse(gameClean$humd > quantile(gameClean$humd, .8 , na.rm = TRUE) , 1 , 0)

# code really windy days
gameClean$VeryWindy <- 
  ifelse(gameClean$wspd > quantile(gameClean$wspd, .8 , na.rm = TRUE) , 1 , 0)


# code generally bad weather
gameClean$BadWeather <- 
  ifelse(gameClean$Precipitation == 1 , 1 ,
  ifelse(gameClean$VeryHot == 1 , 1 ,
  ifelse(gameClean$VeryCold == 1 , 1 ,
  ifelse(gameClean$VeryHumid == 1 , 1 ,
  ifelse(gameClean$VeryWindy == 1 , 1 ,
  ifelse(grepl('Windy|Foggy' , gameClean$cond) , 1 , 0))))))
                               
gameClean$cond <- NULL

# code surfaces; artificial vs. natural is probably fine
gameClean$NaturalSurface <- ifelse(gameClean$surf == "Grass" , 1 , 0)
gameClean$surf <- NULL

### impute remaining missing values
set.seed(31483)
imp <- mice(gameClean , m = 1 , maxit = 1)
gameComplete <- complete(imp)

# tidy up
rm(imp , gameClean , gameRaw)

#### SETUP OUTCOMES ############################################################

# spread is in reference to visitors, so reference visitors first
gameComplete$expectedptsv <- (gameComplete$ou - gameComplete$sprv) / 2
gameComplete$expectedwinv <- ifelse(gameComplete$sprv < 0 , 1 , 0)
gameComplete$winv <- ifelse(gameComplete$ptsv > gameComplete$ptsh , 1 , 0)

# now calculate home equivalents
gameComplete$sprh <- gameComplete$sprv * -1
gameComplete$expectedptsh <- gameComplete$ou - gameComplete$expectedptsv
gameComplete$expectedwinh <- ifelse(gameComplete$sprh < 0 , 1 , 0)
gameComplete$winh <- ifelse(gameComplete$ptsh > gameComplete$ptsv , 1 , 0)

# tie
gameComplete$tie <- ifelse(gameComplete$ptsv == gameComplete$ptsh , 1 , 0)

#### RESTRUCTURE ###############################################################
# we have a slightly wider data frame than necessary for this project; ideally, 
#  we can calculate some season-level stats for teams, then subset to only the
#  underdogs for each regular season game. this will be slightly convoluted, but
#  i'm how to do this manually in base R, rather than using reshape or another
#  package with similar functionality. mostly cuz i'm a masochist, but it's also
#  good to know how to brute force this shit when necessary.

# first setup a visitor data frame, with the specified columns, which we'll
#  relabel in a minute.
df.V <- gameComplete[, names(gameComplete) %in% c('gid' ,
                                                  'seas' ,
                                                  'wk' ,
                                                  'v' ,
                                                  'temp' ,
                                                  'humd' ,
                                                  'wspd' ,
                                                  'ou' ,
                                                  'sprv' ,
                                                  'ptsv' ,
                                                  'Sunday' ,
                                                  'Indoors' ,
                                                  'Precipitation' ,
                                                  'GoodConditions' ,
                                                  'VeryHot' ,
                                                  'VeryCold' ,
                                                  'VeryHumid' ,
                                                  'VeryWindy' ,
                                                  'BadWeather' ,
                                                  'NaturalSurface' ,
                                                  'expectedptsv',
                                                  'expectedwinv' ,
                                                  'winv' ,
                                                  'tie')]
# change column names in df.V to be the following list of column names
names(df.V) <- c('gid' ,
                 'seas' ,
                 'wk' ,
                 'team' ,
                 'temp' ,
                 'humd' ,
                 'wspd' ,
                 'ou' ,
                 'sprfor' ,
                 'ptsfor' ,
                 'Sunday' ,
                 'Indoors' ,
                 'Precipitation' ,
                 'GoodConditions' ,
                 'VeryHot' ,
                 'VeryCold' ,
                 'VeryHumid' ,
                 'VeryWindy' ,
                 'BadWeather' ,
                 'NaturalSurface' ,
                 'expectedpts',
                 'expectedwin' ,
                 'win' ,
                 'tie')
# create a few identifiers
#this column will reference if a team is the home team; these are all visitors
df.V$home <- 0
#this column will specify opponents, in this case, the home teams from prev step
df.V$opp <- gameComplete$h
#this specifies points against, in this case, pts scored by home team
df.V$ptsagainst <- gameComplete$ptsh
# this is a unique identifier column, since gid is now replicated
df.V$uid <- 1:nrow(df.V)

# repeat with home team
df.H <- gameComplete[, names(gameComplete) %in% c('gid' ,
                                                  'seas' ,
                                                  'wk' ,
                                                  'h' ,
                                                  'temp' ,
                                                  'humd' ,
                                                  'wspd' ,
                                                  'ou' ,
                                                  'sprh' ,
                                                  'ptsh' ,
                                                  'Sunday' ,
                                                  'Indoors' ,
                                                  'Precipitation' ,
                                                  'GoodConditions' ,
                                                  'VeryHot' ,
                                                  'VeryCold' ,
                                                  'VeryHumid' ,
                                                  'VeryWindy' ,
                                                  'BadWeather' ,
                                                  'NaturalSurface' ,
                                                  'expectedptsh',
                                                  'expectedwinh' ,
                                                  'winh' ,
                                                  'tie')]
# fix names
names(df.H) <- c('gid' ,
                 'seas' ,
                 'wk' ,
                 'team' ,
                 'temp' ,
                 'humd' ,
                 'wspd' ,
                 'ou' ,
                 'sprfor' ,
                 'ptsfor' ,
                 'Sunday' ,
                 'Indoors' ,
                 'Precipitation' ,
                 'GoodConditions' ,
                 'VeryHot' ,
                 'VeryCold' ,
                 'VeryHumid' ,
                 'VeryWindy' ,
                 'BadWeather' ,
                 'NaturalSurface' ,
                 'expectedpts',
                 'expectedwin' ,
                 'win' ,
                 'tie')
# identifiers again
df.H$home <- 1 #this time, we're looking at home teams
df.H$opp <- gameComplete$v #making the visitors the opponents
df.H$ptsagainst <- gameComplete$ptsv
df.H$uid <- seq.int(from = 4081 , to = 8160) #finish the uid sequence

# merge the visitor and home data frames, so that we have a long data frame
gameMerged <- rbind(df.V , df.H)

# look at the data frame columns now!
str(gameMerged)

# tidy up
rm(df.H, df.V)

#### ENGINEER FEATURES #########################################################
# I think there are a lot of factors that could predict game outcomes, but we
#  only have so much time and data to work with here. S

# create points differential
gameMerged$ptdiff <- gameMerged$ptsfor - gameMerged$ptsagainst

# create a season/team ID for calculating cumulative variables
gameMerged$STID <- paste0(as.character(gameMerged$team) , gameMerged$seas)

# create a season/opponent ID for calculating opponent-related variables
gameMerged$SOID <- paste0(as.character(gameMerged$opp) , gameMerged$seas)

# re-order the df for some order-dependent calculations, first by STID, then wk
gameMerged <- gameMerged[order(gameMerged$STID , gameMerged$wk) , ]

# create a game-in-season index, which we'll use later for cumulative averages
gameMerged$gm <- ave(gameMerged$wk , gameMerged$STID , FUN = seq_along)

# create an opponent game-in-season index, which we'll use later for opp stats
gameMerged <- gameMerged[order(gameMerged$SOID , gameMerged$wk) , ]
gameMerged$oppgm <- ave(gameMerged$wk , gameMerged$SOID , FUN = seq_along)
# put back in order
gameMerged <- gameMerged[order(gameMerged$STID , gameMerged$wk) , ]

# calculate the cumulative sum of wins for each STID for each week; keep in mind
#  this is the number of wins the team has AFTER the game referenced in the row, 
#  so not a predictor!
gameMerged$seasSumWins.postgame <- ave(gameMerged$win , 
                                    gameMerged$STID , 
                                    FUN = cumsum)

# calculate cumulative point differential in the same way
gameMerged$seasSumPtDiff.postgame <- ave(gameMerged$ptdiff , 
                                      gameMerged$STID , 
                                      FUN = cumsum)

# calc average pt differential
gameMerged$seasAvgPtDiff <- gameMerged$seasSumPtDiff.postgame / gameMerged$gm

### now we've got plenty of fodder for engineering predictors
# setup lists to store values from forthcoming loops
prevPtsAgainst <- list()
prevWin <- list()
prevBye <- list()
prevCumWin <- list()
prevPtDiff <- list()
prevCumPtDiff <- list()
prevAvgPtDiff <- list()
# loop through to get values from previous game, which ARE predictors
for (i in 1:nrow(gameMerged)) {
  j = i-1
  if (i == 1) {
    prevPtsAgainst[[i]] <- NA
    prevWin[[i]] <- NA
    prevBye[[i]] <- 1 # this is mostly true!
    prevCumWin[[i]] <- NA
    prevPtDiff[[i]] <- NA
    prevCumPtDiff[[i]] <- NA
    prevAvgPtDiff[[i]] <- NA
  } else {
    if (gameMerged$STID[[i]] == gameMerged$STID[[j]]) {
      prevPtsAgainst[[i]] <- gameMerged$ptsagainst[[j]]
      prevWin[[i]] <- gameMerged$win[[j]]
      prevBye[[i]] <- ifelse(gameMerged$wk[[i]] - gameMerged$wk[[j]] > 1 , 
                             1 , 0)
      prevCumWin[[i]] <- gameMerged$seasSumWins.postgame[[j]]
      prevPtDiff[[i]] <- gameMerged$ptdiff[[j]]
      prevCumPtDiff[[i]] <- gameMerged$seasSumPtDiff.postgame[[j]]
      prevAvgPtDiff[[i]] <- gameMerged$seasAvgPtDiff[[j]]
    } else {
      prevPtsAgainst[[i]] <- NA
      prevPtsAgainst[[i]] <- NA
      prevBye[[i]] <- NA
      prevWin[[i]] <- NA
      prevCumWin[[i]] <- NA
      prevPtDiff[[i]] <- NA
      prevCumPtDiff[[i]] <- NA
      prevAvgPtDiff[[i]] <- NA
    }
  }
  print(paste("finished row ", i))
}

# merge all those lists together into a data frame
prev.df <- data.frame(prevPtsAgainst = unlist(prevPtsAgainst) ,
                      prevBye = unlist(prevBye) ,
                      prevWin = unlist(prevWin) , 
                      prevCumWin = unlist(prevCumWin) , 
                      prevPtDiff = unlist(prevPtDiff) , 
                      prevCumPtDiff = unlist(prevCumPtDiff) ,
                      prevAvgPtDiff = unlist(prevAvgPtDiff))

# add the new data frame to the old data frame
gameMerged <- cbind(gameMerged , prev.df)

# Outcomes of a game depend on opponents too; now get all the same predictors 
#  about the opponent! We'll use a similar methodology for storing loop output
#  in lists, but this time we don't need to reference previous rows, we can just
#  reference the gid and the opponent team, and use the values we just created

# initialize lists again
prevOppPtsAgainst <- list()
prevOppWin <- list()
prevOppBye <- list()
prevOppCumWin <- list()
prevOppPtDiff <- list()
prevOppCumPtDiff <- list()
prevOppAvgPtDiff <- list()

# loop through to get values from prevOppious game; we'll use some sql code to
#  do this via the function sqldf, which makes it pretty easy to get previous
#  values from a table based on a gid/team lookup. 
# NOTE: this loop takes a long time to run! give yourself a couple of hours...

for (i in 1:nrow(gameMerged)) {
  refgame <- gameMerged$gid[[i]] #the gid lookup value we want to use
  refteam <- gameMerged$opp[[i]] #the team lookup value we want to use
  # sqldf syntax means "choose the prevBye value from any rows in gameMerged
  #  where the gid is the same as refgame and team is the refteam (opponent)"
  prevOppBye[[i]] <- as.numeric(
    sqldf(paste0('select PrevBye from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  prevOppCumPtDiff[[i]] <- as.numeric(
    sqldf(paste0('select PrevCumPtDiff from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  prevOppCumWin[[i]] <- as.numeric(
    sqldf(paste0('select PrevCumWin from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  prevOppPtDiff[[i]] <- as.numeric(
    sqldf(paste0('select PrevPtDiff from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  prevOppPtsAgainst[[i]] <- as.numeric(
    sqldf(paste0('select PrevPtsAgainst from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  prevOppWin[[i]] <- as.numeric(
    sqldf(paste0('select PrevWin from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  prevOppAvgPtDiff[[i]] <- as.numeric(
    sqldf(paste0('select PrevAvgPtDiff from gameMerged
                 where gid = "', refgame, '" AND team = "', refteam, '"')))
  # print row progress
  print(paste0('completed row ', i))
}

# build a data.frame from the lists, same as before
prevOpp.df <- data.frame(prevOppPtsAgainst = unlist(prevOppPtsAgainst) ,
                      prevOppWin = unlist(prevOppWin) ,
                      prevOppBye = unlist(prevOppBye) ,
                      prevOppCumWin = unlist(prevOppCumWin) , 
                      prevOppPtDiff = unlist(prevOppPtDiff) , 
                      prevOppCumPtDiff = unlist(prevOppCumPtDiff) ,
                      prevOppAvgPtDiff = unlist(prevOppAvgPtDiff))

# combine gameMerged and the previous columns into yet another data frame!
gameFINAL <- cbind(gameMerged, prevOpp.df)

# DECISION POINT: Models like independent observations, but we currently have 2
#  observations for each gid, which violates the independence assumption. 
#  I'm going to remove duplicate gid's, should give us 4080 rows...
gameFINAL <- subset(gameFINAL , !duplicated(gid)) # 4080 rows!

# We have NAs remaining for the first game of every season. Models don't usually
#  like NAs. We could impute those like we did earlier with the missing weather 
#  data, but for the sake of simplicity (lol) we'll just exclude them.
gameCLEAN <- na.omit(gameFINAL)

# Output the final table so you don't have to go through that restructuring mess
#  again!
write.table(gameCLEAN , 
            file = "gameCLEAN.csv" , 
            sep = "," , 
            col.names = TRUE , 
            row.names = FALSE)

# tidy up
rm(list = ls(pattern = 'prev|Complete|Merged|ref*|[ij]'))
#### PREP DATA FOR MODELS ######################################################
# Now we have a data frame gameFINAL with each row representing a game from the
#  perspective of every team for every game, along with predictors for both the
#  team's and the opponent's previous performance that season. We're very nearly
#  ready to train and test models! 
#
# FIRST: since these models employ some randomness in their behind-the-scenes
#  functions, we use the function set.seed() to ensure reproducibility
set.seed(31483)

# subset our predictors, using regular expressions to select column names
predictornames <- 
  names(gameCLEAN)[!grepl('[ID]$|[id]$|team|^pt|win|tie|opp|seas$|postgame', 
                                   names(gameCLEAN))]
# make a dataframe of predictors
predictors <- gameCLEAN[, predictornames]

# subset our outcome, in this case if a team wins it's game or not; our models
#  will want this to be a factor
outcome <- as.factor(gameCLEAN$win)

# SCALING: VERY IMPORTANT! Many predictive model algorithms need to have the
#  units for each predictor on the same scale. I think it's most useful to scale
#  relative to the season and ideally only with respect to the games that have
#  actually happened, but we can survive without that, but for this example I am
#  going to keep it simple and just scale ALL of our predictors, which converts
#  their units into "standard deviations away from the mean."

predictors.scaled <- scale(predictors)

# Remove highly correlated features, as those things only contribute noise to 
#  our final model.
#
# Build a correlation matrix of your predictors
cormat <- cor(predictors.scaled)
# identify columns that are highly correlated with one another (> 0.9)
highcorr <- findCorrelation(cormat, cutoff = 0.9)
# remove those highly correlated columns (specifically, the ones that are least
#  correlated with the remaining predictors in the set, so you're retaining one
#  of each of the highly correlated column pairs)
predictors.scaled <- predictors.scaled[, -highcorr]

#nzv <- nearZeroVar(predictors.scaled) #empty set!


# DECISION POINT: How will we know that our model is predictive, i.e. capable of
#  generating USEFUL predictions for new data? Several options:
#  split data into 2 sets, build model on one set, test accuracy on the other;
#  do this a bunch of times, called cross-validation;
#  assume that our model is good enough to deploy without testing (never true)
#
#  We're going to use the first two suggestions: we'll split data into test and
#  training partitions, then use cross-validation to build a model in the train
#  partition, which we'll test on the test partition.

# partition your data into a test set and a training set based on outcome; split
#  will be a vector of rownumbers encompassing 80% of our rows
split <- createDataPartition(outcome , p = .8 , list = FALSE)
# create training set by selecting rows IN split
train.x <- predictors.scaled[split , ]
train.y <- outcome[split]
# create testing set by selecting rows OUT of split
test.x <- predictors.scaled[-split, ]
test.y <- outcome[-split]

#### BUILD MODELS ##############################################################
# Since the outcome is a binary factor, we're going to build a classification
#  model (as opposed to a regression model). This naming system can be confusing
#  since we'll actually be using regression to do this, among other things.

# Cross-validated LASSO binomial logistic regression model; uses 10 fold cross-
#  validation to build a model predicting y from x using binomial logistic
#  regression. LASSO has the added benefit of only selecting features that are
#  most important for the final output. We won't give a shit about significance
#  testing here, since we'll actually be testing the model's performance on data
#  from out of the training sample.
fit.lasso <- cv.glmnet(x = train.x ,
                       y = as.numeric(train.y) ,
                       family = 'binomial')

# look at important coefficients; since all of the predictors have been scaled,
#  it is reasonable to conclude that the predictors with the largest absolute
#  coefficient values contribute the most to the final prediction.
coef(fit.lasso)
# plot the error (here, binomial deviance) as a function of hte number of 
#  predictor variables used in the final model. The second dashed line is the
#  sweet spot number of predictors to be used, such that a model using that
#  number of predictors generates models with error within 1 standard error of
#  the minimum (which is the first dashed line, and is usually overfit!).
plot(fit.lasso)

# look at accuracy in sample; to do this, we'll make a confusion matrix, which
#  gives you a bunch of useful information. I generally care about the accuracy,
#  sensitivity (how likely you are to predict a win when there was a win), and
#  specificity (how likely you are to predict a loss when there was a loss).
#
# first generate predictions from your model, with in sample data;
#  outputs a matrix, so i'm converting to numeric
preds.lasso <- as.numeric(predict(fit.lasso , train.x , type = "response"))
# then convert your numeric to a factor (you can output a factor directly from
#  the predict function, but the levels are wacky sometimes)
class.lasso <- as.factor(ifelse(preds.lasso > 0.5 , 1 , 0))
# print your confusion matrix!
confusionMatrix(class.lasso , train.y)
# ~85.16% is pretty good! It's useful to compare this to the No Information Rate
#  in the output, which is what you'd get if you just predicted the bigger of
#  the 2 classes for each observation (here, 50.42%). So far so good!

# Now look at accuracy OUT of sample, which is the REAL test of our model;
#  same process as in sample accuracy, but we'll use test.x and test.y; we hope
#  that the accuracy out of sample is comparable to the accuracy in sample
oos.lasso <- as.numeric(predict(fit.lasso, test.x, type = "response"))
oos.lasso <- as.factor(ifelse(oos.lasso > 0.5 , 1 , 0))
confusionMatrix(oos.lasso , test.y)
# 84.69% is great, and very close to our in-sample prediction. When this happens
#  I tend to put trust in the model's outputs. Lets look at some more algorithms
#  to see if they perform any better!

# Multivariate Adaptive Regression Splines model; these are generally called
#  MARS models, but that acronym is actually trademarked, so we use a function
#  called earth to build them (very clever). These models treat predictors
#  differently depending on their range and relationship to outcome; this 
#  algorithm also utilizes feature selection, pulling out useless predictors.
fit.earth <- earth(x = train.x ,
                   y = train.y ,
                   pmethod = "cv" ,
                   ncross = 1 ,
                   nfold = 10)

# look at the different variables in the model; like with LASSO, the bigger
#  absolute coefficients are more important; unlike LASSO, the predictors are
#  transformed based on where they fall in the range (making them harder to 
#  interpret specific effects of a predictor)
summary(fit.earth)
# in sample accuracy
# predict.earth is much nicer about outputting usable predicted classes
class.earth <- predict(fit.earth , train.x , type = 'class')
confusionMatrix(class.earth, train.y) #84.08%, very similar to LASSO
# out of sample
oos.earth <- predict(fit.earth , test.x , type = 'class')
confusionMatrix(oos.earth , test.y) #82.2%, also close to LASSO

# RANDOMFOREST model; this creates a series of hierarchical decision trees, and
#  generates predictions by going through the trees. It's like a game of 20
#  questions, but repeated lots of times, with slight variations in the specific
#  questions asked each time

fit.rf <- randomForest(x = train.x ,
                       y = train.y)

# look at variable importance! this is measured by counting the number of times
#  a predictor is selected at a decision point in a tree; the plot is pretty
#  straightforward to interpret
varImpPlot(fit.rf)
# plot model performance as a function of number of trees. This is useful if we
#  think we have too many or too few trees...
plot(fit.rf)

# Test accuracy; randomForest objects already contain a nice confusion matrix
#  with out of bag predictions (basically out of sample), so we'll use that here
#  instead of the predict function
fit.rf$confusion
# accurate predictions = 1239 + 1186, all predictions = 1239 + 304 + 331 + 1186
#  = 79.25%; not as good as the others, but better than chance!
# Now out of sample, where we WILL use predict
oos.rf <- predict(fit.rf, test.x)
confusionMatrix(oos.rf, test.y)
# 79.97%

# DECISION POINT: We've run 3 models, and all are ~80-85% accurate on out of
#  sample data. If they were all correctly predicting the same games, then we
#  might just decide to use the most accurate model (LASSO here); if there's
#  variablility in the predictions, though, we can leverage using an ensemble...

# How related are the model's predictions? easier to discern this looking at the
#  predicted class probabilities, rather than the predicted classes
probs.lasso <- predict(fit.lasso, test.x , s = "lambda.1se" , type = "response")
probs.earth <- predict(fit.earth , test.x , type = "response")
# output of predict.randomforest is 2 col matrix; we want 2nd column
probs.rf <- predict(fit.rf , test.x , type = "prob")[, 2] 

# bind together, look at correlations
test <- data.frame(
  LASSO = as.numeric(probs.lasso),
  EARTH = as.numeric(probs.earth),
  RF = probs.rf,
  outcome = test.y)
cor(test[, 1:3])
# correlations are pretty high (.83-.9) but not perfect; we can get an little
#  improvement in our game predictions by combining the output of our 3 types
#  of models. This is called an ensemble, or stacked models, and if done the
#  right way, can actually improve your predictions by averaging out the errors.
#
# a simple stack is just an average of the predictions
test$meanProb <- rowMeans(test[, 1:3])
#
# a more complex stack is predicting the outcome from the predictions; we can
#  see how stacking with our 3 modeling algorithms looks, and choose the best
#
# lasso
stack.lasso <- cv.glmnet(x = as.matrix(test[, 1:3]) , 
                         y = test.y, 
                         family = 'binomial')
lassostack <- as.numeric(predict(stack.lasso , 
                         as.matrix(test[, 1:3]) , 
                         s = "lambda.1se" ,
                         type = "response"))
test$lassostack <- ifelse(lassostack > 0.5 , 1 , 0)
# earth
stack.earth <- earth(x = test[, 1:3] ,
                     y = test.y , 
                     pmethod = "cv" ,
                     ncross = 1 ,
                     nfold = 10)
test$earthstack <- predict(stack.earth , test[, 1:3] , type = 'class')
# random forest
stack.rf <- randomForest(x = test[, 1:3] ,
                         y = test.y , 
                         ntree = 100 ,
                         mtry = 1)
test$rfstack <- predict(stack.rf , test[, 1:3] , type = "class")
# Now we can see if any of the stacks help out! Currently our best model is the
#  LASSO, with an accuracy of 84.03% Can we beat it?
confusionMatrix(test$lassostack , test.y) #84.55% < 84.69%
confusionMatrix(test$earthstack , test.y) #83.64% > 84.69%
stack.rf$confusion #80.76% < 84.69%

# SO, after all is said and done, our LASSO predictions are more accurate on
#  out of sample data than earth or random forest models, or ensembles built
#  from those models.
#
# There are more than a million ways to improve on these models, including
#  adding more data (offensive/defensive production), better scaling, etc.
