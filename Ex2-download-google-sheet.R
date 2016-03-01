#In this exercise we'll download some data from a google sheet. In order to do that we'll have to authenticate through a google account which takes place in a browser.

#load the needed libraries.

library(googlesheets)
library(dplyr)

#Thanks to Kevin Cole for pointing out the googlesheets package and offering this bit of code to pull in the Google sheet. This relies on dplyr for passing the objects through a pipe (%>%) which we'll discuss more later. gs_read(ws = 2) says that we want the 2nd google sheet. Note that you may be redirected to your browser to provide Google Authentication.

rbTbl <- gs_url("https://docs.google.com/spreadsheets/d/1MhmzWDgIqCIoYL0K1c8MG43W1djmSw5trl5-oJfe24Q/") %>% gs_read(ws = 2)

#Maybe the first thing we want to do is drop columns that we don't need. We can do that by subsetting the data frame using its index properties. This line just redefines the data frame as being only columns 2 through 18.

rbTbl <- rbTbl[,2:18]

#Now we have some numeric columns that need to be transformed in order to be able to run any calculations on them. The height column uses the crazy combine format of FeetInchesEighths. So let's pull that into its component parts using some regular expressions. Regex is a powerful way to parse strings. We'll use the stringr package which you may need to install. Note that you can always install a package in RStudio by using the packages tab in the lower right.

library(stringr)

#This line uses the str_extract function to pull one piece of string out of another. When you're first learning how to use regular expressions it will be helpful to do these things one step at a time. Later you'll be able to combine several lines of code into one. This line says to extract the first occurrence {1} of the pattern [0-9] which essentially just tells it to pull out the first number it finds in the string. We know that no player is double digits in feet tall, so we just need to look for that first number in order to get the feet. We're creating a new column to hold this value.

rbTbl$FEET <- str_extract(rbTbl$Height,"[0-9]{1}")

#The next thing we want to do is look for the numbers that are in the 2-3 positions in the string, because those will be the inches of height. But that's easier to do if we delete the feet number out of the height column first. Otherwise we'll have to do a more complex Regex that tells it to look for the second occurence of something. So first we'll delete the feet number from the string using the str_replace function and then just specifying that we're going to replace it with nothing.

rbTbl$Height <- str_replace(rbTbl$Height,"[0-9]{1}","")

#Now we can use str_extract to pull out the inches digits since they're now first in the sequence after we've deleted the feet. Don't worry, we're going to put this all back together later. But this line of code says to find the first occurrence of two consecutive number characters and then create a new column called INCHES with those characters

rbTbl$INCHES <- str_extract(rbTbl$Height,"[0-9]{2}")

#IF you look at the data you can see that it's unlikely that the data is actually being recorded down to the eighth, which this format presupposes. More likely the data is being recorded down to the half inch. But we can still pull out the eighths by using a regular expression saying to find a single occurrence of a number character, but starting at the end of the string. The $ operator in a regular expression says to start at the end of the string.

rbTbl$EIGHTHS <- str_extract(rbTbl$Height,"[0-9]{1}$")

#Now we have three columns where we've done some string extraction to parse out the combine's height measurement. But the data is still in string or character format. So we'll use the sapply function to iterate over the 18:20 columns and apply the as.numeric function.

rbTbl[18:20] <- sapply(rbTbl[18:20],as.numeric)

#Now we can calculate the players' heights in inches by referring to each column by the dataFrame$column convention. 

rbTbl$Height <- rbTbl$FEET*12 + rbTbl$INCHES + rbTbl$EIGHTHS/8

#The weight column is in pretty good shape but we just need to make sure it's in number format since the functions that brought the data in are reliant on functions that default to character format. We can change to numeric with one line/function.

rbTbl$Weight <- as.numeric(rbTbl$Weight)

#This line just calculates the players' BMI. If we wanted to get rid of lots of decimal places we could wrap this function in a round()

rbTbl$BMI <- rbTbl$Weight*703/(rbTbl$Height^2)

#Both the arm length and the hand length can be parsed by pulling out the whole numbers, as well as the numerator and denominator of the fractions. This line extracts the first occurence of number characters until it runs into a non-numeric character. The + operator just tells it to keep going and not stop at any set number. That's because some hand lengths are single digits and some players have hands that are 10 inches.

rbTbl$WHOLE <- as.numeric(str_extract(rbTbl$Hand,"[0-9]+"))

#Now we can pull out the numerator of the fraction. In this case the pattern I'm telling it to look for is a space followed by a single numeric character. Then I wrap it in as.numeric to convert the string that it extracts into a number.

rbTbl$NUM <- as.numeric(str_extract(rbTbl$Hand," [0-9]"))

#to get the denominator we again use the $ operator to start at the end of the string.

rbTbl$DENOM <- as.numeric(str_extract(rbTbl$Hand,"[0-9]$"))

#Now it's time to start re-assembling those components back into a decimal version of hand size. First, you may note that some players don't have a fraction for their hand size because they're landing right on a whole number. In those cases the NUM column is NA. We can use that NA to tell R that in cases where NUM is NA, we want Hand to be equal to the WHOLE column. This line does that. It uses the brackets following the dataFrame$column method to tell R that we're only going to be using a subset of that column.

rbTbl$Hand[is.na(rbTbl$NUM)] <- rbTbl$WHOLE[is.na(rbTbl$NUM)]

#Now we're going to reverse what we've just done above by going to the instances where NUM IS NOT NA - remember that ! is the same thing as IS NOT. So each of the columns is subset to be only instances where NUM is not NA, and then we do a simple calculation to turn those component columns into a decimal representation of hand size. If you don't subset each of the values you're trying to use in your calculation then you'll run into an error because you'll be trying to add vectors of different lengths.

rbTbl$Hand[!is.na(rbTbl$NUM)] <- rbTbl$WHOLE[!is.na(rbTbl$NUM)] + (rbTbl$NUM[!is.na(rbTbl$NUM)]/rbTbl$DENOM[!is.na(rbTbl$NUM)])

#We can then re-use those columns we just created and go through the same exercise to convert Arm into a decimal.

rbTbl$WHOLE <- as.numeric(str_extract(rbTbl$Arm,"[0-9]+"))

rbTbl$NUM <- as.numeric(str_extract(rbTbl$Arm," [0-9]"))

rbTbl$DENOM <- as.numeric(str_extract(rbTbl$Arm,"[0-9]$"))

rbTbl$Arm[is.na(rbTbl$NUM)] <- rbTbl$WHOLE[is.na(rbTbl$NUM)]

rbTbl$Arm[!is.na(rbTbl$NUM)] <- rbTbl$WHOLE[!is.na(rbTbl$NUM)] + (rbTbl$NUM[!is.na(rbTbl$NUM)]/rbTbl$DENOM[!is.na(rbTbl$NUM)])

#Now we might want to calculate some speed scores. First we need to make sure that the forty yard dash column is named such that it will be easy to refer back to it. The colnames function should be familiar by now. Note that we could have named all columns before we started, and when you know exactly what you want to do that's easiest. I'm more making this up as I go.

colnames(rbTbl)[7] <- "FORTY.FIRST"

#Then we just make sure it's in numeric format. Again, if you know which columns you'll be working with ahead of time the easiest thing to do is to convert all of them to numeric in a sapply call at once.

rbTbl$FORTY.FIRST <- as.numeric(rbTbl$FORTY.FIRST)

#This is the formula for speed score.

rbTbl$SPEED <- round(rbTbl$Weight*200 / (rbTbl$FORTY.FIRST^4),0)

#If I wanted just a table of weight, forty and speed score I can subset the columns using a named list. The column names need to be quoted.

ssTbl <- rbTbl[,c("Name","Weight","FORTY.FIRST","SPEED")]

