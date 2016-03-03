#### LIBRARIES #################################################################
library(XML)

#### READ DATA #################################################################
# make a list of the url's you want to scrape (generated from xml-sitemaps.com)
urlList <- list(
  'http://www.footballstudyhall.com/pages/2015-clemson-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ohio-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-oklahoma-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ole-miss-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-michigan-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-florida-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-notre-dame-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-lsu-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-stanford-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-western-kentucky-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-washington-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-michigan-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-baylor-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-arkansas-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-mississippi-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-usc-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-florida-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-bowling-green-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-navy-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-toledo-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-tennessee-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ucla-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-west-virginia-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-oregon-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-tcu-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-utah-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-louisville-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-north-carolina-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-penn-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-wisconsin-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-georgia-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-oklahoma-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-california-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-auburn-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-nc-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-minnesota-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-byu-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-georgia-southern-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-pittsburgh-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-boise-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-texas-a-m-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-appalachian-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-houston-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-san-diego-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-utah-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-iowa-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-nebraska-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-temple-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-south-florida-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-miami-fl-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-air-force-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-western-michigan-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-louisiana-tech-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-memphis-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-northwestern-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-arizona-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-southern-miss-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-virginia-tech-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-indiana-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-illinois-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-texas-tech-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-washington-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-marshall-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-maryland-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ohio-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-boston-college-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-georgia-tech-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-northern-illinois-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-iowa-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-syracuse-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-texas-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-virginia-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-cincinnati-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-duke-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-east-carolina-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-arizona-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-central-michigan-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-arkansas-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-akron-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-colorado-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-middle-tennessee-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-vanderbilt-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-kansas-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-missouri-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-purdue-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-connecticut-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-south-carolina-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-san-jose-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-troy-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-georgia-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-wake-forest-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-florida-atlantic-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-tulsa-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-kentucky-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-buffalo-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-massachusetts-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-colorado-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-nevada-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-fresno-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-new-mexico-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-oregon-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-south-alabama-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-smu-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-rutgers-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-unlv-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ul-lafayette-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-florida-international-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-idaho-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-old-dominion-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ball-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-texas-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-utsa-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-wyoming-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-kent-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-new-mexico-state-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-miami-oh-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-hawaii-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-rice-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-tulane-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-eastern-michigan-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-ul-monroe-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-army-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-charlotte-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-central-florida-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-utep-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-kansas-advanced-statistical-profile'	,
  'http://www.footballstudyhall.com/pages/2015-north-texas-advanced-statistical-profile')

# initialize an empty list for storing your 
dfList <- list()

# loop through URLs from urlList to create a list of data frames
for (i in urlList) { 
  Sys.sleep(3) #we don't want to crush the server
  temp <- readHTMLTable(i , #one of the URLs from urlList
                        header = TRUE , #the columns have labels
                        which = 14 , #we only want the 14th table for IDP stats
                        colClasses = c(rep('factor' , 2) ,
                                       'character' ,
                                       'factor' ,
                                       'numeric' ,
                                       'character' ,
                                       rep('numeric' , 6)))
  # add a column for the college name
  college.temp <- strsplit(i, '2015-') #splits the string i at '2015-'
  temp$College <- strsplit(college.temp[[1]][[2]], '-advanced')[[1]][[1]] #subset
  
  dfList[[i]] <- temp #add temp dataframe from url i to your dfList
}

# combine all of the data frames in dfList into one data frame
output <- do.call('rbind', dfList)

#### INSPECT YOUR DATA #########################################################
# summary statistics
summary(output)

# look at rows with NAs
summary(output[is.na(output) , ])

#### CLEANSE YOUR DATA #########################################################
# since all the rows with NAs are all NAs, discard them; 
#  you can do this w/o droplevels() but makes factors cleaner
clean <- droplevels(na.omit(output))
summary(clean) #no NAs!

# fix % of Team tackles; currently is character string, needs to be a number
clean$teamTacklePerc <- as.numeric( #we want this to be a number
                                  gsub('%' , #we're substituting % character
                                       '' , #with nothing
                                       clean$`% of Team` , #the input values
                                       fixed = TRUE)) #in case % is a regex
clean$`% of Team` <- NULL #delete the original column for tidiness

# fix College
clean$College <- as.factor(clean$College) #make a factor as it's easier to group

# fix height and weight
# height
# split the character string at the comma to separate ht from wt
hw <- strsplit(clean$`Ht, Wt` , ', ')
# hw is a list of 2 element vectors, and we want the first element from each list value
ht <- unlist(lapply(hw , function(x) x[[1]]))
# ht is a list of 2 element vectors, and again we want the first element for feet
feet <- as.numeric(unlist(lapply(strsplit(ht, '\''), function(x) x[[1]])))
# repeat for inches, but grab the second element
inches <- as.numeric(unlist(lapply(strsplit(ht, '\''), function(x) x[[2]])))
# compute height
clean$Height <- (feet * 12) + inches

# weight
# much easier!
clean$Weight <- as.numeric(unlist(lapply(hw , function(x) x[[2]])))
# tidy up
clean$`Ht, Wt` <- NULL

#### OUTPUT ####################################################################

write.table(clean , #object to write
            file = 'ncaaIDP.csv' , #name of output
            sep = ',' , #how the values in the output are separated
            row.names = FALSE , #we don't want rownames
            col.names = TRUE) # we do want column names