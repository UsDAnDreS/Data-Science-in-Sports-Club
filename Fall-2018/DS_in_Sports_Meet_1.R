####
## REMINDER: "DOING DATA SCIENCE" BOOK
##            BLOGS - LYFT, UBER, GOOGLE
##################

library(xml2)
library(rvest)  # Should be installed along with 'xml2'
library(plotrix) # Also install it please.

url_link <- "https://www.sports-reference.com/cfb/schools/houston/2017/gamelog/"
#url_link <- "https://www.sports-reference.com/cfb/players/ed-oliver-2.html"

# Read the HTML webpage for that link into an R object
url <- read_html(url_link) 

# html_nodes(url, ".left , .right")
#library(httr)
# res <- GET("https://www.sports-reference.com/cfb/schools/houston/2017/gamelog/")
# doc <- read_html(content(res, as="text"))
# doc
# html_nodes(doc, "#defense th , #defense .left , #defense .right")


# Find a 'table' on that HTML page
table_one <- xml_find_all(url, "//table") 
class(table_one)

# Obtain the actual contents of the table, wrapped into a data frame
table_one <- html_table(table_one)
class(table_one)
class(table_one[[1]])

table_one
length(table_one)

table_one[[1]]
class(table_one[[1]])

# Looks promising, but there's one big issue - there were two header rows in the table,
# and only the first one got accounted for. Hence the second header row ('Rk', 'Date', 'Opponent' etc)
# is considered as the first OBSERVATION rather than COLUMN NAMES.

# That can be fixed via hard-coding:
tab.col.names <- table_one[[1]][1,]
table_one[[1]] <- table_one[[1]][-1,]
colnames(table_one[[1]]) <- tab.col.names
table_one[[1]]
rownames(table_one[[1]]) <- c(1:nrow(table_one[[1]]))
table_one[[1]]

# Also, we don't quite need the LAST ROW, which is just totals for the year:
# we can always calculate those on our own.
table_one[[1]] <- table_one[[1]][-nrow(table_one[[1]]),]


# Let's now take some variable summaries, e.g. average no. of completions:
mean(table_one[[1]]$Cmp)

# What's wrong? Check the types of variables in data frame resulting from read_html operation:
summary(table_one[[1]])
str(table_one[[1]])

# All are automatically treated as characters. We'd like to convert most of those (but NOT ALL) to numeric.
# In particular, everything except Date, Home/Away marker, Opponent and Result.
head(table_one[[1]])
colnames(table_one[[1]])[3] <- "Home/Away"

as.numeric(table_one[[1]]$Opponent)

# Function as numeric comes to rescue:
table_one[[1]][,-c(2:5)] <- lapply(table_one[[1]][,-c(2:5)],as.numeric)

# Now verify that everything is in order:

head(table_one[[1]])
summary(table_one[[1]])
str(table_one[[1]])


# Get pairwise scatterplots for different variables
pairs(table_one[[1]]) # Error - get rid of non-numerical stuff

pairs(table_one[[1]][,-c(1:5)])
# A bit problematic to read, isn't it?

# Let's just look at subsets of variables
pairs(table_one[[1]][,c(6:10)])
pairs(table_one[[1]][,c(11:19)])

# Honestly, there's not too much you can ever see from 12 data points.

plot(Cmp ~ Att, data=table_one[[1]])
plot(Cmp ~ Pct, data=table_one[[1]])


####
## REMINDER: "DOING DATA SCIENCE" BOOK
##            BLOGS - LYFT, UBER, GOOGLE
##################


####
##  HADLEY, rvest TUTORIAL? DEMO? https://github.com/hadley/rvest
##                                https://github.com/hadley
##
#####

#####
##   vignette("selectorgadget")
########



#################################
### GETTING DATA ON MORE TEAMS ##
#################################


# Let's get tables on several teams, to obtain a larger sample of games.
# For that, first we need to specify the url-versions of team names we're interested in:
team_names <- c('houston', 'rice', 'texas-tech','temple','tulsa')

# Next, we need to code up a method to automatically creat a full URL for each team name.
# It is done via 'paste()' function.

# E.g., how can we get the name:
#  'https://www.sports-reference.com/cfb/schools/houston/2017/gamelog/'
# via paste() function and the team name 'houston'?

url_link <- paste('https://www.sports-reference.com/cfb/schools/',team_names[1],'/2017/gamelog/')
url_link

# Issue here? There are white spaces at string connection points.
# Use 'sep=' option to get rid of those.

url_link <- paste('https://www.sports-reference.com/cfb/schools/',team_names[1],'/2017/gamelog/',
                  sep="")
url_link

# And we got it from here - now we could simply apply all of the aforementioned code
# to this calculated url.
# Now, how do we automatically loop through all the team names? Easy:
#  Loop through indices (i=1,...,5), where for each i we:
#       Calculate the url for team_names[i]
#       Extract the table.


# We'll keep all the tables in one big data frame object,
# and also REPLACE THE USELESS FIRST 'Rk' COLUMN for TEAM NAME column A


offense_data <- NULL
team_names <- c('houston', 'rice', 'texas-tech','temple','tulsa')

for (i in team_names){
  url_link <- paste('https://www.sports-reference.com/cfb/schools/',i,'/2017/gamelog/',
                    sep="") 
  
  print(url_link)
  
  url <- read_html(url_link) 
  table_one <- xml_find_all(url, "//table")
  table_one <- html_table(table_one)[[1]]
  
  tab.col.names <- table_one[1,]
  table_one <- table_one[-1,]
  colnames(table_one) <- tab.col.names
  
  table_one <- table_one[-nrow(table_one),]
  table_one[,-c(2:5)] <- lapply(table_one[,-c(2:5)],as.numeric)
  table_one[,1] <- i
  colnames(table_one)[1] <- "Team"
  
  print(head(table_one,2)) # Print out extracted table for the team.
  cat("\n")
  offense_data <- rbind(offense_data, table_one)
}


dim(offense_data)
head(offense_data)

# Save the scraped offensive logs data.
write.table(offense_data,"Scraped_Offense.txt")


# Get pairwise scatterplots for different variables
# Let's just look at subsets of variables
pairs(offense_data[,c(6:10)])
pairs(offense_data[,c(11:19)])

##  ???? why plot(Pct ~ Yds, data = offense_data)

class(offense_data)

## Get correlation matrix.
offense_data_num <- offense_data[,-c(1:5)]
cor(offense_data_num)  # Tough to read.

## Try plotting?
library(plotrix)
color2D.matplot(cor(offense_data_num),
                #cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T)

# We are not really interested in SIGN of correlation (whether it's positive or negative),
# but more in just the STRENGTH of correlation. Apply 'abs()' function.

color2D.matplot(abs(cor(offense_data_num)),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T)

# Cleaner, but still a bit dirty. We could THRESHOLD values, so that
# all correlations less than the thresholds are SET TO 0.
# This may be done via 'ifelse()'

abs.cor.mat <- abs(cor(offense_data_num))

color2D.matplot(ifelse(abs.cor.mat>=0.8, abs.cor.mat,0),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T)

colnames(abs.cor.mat)

## To pretty up the plot and make it much more informative, we could take
## numerous steps, e.g. making the variable names show up along rows and columns.

color2D.matplot(ifelse(abs.cor.mat>=0.8, abs.cor.mat,0),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend = T,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
axis(1,at=c(1:ncol(abs.cor.mat))-0.5,labels=colnames(abs.cor.mat))
par(las=1)
axis(2,at=c(ncol(abs.cor.mat):1)-0.5,labels=colnames(abs.cor.mat))

# title("Game Stats Correlation Matrix")




## 
thresholded.corr <- function(data,thr=0.8){
  cor.mat <- abs(cor(data))
  return(ifelse(cor.mat > thr,cor.mat,0))
}



no.TDs <- subset(Full_Team_data_numeric, select = -c(TD,TD.1))

color2D.matplot(thresholded.corr(no.TDs),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend = T,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
axis(1,at=c(1:ncol(no.TDs))-0.5,labels=colnames(no.TDs))
par(las=1)
axis(2,at=c(ncol(no.TDs):1)-0.5,labels=colnames(no.TDs))