####
## REMINDER: GOOGLE DATA SEARCH (LAUNCHED SEP 5TH)
##           https://toolbox.google.com/datasetsearch
##################

library(xml2)
library(rvest)  # Should be installed along with 'xml2'
library(plotrix) # Also install it please.
library(stringi) # Needed for certain string operations.

offense_data <- read.table("Scraped_Offense.txt")

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


## This only finds the OFFENSE Table....
# xml_find_all looks in-between the <> of the object resulting from read_html().
all_tables <- xml_find_all(url, "//table") 
class(all_tables)
all_tables


### How to get the DEFENSE TABLE??

####
##  HADLEY, rvest TUTORIAL? DEMO? https://github.com/hadley/rvest
##                                vignette("selectorgadget")
## 
# From the VIGNETTE EXAMPLE:

# html <- read_html("http://www.imdb.com/title/tt1490017/")
# cast <- html_nodes(html, "#titleCast .itemprop")
# length(cast)
#####

# This 'selectorgadget' approach via html_nodes() + css didn't work.. Gotta ask Hadley about this whole business.
#all_nodes <- html_nodes(url, "th , .right , .left") 
all_nodes <- html_nodes(url,"#defense .left , #defense .right , #defense_clone .left")
class(all_nodes)
length(all_nodes)


## It turns out, the DEFENSE table is COMMENTED OUT in the page source code.

find_extra_table <- function(url_link){
  # Additional tables are within the comment tags -  between "<!--"  and  "-->"
  # Which is why your xpath is missing them.
  
  # Find all the commented nodes. 
  # xml_find_all looks in-between the <> of the object resulting from read_html().
  alt_tables <- xml_find_all(read_html(url_link),"//comment()")
  
  #Among commented nodes, find those containing '<table' reg. expression 
  #raw_parts <- as.character(alt_tables[grep("\\</?table", as.character(alt_tables))])
  raw_parts <- as.character(alt_tables[grep("<table", as.character(alt_tables))])
  
  # Remove the comment begin ("<!--") and end ("-->") tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  
  # Apply what we did for UNcommented table before
  strip_html <- read_html(strip_html)
  table_two <- xml_find_all(strip_html, "//table") 
  class(table_two)
  
  # Obtain the actual contents of the table, wrapped into a LIST of DATA FRAMES
  table_two <- html_table(table_two)
  return(table_two)
}

find_extra_table(url_link)[[1]]


########################
########################

# We'll keep all the tables in one big data frame object,
# and also REPLACE THE USELESS FIRST 'Rk' COLUMN for TEAM NAME column A

defense_data <- NULL
team_names <- c('houston', 'rice', 'texas-tech','temple','tulsa')

for (i in 1:length(team_names)){
  url_link <- paste('https://www.sports-reference.com/cfb/schools/',team_names[i],'/2017/gamelog/',
                    sep="") 
  table_two <- find_extra_table(url_link)[[1]]
  
  # That can be fixed via hard-coding:
  tab.col.names <- table_two[1,]
  table_two <- table_two[-1,]
  colnames(table_two) <- tab.col.names
  rownames(table_two) <- c(1:nrow(table_two))
  
  # Also, we don't quite need the LAST ROW, which is just totals for the year:
  # we can always calculate those on our own.
  table_two <- table_two[-nrow(table_two),]
  
  # Function as numeric comes to rescue:
  table_two[,-c(2:5)] <- lapply(table_two[,-c(2:5)],as.numeric)
  table_two[,1] <- team_names[i]
  colnames(table_two)[1] <- "Team"
  
  defense_data <- rbind(defense_data, table_two)
}

dim(defense_data)

head(defense_data)

# Save the scraped defensive logs data.
write.table(defense_data,"Scraped_Defense.txt")

# And just read it back into defense_data right away, 
# to make sure both offense_data and defense_data are of the same format.
defense_data <- read.table("Scraped_Defense.txt") 

################
##  To FINALIZE the data WRANGLING 
# ( <=> putting data into that FINAL FORMAT which we'll use for model fitting/analysis)
##  we will convert the 'Result' string into:
#     - POINTS SCORED (for the OFFENSE table)
#     - POINTS ALLOWED (for the DEFENSE table)
################

offense_data <- read.table("Scraped_Offense.txt")
## First, let's convert a single 'Result' value into points scored and allowed.

offense_data$Result[1]
class(offense_data$Result[1])

# Factor variable can't be worked on in terms of extracting sub-strings => 
# convert to character via 'as.character()' function.

as.character(offense_data$Result[1])
# substr(as.character(offense_data$Result[1]), 1,3)

# To split it into "points scored" & "points allowed", we use base R's 'strsplit()' function, 
# which takes as arguments:
#   - String to be split into parts,
#   - Character according to which the string will be split.

spl <- strsplit(as.character(offense_data$Result[1]), split=" ")
spl[[1]]
spl[[1]][2]

score <- spl[[1]][2]
score.no.parenth <- substr(score,
                           start=2,
                           stop=nchar(score)-1)

strsplit(score.no.parenth,
         split="-")

points.scored <- strsplit(score.no.parenth,split="-")[[1]][1]
points.allowed <- strsplit(score.no.parenth,split="-")[[1]][2]

points.scored
points.allowed

as.numeric(points.scored)
as.numeric(points.allowed)

Points <- sapply(as.character(offense_data$Result),
       function(x){ 
         spl <- strsplit(as.character(x), split=" ");
         score <- spl[[1]][2]
         score.no.parenth <- substr(score,2,nchar(score)-1)
         points.scored <- strsplit(score.no.parenth,split="-")[[1]][1]
         points.allowed <- strsplit(score.no.parenth,split="-")[[1]][2]
         return(c(points.scored=as.numeric(points.scored),
                  points.allowed=as.numeric(points.allowed)))})


offense_data <- read.table("Scraped_Offense.txt")
defense_data <- read.table("Scraped_Defense.txt")

offense_data$Points <- Points[1,]
defense_data$Points <- Points[2,]


#####################
##  Get some numeric summaries
#####################

### Offense
attach(offense_data)
# Passing
tapply(Yds, Team, mean)
tapply(TD, Team, mean)
# Rushing
tapply(Yds.1, Team, mean)
tapply(TD.1, Team, mean)
# Turnovers
tapply(Fum, Team, mean)
tapply(Int, Team, mean)
tapply(Tot.1, Team, mean)
detach(offense_data)


### Defense
attach(defense_data)
# Passing
tapply(Yds, Team, mean)
tapply(TD, Team, mean)
# Rushing
tapply(Yds.1, Team, mean)
tapply(TD.1, Team, mean)
# Turnovers
tapply(Fum, Team, mean)
tapply(Int, Team, mean)
tapply(TO, Team, mean)
detach(defense_data)


## This code looks way too repetitive. How to avoid that? 
# That's where fancy coding comes in: using 'tapply' WITHIN AN 'apply()' CALL.
# We use 'apply' to work on the 'offense_data' data frame's COLUMNS,
# and the function we apply to EACH COLUMN is 'tapply(..)'

# Separately create a vector of team names, and of STAT names.
Team.vec <- offense_data$Team
stat.names.off <- c("Yds","TD","Yds.1","TD.1","Fum","Int","Tot.1")
stat.names.def <- c("Yds","TD","Yds.1","TD.1","Fum","Int","TO")

# Offense.

# Means (apply to COLUMNS - MARGIN=2)
off.team.means <- apply(offense_data[,stat.names.off], MARGIN=2, 
                        function(x) tapply(x,Team.vec,mean))
print(off.team.means)

# Medians.
off.team.medians <- apply(offense_data[,stat.names.off], 2, 
                          function(x) tapply(x,Team.vec,median))
print(off.team.medians)

# Defense.
def.team.means <- apply(defense_data[,stat.names.def], 2, 
                        function(x) tapply(x,Team.vec,mean))
print(def.team.means)

def.team.medians <- apply(defense_data[,stat.names.def], 2, 
                          function(x) tapply(x,Team.vec,median))
print(def.team.medians)



##########################
## Now that we have data on both offense and defense,
## we have complete picture for each of 62 games,
## and we could proceed to try and explain the factors driving the outcome of the game.
##########################

diff_data <- offense_data
diff_data[,-c(1:5)] <- offense_data[,-c(1:5)] - defense_data[,-c(1:5)]
head(diff_data)


## Try plotting?
library(plotrix)

diff_data_num <- diff_data[,-c(1:5)]

# Get the pretty thresholded heatmap of correlation matrix. 
color2D.matplot(ifelse(abs.cor.mat>=0.75, abs.cor.mat,0),
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend = T,
                xlab='',
                ylab='',
                axes=F)
par(las=2)
axis(1,at=c(1:ncol(abs.cor.mat))-0.5,labels=colnames(abs.cor.mat))
par(las=1)
axis(2,at=c(ncol(abs.cor.mat):1)-0.5,labels=colnames(abs.cor.mat))



#### Fitting a LINEAR MODEL to PREDICT SCORE DIFFERENTIALS.

## Fitting a linear model with hand-picked variables.

lm.obj <- lm(Points ~ Cmp + Pct + Att.1 + Avg + Avg.1 + No. + Fum + Int,
              data=diff_data_num)

summary(lm.obj)


# That's actual values
diff_data$Points

# That's how we obtain predicted values, via predict() function
model.vars <- c("Cmp","Pct","Att.1","Avg","Avg.1","No.","Fum","Int")
predict(lm.obj,newdata = diff_data_num[,model.vars])

pred.mat <- data.frame(true = diff_data_num$Points,
                       predicted = predict(lm.obj,diff_data_num[,model.vars]))
head(pred.mat,20)
print(sum((pred.mat$true - pred.mat$predicted)^2)/nrow(pred.mat))

# To make it nicer, round the predicted values to integers

pred.mat <- data.frame(true = diff_data_num$Points,
                       predicted = predict(lm.obj,diff_data_num[,model.vars]))
pred.mat$predicted <- round(pred.mat$predicted)
head(pred.mat,20)

# Plotting all the residuals
plot(residuals(lm.obj))

# Prediction INTERVALS

pred.int <- predict(lm.obj,newdata=diff_data_num[,model.vars], int="predict")
head(pred.int,10)

pred.mat <- data.frame(true = diff_data_num$Points,
                       predicted.low = round(pred.int[,"lwr"]),
                       predicted.high = round(pred.int[,"upr"]))
head(pred.mat, 20)

# Confidence INTERVALS

pred.int <- predict(lm.obj,newdata=diff_data_num[,model.vars], int="conf")
head(pred.int,10)

pred.mat <- data.frame(true = diff_data_num$Points,
                       predicted.low = round(pred.int[,"lwr"]),
                       predicted.high = round(pred.int[,"upr"]))
head(pred.mat, 20)


## TRAIN/TEST DATA APPROACH

set.seed(1)
n <- nrow(diff_data_num)
train.size <- 0.9*nrow(diff_data_num)
train.ind <- sample(1:n,train.size)
test.ind <- c(1:n)[-train.ind]

model.vars <- c("Cmp","Pct","Att.1","Avg","Avg.1","No.","Fum","Int")

lm.obj <- lm(Points ~ Cmp + Pct + Att.1 + Avg + Avg.1 + No. + Fum + Int,
             data=diff_data[train.ind,])

pred.mat <- data.frame(true = diff_data_num[test.ind,]$Points,
                       predicted = predict(lm.obj, diff_data_num[test.ind,model.vars]))
pred.mat$predicted <- round(pred.mat$predicted)
head(pred.mat,20)

MSE <- sum((pred.mat$true - pred.mat$predicted)^2)/nrow(pred.mat)
MSE



## Doing many random subdivisions of data into train/test subsets,
## in order to obtain a more reliable estimate of PREDICTION ERROR.

B <- 100
all.MSEs <- numeric(B)

for (b in 1:B){
  n <- nrow(diff_data_num)
  train.size <- 0.9*nrow(diff_data_num)
  train.ind <- sample(1:n,train.size)
  test.ind <- c(1:n)[-train.ind]
  
  model.vars <- c("Cmp","Pct","Att.1","Avg","Avg.1","No.","Fum","Int")
  
  lm.obj <- lm(Points ~ Cmp + Pct + Att.1 + Avg + Avg.1 + No. + Fum + Int,
               data=diff_data[train.ind,])
  
  pred.mat <- data.frame(true = diff_data[test.ind,]$Points,
                         predicted = predict(lm.obj, diff_data[test.ind,model.vars]))
  
  all.MSEs[b] <- sum((pred.mat$true - pred.mat$predicted)^2)/nrow(pred.mat)
}

print(mean(all.MSEs))
hist(all.MSEs)

















#### Doing TRAIN/TEST SET SUBDIVISION

set.seed(1)
n <- nrow(diff_data_num)
train.size <- 0.9*nrow(diff_data_num)
train.ind <- sample(1:n,train.size)
test.ind <- c(1:n)[-train.ind]

data.subset <- subset(diff_data_num, 
                      select=-c(Plays,Yds.2,Tot,Tot.1,
                                Yds,Yds.1,Cmp, Yds.3,
                                TD,TD.1,
                                Pass,Rush,Pen))
select.model <- lm(Points ~ ., 
                   data=data.subset[train.ind,])

mean((predict(select.model,data.subset[test.ind,-ncol(data.subset)]) - 
                              data.subset[test.ind,]$Points)^2)
print(cbind(true = data.subset[test.ind,]$Points,
            predicted = predict(select.model,data.subset[test.ind,-ncol(data.subset)])))

## Not too bad.
## To get a BETTER ESTIMATE OF ERROR.

B <- 100
all.MSEs <- numeric(B)

for (b in 1:B){
  n <- nrow(diff_data_num)
  train.size <- 0.9*nrow(diff_data_num)
  train.ind <- sample(1:n,train.size)
  test.ind <- c(1:n)[-train.ind]
  
  data.subset <- subset(diff_data_num, 
                        select=-c(Plays,Yds.2,Tot,Tot.1,
                                  Yds,Yds.1,Cmp, Yds.3,
                                  TD,TD.1,
                                  Pass,Rush,Pen))
  select.model <- lm(Points ~ ., 
                     data=data.subset[train.ind,])
  
  all.MSEs[b] <- mean((predict(select.model,data.subset[test.ind,-ncol(data.subset)]) - 
          data.subset[test.ind,]$Points)^2) 
}

print(mean(all.MSEs))

######################
######################

library(car)
vif(offense_data)