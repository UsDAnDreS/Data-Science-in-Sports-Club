library(rvest)  # Should be installed along with 'xml2'

########
## Scraping game scores.
########

# Scraping all the scores up until the end of January of 2018. 
# No comments, simply dig into this code upon your desire.  
# We've covered very similar examples with college football data.

months <- c("october","november","december","january")
all_games <- NULL

for (month_name in months){
  url_link <- paste('https://www.basketball-reference.com/leagues/NBA_2018_games-', 
                    month_name,
                    '.html',
                    sep="")
  
  # Read the HTML webpage for that link into an R object
  url <- read_html(url_link)
  url
  
  # Find a 'table' on that HTML page
  table_one <- xml_find_all(url, "//table") 
  # Obtain the actual contents of the table, wrapped into a data frame
  table_one <- html_table(table_one)
  length(table_one)
  
  all_games <- rbind(all_games,table_one[[1]])
}

head(all_games)
str(all_games)

all_games <- all_games[,c(3:6)]   # Retain only needed info

# Convert scores from "chr" to "numeric"
all_games[,c(2,4)] <- apply(all_games[,c(2,4)],
                            2,
                            as.numeric)

str(all_games) # Now we're all set.



#######
## Scraping team performance stats.
######

# Here we'll repeat the code we used to scrape data for NBA team clustering (see the respective code), 
# with only exception being - we'll obtain data until Jan 31st (to include a full month of January), 
# rather that Jan 18th.

url_link <- 'https://www.basketball-reference.com/friv/standings.fcgi?month=1&day=31&year=2018&lg_id=NBA'

# Read the HTML webpage for that link into an R object
url <- read_html(url_link)
url


# Define the function to extract the "commented out" tables.

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
  
  # Read all the tables that used to be commented before.
  alt_tables <- lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table"))[[1]]
  })
  
  return(alt_tables)
}

###  Getting the needed tables.

all_stats <- find_extra_table(url_link)
team_stats <- all_stats[[1]]
opp_stats <- all_stats[[2]]

head(team_stats)
head(opp_stats)

###  Cleaning up memory.

rm(all_stats)
team_stats$Rk <- opp_stats$Rk <- NULL
team_stats <- team_stats[-nrow(team_stats),]
opp_stats <- opp_stats[-nrow(opp_stats),]

# Next, we need to obtain **per-game averages** in such categories as 
# FG made, attempted (FGM/A),  3PTM/A, FTM/A, 
# offensive rebounds (ORB), DRB, TRB, AST, STL, BLK, TOV, PF, PTS.

# All but the %-type of stats (FG%, 3PT% etc). 
# We can obtain indices of those %-stats and other unwanted columns (team names) as follows 
# (again some data wrangling/string operations):


ind.exclude <- which(grepl("%",names(team_stats)))
ind.exclude <- c(ind.exclude, which(grepl("[a-z]",names(team_stats))))
ind.exclude


### Sub the totals for averages.

team_stats[,-ind.exclude] <- apply(team_stats[,-ind.exclude],
                                   2,
                                   function(x) x/team_stats$G)

opp_stats[,-ind.exclude] <- apply(opp_stats[,-ind.exclude],
                                  2,
                                  function(x) x/opp_stats$G)


#head(team_stats)
#head(opp_stats)

### Get rid of "G" and "MP" columns.

team_stats$G <- opp_stats$G <- NULL
team_stats$MP <- opp_stats$MP <- NULL





# We will also use the same variables we've hand-picked via studying correlations and 
# multi-collinearities when performing NBA team clustering:

# Var "selection": gotta get rid of correlated and collinear stuff.

needed.vars.team <- c("FGA","FG%","3PA","3P%", "ORB", "TRB", "AST", "BLK", "TOV","PF")
needed.vars.opp <- c("FGA","FG%","3PA", "3P%",  "ORB", "TRB", "AST", "TOV")

all_stats <- merge(team_stats[,c("Team",needed.vars.team)], 
                   opp_stats[,c("Team",needed.vars.opp)], 
                   by = "Team")
head(all_stats)



############
### Actual Modeling.
############

# Setting up the data frame into a nice format, to feed to lm() eventually.

colnames(all_games)
full_data <-  merge(all_games, all_stats, by.x=c("Visitor/Neutral"), by.y=c("Team"))
p <- dim(full_data)[2]-4
full_data <- merge(full_data, all_stats, by.x=c("Home/Neutral"), by.y=c("Team"))

# The resulting 'full_data' frame was pretty messy, 
# hence a weird indexation in the line below.
diff_data <- full_data[,c(4,(5+p):ncol(full_data))] - full_data[,c(3,5:(5+p-1))]
dim(diff_data)
head(cbind(full_data[,c(1,2)],diff_data))



## Fitting the LINEAR MODEL via lm().

lm.obj <- lm(PTS.1 ~ ., data=diff_data)
summary(lm.obj)



## VARIANCE INFLATION FACTOR.

# You could take care of potential collinearities among your predictors
# via studying VARIANCE INFLATION FACTOR.

# Multicollinearity reduces a model's legitimacy and predictive power. 
# Variance inflation factors allow a quick measure of how much a variable 
# is contributing to the standard error in the regression.

library(car)
vif(lm.obj)

# Good values of VIF are between 1 and 10. But once you get > 10, those variables are suspect.
# Here we see TRB.x.y (difference in total rebounds by the team) yielding a 98.56 VIF => 
# get rid of it.

vif(lm(PTS.1 ~ .-TRB.x.y, data=diff_data))

# One sees how most VIFs got automatically reduced once this suspect variable got eliminated,
# pointing to the fact that it probably had heavy collinearities with multiple predictors.

# Now let's keep eliminating the predictors producing largest VIF values that end up above 10.

vif(lm(PTS.1 ~ .-TRB.x.y-`FG%.x.y`, data=diff_data))
vif(lm(PTS.1 ~ .-TRB.x.y-`FG%.x.y`-FGA.y.y, data=diff_data))

# Now all the remaining variables have VIF between 1 and 10 - the 'healthy' range.


### Stepwise Model Selection.

lm.obj <- lm(PTS.1 ~ .-TRB.x.y-`FG%.x.y`-FGA.y.y, data=diff_data)
summary(lm.obj)

lm.obj.step <- step(lm.obj, trace=F)
summary(lm.obj.step)





### Evaluating predictive performance.

# Get the predicted score differentials for each game.
head(predict(lm.obj.step))

pred.diff <- predict(lm.obj.step)
true.diff <- diff_data$PTS.1

# How far do they deviate from true values?
mean(abs((pred.diff - true.diff)))

# In order to look at first few games in particular:
pred.matrix <- cbind(True=true.diff,
                     Predicted=pred.diff)
head(pred.matrix)

# To look at random samples of games:
pred.matrix[sample(nrow(pred.matrix),10),]

plot(lm.obj.step, which=1)

## What if we focus on predicting won/lost? 
## Meaning: was it a positive or negative point differential?

mean(predict(lm.obj.step)*true.diff >= 0)

# Roughly 65% of correct sign identifications,  hence correct predictions of winner/loser.

# 65% might be underwhelming, but it will seem even worse once I say that 
# it is an OPTIMISTIC error estimate.
# We calculated it using the model's performance on the SAME DATA that the MODEL WAS TRAINED ON.


## Model Validation: Training/Testing subset.

# To get a more legitimate error estimate, one has to
# break data into 
#    - Training subset (data used to train the model), and 
#    - Testing (holdout) subset (data used to validate the model performance)

set.seed(3)

# Post-set.seed() code starts.
n <- nrow(diff_data)
train <- sample(n,0.8*n)
test <- c(1:n)[-train]

lm.obj.step
lm.obj.train <- lm(PTS.1 ~ `3PA.x.y` + BLK.y + TOV.x.y + PF.y + `3P%.y.y` + ORB.y.y + TRB.y.y, 
                   data=diff_data,
                   subset=train)
summary(lm.obj.train)

## Some performance metrics on the testing data:

test.true <- diff_data[test,'PTS.1']
test.predict <- predict(lm.obj.train,
                        newdata = diff_data[test,-1])

## Mean Absolute Error (MAE) rate on test data.
mean(abs(test.true - test.predict))

## Sheer predicted numbers vs true test numbers: first 20 predictions.
test.pred.mat <- cbind(True=test.true,
                       Predicted=test.predict)
head(test.pred.mat)

test.pred.mat[sample(nrow(test.pred.mat),10),]

## Proportion of times we predicted the sign correctly (positive/negative point differential)
mean(test.true*test.predict>0)


## RUN THE POST- set.seed(3)  PART OF THE CODE MULTIPLE TIMES, 
# NOTICE HOW THE OUTPUT CHANGES (the last line)




### FULL-ON CROSS VALIDATION

glm.obj <- glm(PTS.1 ~ `3PA.x.y` + BLK.y + TOV.x.y + PF.y + `3P%.y.y` + ORB.y.y + TRB.y.y,
               data=diff_data)
summary(glm.obj)



library(boot)

set.seed(1)

cv.obj <- cv.glm(data=diff_data, 
                 glm.obj, 
                 K=10,
                 cost = function(x,y) mean(x*y>0))
cv.obj$delta