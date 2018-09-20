# More AUTOMATED WAY of dealing with CORRELATION between predictors,
# or also known as a COLLINEARITY ISSUE: 
# is Variance Inflation Factor (VIF).

# VIF is a metric of a predictor, such that:
#     - it takes on values larger or equal to 1, and
#     - the larger the VIF value => the more collinearity predictor introduces.

# Stable, or "normal", values of VIF are between 1 and 10. 
# Once we get over 10 - we might have a collinearity issue.

library(rvest)  # Should be installed along with 'xml2'
library(plotrix) # Also install it please.
library(stringi) # Needed for certain string operations.


###########
## SCRAPING to the VERY END.
###########

## We've fit some linear models to our 62-game data of 5 teams
## But chances are - those 5 teams might not be representative of the whole league.
## Let's try to get the data for ALL the teams.

# First order of business - getting a vector of all team names. 
# We'll scrape it off of one 'Season Summary' table on sports-reference.

url_link <- 'https://www.sports-reference.com/cfb/years/2017-team-defense.html'

url <- read_html(url_link)

total_table <- html_table(url)[[1]]
head(total_table)
class(total_table)

# Pretty it up:

## Making 2nd row the HEADER
c.names <- total_table[1,]
total_table <- total_table[-1,]
colnames(total_table) <- c.names

# All we really need from this table is school names, hence the School column. 
total_table$School
# See an issue? Where are those occasional "" and "School" values coming from? Check the whole table:
total_table

# That can be easily cleaned for the whole table as follows:

total_table$School %in% c("","School")
!(total_table$School %in% c("","School"))

good.ind <- !(total_table$School %in% c("","School"))
total_table_clean <- total_table[good.ind,]
total_table_clean$School


## Next, recall the format of school names in the url links: 
#      - all lowercase,
#      - spaces are subbed by "-".
low.names <- tolower(total_table_clean$School)
url.names <- gsub(" ","-",low.names)

# One other aspect is that there shouldn't be any "(" ")" in the name like in miami(oh) or miami(fl),
# hence just replace both "(" and ")" with "". 

# When using either "(" or ")" in a regular expression, keep in mind that those are consider special characters,
# and need to be pre-fixed with escape sequence "\\":

url.names <- gsub("\\(","",url.names)
url.names <- gsub("\\)","",url.names)

print(url.names)

# Next, I will run my two-table scraping procedure for all those names, BUT
# accounting for the fact that there will still be some incorrect url names among the ones we've calculated.

# I will keep the indices of "bad" names in object "bad.ind". Those names will reveal themselves once you witness
# an empty scraped table (length(table_one) == 0), at which point we 
#      - record the index (bad.ind <- c(bad.ind, j)), and 
#      - proceed to the next inde (next).

bad.ind <- NULL
for (j in 1:length(low.names)){
  url <- paste('https://www.sports-reference.com/cfb/schools/',url.names[j],'/2017/gamelog/', sep="")
  print(url)
  print("")
  table_one <- xml_find_all(read_html(url), "//table") %>% html_table
  if (length(table_one) == 0) {bad.ind <- c(bad.ind,j); next}
  other_tables <- find_extra_table(url)
  print(c(table_one,other_tables))
  print("")
}


## Now, what were the bad names?
url.names[bad.ind]


## Deal with bad names on INDIVIDUAL BASIS
url.names[bad.ind] <- c("texas-san-antonio","louisiana-state","central-florida",
                        "alabama-birmingham","southern-california","pittsburgh",
                        "texas-am","nevada-las-vegas","mississippi","southern-methodist",
                        "texas-el-paso","louisiana-lafayette")
  


for (j in bad.ind){
  url <- paste('https://www.sports-reference.com/cfb/schools/',url.names[j],'/2017/gamelog/', sep="")
  print(url)
  print("")
  table_one <- xml_find_all(read_html(url), "//table") %>% html_table
  if (length(table_one) == 0) bad.ind <- c(bad.ind,j)
  other_tables <- find_extra_table(url)
  print(c(table_one,other_tables))
  print("")
}



offense_data <- defense_data <- NULL

for (i in 1:length(url.names)){
  url_link <- paste('https://www.sports-reference.com/cfb/schools/',url.names[i],'/2017/gamelog/',
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
  table_one[,1] <- url.names[i]
  colnames(table_one)[1] <- "Team"
  
  print(head(table_one,2)) # Print out extracted table for the team.
  cat("\n")
  offense_data <- rbind(offense_data, table_one)
  
  
  url_link <- paste('https://www.sports-reference.com/cfb/schools/',url.names[i],'/2017/gamelog/',
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
  table_two[,1] <- url.names[i]
  colnames(table_two)[1] <- "Team"
  
  defense_data <- rbind(defense_data, table_two)

}

dim(offense_data)
dim(defense_data)



# Save the scraped defensive logs data.
write.table(offense_data,"Scraped_Offense_ALL_TEAMS.txt")

# Save the scraped defensive logs data.
write.table(defense_data,"Scraped_Defense_ALL_TEAMS.txt")




# Re-read the data back, just to make sure that it was saved in the files.
offense_data <- read.table("Scraped_Offense_ALL_TEAMS.txt")
defense_data <- read.table("Scraped_Defense_ALL_TEAMS.txt")




# Create the "points scored" and "points allowed" column.
Points <- sapply(as.character(offense_data$Result),
                 function(x){ 
                   spl <- strsplit(as.character(x), split=" ");
                   score <- spl[[1]][2]
                   score.no.parenth <- substr(score,2,nchar(score)-1)
                   points.scored <- strsplit(score.no.parenth,split="-")[[1]][1]
                   points.allowed <- strsplit(score.no.parenth,split="-")[[1]][2]
                   return(c(points.scored=as.numeric(points.scored),
                            points.allowed=as.numeric(points.allowed)))
                 }
)



offense_data$Points <- Points[1,]
defense_data$Points <- Points[2,]


head(offense_data)


## Create the matrix of differences (Offense - Defense)
diff_data <- offense_data
diff_data[,-c(1:5)] <- offense_data[,-c(1:5)] - defense_data[,-c(1:5)]
diff_data_num <- diff_data[,-c(1:5)]

  
  
# Take matrix of absolute correlations:
abs.cor.mat <- abs(cor(diff_data_num))
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



## Fitting a linear model with hand-picked variables.

lm.obj <- lm(Points ~ Cmp + Pct + Att.1 + Avg + Avg.1 + No. + Fum + Int,
             data=diff_data_num)

# We see that league-wide, interceptions turn out to be PIVOTAL, 
# even more important than fumbles (which are still significant, though).

# The yards per play (Avg.1) and rushing attempts (Att.1) remain important too.

vif(lm.obj)
summary(lm.obj)