library(rvest)  # Should be installed along with 'xml2'

# Link containing data of "Up until a particular date", we take "Up to January 18, 2018"
url_link <- 'https://www.basketball-reference.com/friv/standings.fcgi?month=1&day=18&year=2018&lg_id=NBA'

# Read the HTML webpage for that link into an R object
url <- read_html(url_link)
# Find a 'table' on that HTML page
table_one <- xml_find_all(url, "//table") 
# Obtain the actual contents of the table, wrapped into a data frame
table_one <- html_table(table_one)



length(table_one)

# That's not the tables we need.
head(table_one[[1]])
head(table_one[[2]])


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



url_link <- 'https://www.basketball-reference.com/friv/standings.fcgi?month=1&day=18&year=2018&lg_id=NBA'

###  Getting the needed tables.

all_stats <- find_extra_table(url_link)
team_stats <- all_stats[[1]]
opp_stats <- all_stats[[2]]

head(team_stats)
head(opp_stats)

###  Cleaning up memory.
rm(all_stats)
team_stats$Rk <- opp_stats$Rk <- NULL

# Get rid of league averages.

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


head(team_stats)
head(opp_stats)


### Get rid of "G" and "MP" columns.

team_stats$G <- opp_stats$G <- NULL
team_stats$MP <- opp_stats$MP <- NULL


### Variable "selection". Plotting correlation matrices, dropping redundant variables.

library(plotrix)

abs.cor <- abs(cor(team_stats[,-1]))
abs.thresh.cor <- ifelse(abs.cor >= 0.8, abs.cor, 0)

color2D.matplot(abs.thresh.cor,
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T,
                xlab='',
                ylab='',
                axes=F)

par(las=2)
axis(1,at=c(1:ncol(abs.thresh.cor))-0.5,labels=colnames(abs.thresh.cor))
par(las=1)
axis(2,at=c(ncol(abs.thresh.cor):1)-0.5,labels=colnames(abs.thresh.cor))


### Var "selection": gotta really minimize the selection, as we just got 30 teams.

needed.vars.team <- c("FGA","FG%","3PA","3P%", "ORB", "TRB", "AST", "BLK", "TOV","PF")

color2D.matplot(abs.thresh.cor[needed.vars.team,needed.vars.team],
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T,
                xlab='',
                ylab='',
                axes=F)

par(las=2)
axis(1,at=c(1:length(needed.vars.team))-0.5,labels=needed.vars.team)
par(las=1)
axis(2,at=c(length(needed.vars.team):1)-0.5,labels=needed.vars.team)




# For "Opponent Per Game" stats:

abs.cor <- abs(cor(opp_stats[,-1]))

abs.thresh.cor <- ifelse(abs.cor >= 0.8, abs.cor, 0)

color2D.matplot(abs.thresh.cor,
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T,
                xlab='',
                ylab='',
                axes=F)

par(las=2)
axis(1,at=c(1:ncol(abs.thresh.cor))-0.5,labels=colnames(abs.thresh.cor))
par(las=1)
axis(2,at=c(ncol(abs.thresh.cor):1)-0.5,labels=colnames(abs.thresh.cor))



# Var "selection": minimize the selection.

needed.vars.opp <- c("FGA","FG%","3PA", "3P%",  "ORB", "TRB", "AST", "TOV")

color2D.matplot(abs.thresh.cor[needed.vars.opp,needed.vars.opp],
                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
                show.legend=T,
                xlab='',
                ylab='',
                axes=F)

par(las=2)
axis(1,at=c(1:length(needed.vars.opp))-0.5,labels=needed.vars.opp)
par(las=1)
axis(2,at=c(length(needed.vars.opp):1)-0.5,labels=needed.vars.opp)




## MERGING data frames:

all_stats <- merge(team_stats[,c("Team",needed.vars.team)], 
                   opp_stats[,c("Team",needed.vars.opp)], 
                   by = "Team")
head(all_stats)

# For further convenience, make the Team column to be ROW NAMES,
# while disposing of the column itself. This way we will remain
# with only numeric columns => perfect for clustering.

rownames(all_stats) <- all_stats[,1]
all_stats$Team <- NULL



### CLUSTERING

## Scaling the data.

# Clustering is pretty sensitive to the variable scales, and 
# can be easily dominated by variables that have higher standard deviations,
# due to units of measurement.
#
# E.g. difference of 1 FGA per game isn't comparable to difference of 0.1 in FG%,
# but from geometrical perspective: the difference of 1 FGA would be larger than 0.1 for FG%.


all_stats <- scale(all_stats, center=F, scale=T)
head(all_stats)

## HIERARCHICAL CLUSTERING.

# Calculate distance matrix, apply hclust() function.

team.dist <- dist(all_stats)
team.hc <- hclust(team.dist, method = "complete")

# Plot the resulted hierarchy of clusters.
plot(team.hc,
     hang = -1)


## To make the dendrogram  more readable: some half-voodoo magic needed.

library(graphics)

default.margins <- par()$mar

par(mar=c(3,1,1,10)) 
plot(as.dendrogram(team.hc), 
     horiz=T)
par(mar=default.margins) 


# Get the teams names from the bottom to the top of the dendrogram.
ord.names <- rownames(all_stats)[team.hc$order]
ord.names

# Function to calculate average ranks of teams with respect to a stat.
ranking.calc <- function(stat,team){
  if (!is.numeric(stat)) print(stat)
  if (is.numeric(stat)) print(colnames(all_stats)[stat])
  return(mean(which(names(sort(all_stats[,stat],decreasing = T)) %in% team)))
  
}



# 2:4 - high 3PA, not great rim-protecting defense
# 5:13 - great rebounding teams, great 3P% defense
for (j in colnames(all_stats)){
  print(ranking.calc(j,ord.names[5:13]))
}




## K-Means Approach.

kmeans(all_stats, centers=5, nstart=50,iter.max = 100)
kmeans(all_stats, centers=10, nstart=50,iter.max = 100)


# Choosing K - elbow method.

k.max <- 29
wss <- sapply(1:k.max, 
              function(k){kmeans(all_stats, k, nstart=50,iter.max = 100)$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

lines(c(1,k.max),c(wss[1],wss[k.max]))


# 6-7-8 appear "elbow-ish"

k.opt <- 7
kmeans.res <- kmeans(all_stats, k.opt, nstart=50, iter.max = 100)
kmeans.res$cluster


# Listing resulting clusters by team names.

for (k in 1:k.opt){
  print(k)
  print(rownames(all_stats)[which(kmeans.res$cluster==k)])
}



# Specify which cluster you're interested in.
k.int <- 4
teams.int <- rownames(all_stats)[which(kmeans.res$cluster==k.int)]
print(teams.int)
for (j in colnames(all_stats)){
  print(ranking.calc(j,teams.int))
}








