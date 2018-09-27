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



unique(diff_data$Team)[1:10]
unique(diff_data$Opponent)[1:10]


library(stringdist)

ClosestMatch2 = function(string, stringVector){
  stringVector[amatch(string, stringVector, maxDist=Inf)]
}

ClosestMatch2("florida-state",unique(diff_data$Opponent) )

data.frame(Team=unique(diff_data$Team),
                 Match=ClosestMatch2(unique(diff_data$Team),unique(diff_data$Opponent)))



## miami-fl  => Citadel
## miami-oh  => Hampton
## texas-am  => Texas
## ucla => Iowa
## louisiana-lafayette => Lousiana State

diff_data$Team_Proper <- ClosestMatch2(diff_data$Team, diff_data$Opponent)
head(diff_data)

diff_data[diff_data$Team == "miami-fl",]$Team_Proper <- "Miami (FL)"
diff_data[diff_data$Team == "miami-oh",]$Team_Proper <- "Miami (OH)"
diff_data[diff_data$Team == "texas-am",]$Team_Proper <- "Texas A&M"
diff_data[diff_data$Team == "ucla",]$Team_Proper <- "California"
diff_data[diff_data$Team == "louisiana-lafayette",]$Team_Proper <- "Louisiana"

unique(diff_data$Opponent)[agrep("Louisiana", unique(diff_data$Opponent))]

diff_data$Opponent <- gsub("\\*","",diff_data$Opponent)


# Placeholder for indices of replicates.
baaaad.ind <- NULL

# This loop will keep track of replicate indices, and print them out as they appear.

for (j in 1:nrow(diff_data)){
  if (j%%20==0) print(j)
  log.vec <- apply(diff_data[1:(j-1),c("Opponent","Team_Proper")], 
                   1, 
                   function(x) all(x == diff_data[j,c("Team_Proper","Opponent")]))
  if (sum(log.vec)>0) {baaaad.ind <- c(baaaad.ind,j); print(j)}
}

length(baaaad.ind)

## The loop might take way too long, so this is the alternative:
#     - Download bad_indices.txt file from https://github.com/UsDAnDreS/Data-Science-in-Sports-Club/tree/master/Fall-2018
#     - Perform the following line:
#  baaaad.ind <- unlist(read.table("bad_indices.txt"))


#length(unlist(baaaad.ind))


dim(diff_data_num)
dim(diff_data_num[-baaaad.ind,])



## Fitting a linear model with hand-picked variables.

baaaad.ind <- c(read.table("bad_indices.txt"))

lm.obj <- lm(Points ~ Cmp + Pct + Avg + Avg.1 + No. + Fum + Int,
             data=diff_data_num)
summary(lm.obj)

lm.obj <- lm(Points ~ Cmp + Pct  + Avg + Avg.1 + No. + Fum + Int,
             data=diff_data_num,
             subset=-baaaad.ind)
summary(lm.obj)