##Usage##
# bottoms should start with the loser(s)
# tops should start with the winnter(s)
# tables fed in should NOT contain blank columns with just titles anywhere!

calculateNewScores = function(currentData, currentScores, tops, bottoms, isThereATie=FALSE, howManyPeopleTied=1, isThereADoubleSave = FALSE){
  
  if(isThereADoubleSave==TRUE){
    #only award one point for the bottom person being in the bottom but not getting eliminated
    predictedLosers = currentData[,ncol(currentData)]
    currentScores [which(predictedLosers == bottoms[1])] = currentScores [which(predictedLosers == bottoms[1])] + 1
  } else{
    ##Award full points for loser
    predictedLosers = currentData[,ncol(currentData)]
    currentScores [which(predictedLosers == bottoms[1])] = currentScores [which(predictedLosers == bottoms[1])] + 3  
  }
  
  
  
  ##Award points for winner
  predictedEpisodeWinners = currentData[,3]
  if(isThereATie==TRUE){
    tracker = c(rep(0,nrow(currentData)))
    for(i in 1:howManyPeopleTied){
      for(j in 3:(3+howManyPeopleTied-1)){
        which(tops[i] ==  currentData[,j])
        tracker[which(tops[i] ==  currentData[,j])] = tracker[which(tops[i] ==  currentData[,j])]+1
      }
    }
    currentScores [which(tracker >=1)] = currentScores [which(tracker >=1)] + 2
  }else{
    currentScores [which(predictedEpisodeWinners == tops[1])] = currentScores [which(predictedEpisodeWinners == tops[1])] + 2  
  }
  
  ##Now look for predicted losers in the bottom two but not the eliminated queen
  if(isThereADoubleSave==TRUE){
    #Use the true bottom 2 (including both non-eliminated queens) to award points
    currentScores [which(predictedLosers %in% bottoms[1:length(bottoms)-1])] = currentScores [which(predictedLosers %in% bottoms[1:length(bottoms)-1])] + 1  
    for(i in 2:length(bottoms)-1){
      bottomCandidate = currentData[,ncol(currentData)-(i-1)]
      currentScores [which(bottomCandidate %in% bottoms[1:length(bottoms)-1])] = currentScores [which(bottomCandidate %in% bottoms[1:length(bottoms)-1])] + 1
    }
  } else{
    currentScores [which(predictedLosers %in% bottoms[2:length(bottoms)])] = currentScores [which(predictedLosers %in% bottoms[2:length(bottoms)])] + 1  
    for(i in 2:length(bottoms)){
      bottomCandidate = currentData[,ncol(currentData)-(i-1)]
      currentScores [which(bottomCandidate %in% bottoms)] = currentScores [which(bottomCandidate %in% bottoms)] + 1
    }
  }
  
  #predictedLipSyncWinner = currentData[,ncol(currentData)-1]
  #currentScores [which(predictedLipSyncWinner %in% bottoms[1:length(bottoms)])] = currentScores [which(predictedLipSyncWinner %in% bottoms[2:length(bottoms)])] + 1
  
  
  
  ##Now look for predicted winners in the top but not the winner
  currentScores [which(predictedEpisodeWinners %in% tops[2:length(tops)])] = currentScores [which(predictedEpisodeWinners %in% tops[2:length(tops)])] + 1
  # TODO check whether above line is necessary
  for(i in 2:length(tops)){
    topCandidate = currentData[,2+i]
    currentScores [which(topCandidate %in% tops)] = currentScores [which(topCandidate %in% tops)] + 1
  }
  return(currentScores)
}

################
##Read Data In##
################

require(data.table)
require(stringr)
require(RColorBrewer)
baseDir = "/Users/mf/Desktop/rpdr_fantasy_score_tracker/"


##########
##Week 1##
##########
fileName = "1_s11_responses.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Brooke Lynn Hytes", "A'keria Chanel Davenport", "Plastique Tiara", "Vanessa Vanjie Mateo")
bottoms = c("Soju", "Kahanna Montrese")
wk1Scores = calculateNewScores(currentData, currentScores, tops, bottoms)
wk1Scores = c(wk1Scores, 2.75) #Jimi gets average for late join

darkcols <- brewer.pal(nrow(data), "Paired")
x = barplot(newScores,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-0.8, c(currentData[,2],"Jimi"), xpd=TRUE, srt=90)

##########
##Week 2##
##########

fileName = "2_s11_responses_soju_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData))) #wk1Scores
tops = c("Yvie Oddly","Scarlet Envy", "Plastique Tiara", "Shuga Cain")
isThereATie = TRUE
howManyPeopleTied =2
#topsAlt = c("Scarlet Envy","Yvie Oddly", "Plastique Tiara", "Shuga Cain")
bottoms = c("Kahanna Montrese", "Mercedes Iman Diamond")
#wk2Scoresalt = calculateNewScores(currentData, currentScores, topsAlt, bottoms)
wk2Scores = calculateNewScores(currentData, currentScores, tops, bottoms, TRUE, 2)
wk2Scores = c(wk2Scores,1.583333)

darkcols <- brewer.pal(nrow(data)*2, "Paired")
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-1.3, c(currentData[,2],"Jimi"), xpd=TRUE, srt=90)


##########
##Week 3##
##########

fileName = "3_s11_responses_sojuKahanna_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData))) #wk1Scores
tops = c("Nina West","Vanessa Vanjie Mateo", "Ariel Versace")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Honey Davenport", "Shuga Cain", "A'keria Chanel Davenport", "Scarlet Envy", "Ra'jah O'Hara", "Plastique Tiara")

wk3Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-1.3, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 4##
##########

fileName = "4_s11_responses_sojuKahannaHoney_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Silky Nutmeg Ganache","Brooke Lynn Hytes", "Yvie Oddly")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Mercedes Iman Diamond", "Ra'jah O'Hara", "Vanessa Vanjie Mateo")

wk4Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-1.5, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 5##
##########

fileName = "5_s11_responses_sojuKahannaHoneyMercedes_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("A'keria Chanel Davenport","Brooke Lynn Hytes", "Yvie Oddly", "Silky Nutmeg Ganache", "Shuga Cain")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Scarlet Envy", "Ra'jah O'Hara", "Nina West")

wk5Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-2, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 6##
##########

fileName = "6_s11_responses_sojuKahannaHoneyMercedesAriel_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Brooke Lynn Hytes","Plastique Tiara", "Yvie Oddly")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Ariel Versace", "Shuga Cain", "Silky Nutmeg Ganache")

wk6Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-1.7, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 7##
##########

fileName = "7_s11_responses_sojuKahannaHoneyMercedesArielScarlet_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Plastique Tiara","Brooke Lynn Hytes", "Yvie Oddly")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Ra'jah O'Hara", "A'keria Chanel Davenport", "Nina West")

wk7Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,20))
text(cex=1, x=x, y=-2.2, currentData[,2], xpd=TRUE, srt=90)

##########
##Week 8##
##########

fileName = "8_s11_responses_sojuKahannaHoneyMercedesArielScarletRajah_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Silky Nutmeg Ganache", "Shuga Cain", "Nina West")
isThereATie = FALSE
isThereADoubleSave = TRUE
howManyPeopleTied =1

bottoms = c("Brooke Lynn Hytes", "Yvie Oddly", "Vanessa Vanjie Mateo")

wk8Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,30))
text(cex=1, x=x, y=-3.2, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 9##
##########

fileName = "9_s11_responses_sojuKahannaHoneyMercedesArielScarletRajah_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("A'keria Chanel Davenport", "Brooke Lynn Hytes", "Silky Nutmeg Ganache")
isThereATie = FALSE
isThereADoubleSave = FALSE
howManyPeopleTied =1

bottoms = c("Plastique Tiara", "Vanessa Vanjie Mateo", "Shuga Cain")

wk9Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores,wk9Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,35))
text(cex=1, x=x, y=-3.7, currentData[,2], xpd=TRUE, srt=90)


###########
##Week 10##
###########

fileName = "10_s11_responses_sojuKahannaHoneyMercedesArielScarletRajahPlastique_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Nina West", "Brooke Lynn Hytes")
isThereATie = FALSE
isThereADoubleSave = FALSE
howManyPeopleTied =1

bottoms = c("Shuga Cain", "Vanessa Vanjie Mateo")

wk10Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,40))
text(cex=1, x=x, y=-4.3, currentData[,2], xpd=TRUE, srt=90)

##Dynamic Plots##
# library(tidyverse)
# library(janitor)
# 
# dynamicData <- read_csv(paste0(baseDir, fileName))
