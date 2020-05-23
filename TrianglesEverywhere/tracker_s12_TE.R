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
baseDir = "/Users/mf/Desktop/rpdr_fantasy_tracker/TrianglesEverywhere/"


##########
##Week 3##
##########
fileName = "1_TE_RPDR_S12_StartingSheet.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Sherry Pie", "Heidi N Closet", "Jackie Cox")
bottoms = c("Dahlia Sin", "Nicky Doll")
wk3Scores = calculateNewScores(currentData, currentScores, tops, bottoms)
# wk1Scores = c(wk1Scores, 2.75) #Jimi gets average for late join

darkcols <- brewer.pal(nrow(data), "Paired")
scoreTable = rbind(wk3Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-0.75, c(currentData[,2]), xpd=TRUE, srt=90)


# 
# 
# ##########
# ##Week 4##
# ##########
# 
fileName = "3_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Gigi Goode","Nicky Doll", "Jaida Essence Hall")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Rock M. Sakura", "Brita", "Aiden Zhane")

wk4Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-1.5, currentData[,2], xpd=TRUE, srt=90)
# 
# 
# ##########
# ##Week 5##
# ##########
# 
fileName = "4_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Sherry Pie","Widow Von'Du", "Jan", "Jackie Cox", "Gigi Goode", "Aiden Zhane")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Nicky Doll", "Heidi N Closet", "Brita")

wk5Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores, wk5Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-2, currentData[,2], xpd=TRUE, srt=90)
 
 
##########
##Week 6##
##########
 
fileName = "5_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Gigi Goode", "Sherry Pie", "Jackie Cox")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Aiden Zhane", "Brita", "Crystal Methyd")

wk6Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores, wk5Scores, wk6Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-3, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 7##
##########

fileName = "6_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Gigi Goode","Jan", "Crystal Methyd")
isThereATie = FALSE
howManyPeopleTied =1

bottoms = c("Brita", "Heidi N Closet", "Jackie Cox")

wk7Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk4Scores, wk5Scores, wk6Scores, wk7Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,20))
text(cex=1, x=x, y=-3.2, currentData[,2], xpd=TRUE, srt=90)

##########
##Week 8##
##########

fileName = "7_ep_TE_RPDR_S12 - 3_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Heidi N Closet", "Jackie Cox", "Crystal Methyd")
isThereATie = FALSE
isThereADoubleSave = TRUE
howManyPeopleTied =1

bottoms = c("Jan", "Widow Von'Du", "Gigi Goode", "Sherry Pie")

wk8Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,30))
text(cex=1, x=x, y=-4.2, currentData[,2], xpd=TRUE, srt=90)


##########
##Week 9##
##########

fileName = "8_ep_TE_RPDR_S12 - 3_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Jaida Essence Hall", "Heidi N Closet", "Crystal Methyd")
isThereATie = FALSE
isThereADoubleSave = FALSE
howManyPeopleTied =1

bottoms = c("Widow Von'Du", "Jackie Cox", "Sherry Pie")

wk9Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores,wk9Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,35))
text(cex=1, x=x, y=-4.9, currentData[,2], xpd=TRUE, srt=90)


###########
##Week 10##
###########

fileName = "9_ep_TE_RPDR_S12 - 3_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Jaida Essence Hall", "Sherry Pie", "Crystal Methyd")
isThereATie = FALSE
isThereADoubleSave = TRUE
howManyPeopleTied = 2

bottoms = c("Heidi N Closet", "Jackie Cox")

wk10Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,40))
text(cex=1, x=x, y=-5.7, currentData[,2], xpd=TRUE, srt=90)

###########
##Week 11##
###########

fileName = "9_ep_TE_RPDR_S12 - 3_ep_TE_RPDR_S12.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Crystal Methyd", "Jackie Cox")
isThereATie = FALSE
isThereADoubleSave = FALSE
howManyPeopleTied =1

bottoms = c("Heidi N Closet", "Jaida Essence Hall")

wk11Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)

colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
darkcols <- colorRampFunction(nrow(data)*2)
# table(wk1Scores,wk2Scores)
scoreTable = rbind(wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores, wk11Scores)
colnames(scoreTable) = currentData[,2]
x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,50))
text(cex=1, x=x, y=-7.2, currentData[,2], xpd=TRUE, srt=90)


# ###########
# ##Week 12##
# ###########
# 
# fileName = "12_s11_responses_sojuKahannaHoneyMercedesArielScarletRajahPlastiqueShugahNina_removed.csv"
# revisedFileName = sub('\\.csv','', fileName)
# data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)
# 
# currentData = data
# currentScores = c(rep(0,nrow(currentData)))
# tops = c("Yvie Oddly", "A'keria Chanel Davenport", "Silky Nutmeg Ganache")
# isThereATie = FALSE
# isThereADoubleSave = FALSE
# howManyPeopleTied =1
# 
# bottoms = c("Vanessa Vanjie Mateo", "Brooke Lynn Hytes")
# 
# wk12Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)
# 
# colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
# darkcols <- colorRampFunction(nrow(data)*2)
# # table(wk1Scores,wk2Scores)
# # TODO apply sum by column scoreTotal = sum(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores,wk11Scores, wk12Scores)
# scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores, wk11Scores, wk12Scores)
# colnames(scoreTable) = currentData[,2]
# x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,50))
# text(cex=1, x=x, y=-5.7, currentData[,2], xpd=TRUE, srt=90)
# 
# ######################
# ##Week 13 and finale##
# ######################
# 
# fileName = "13_s11_responses_sojuKahannaHoneyMercedesArielScarletRajahPlastiqueShugahNinaVanjie_removed.csv"
# revisedFileName = sub('\\.csv','', fileName)
# data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)
# 
# currentData = data
# currentScores = c(rep(0,nrow(currentData)))
# tops = c("Yvie Oddly")
# isThereATie = FALSE
# isThereADoubleSave = FALSE
# howManyPeopleTied =1
# 
# bottoms = c("Brooke Lynn Hytes", "A'keria Chanel Davenport", "Silky Nutmeg Ganache")
# 
# wk12Scores = calculateNewScores(currentData, currentScores, tops, bottoms, isThereATie, howManyPeopleTied, isThereADoubleSave)
# 
# colorRampFunction = colorRampPalette(brewer.pal(12, "Paired"))
# darkcols <- colorRampFunction(nrow(data)*2)
# # table(wk1Scores,wk2Scores)
# # TODO apply sum by column scoreTotal = sum(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores,wk11Scores, wk12Scores)
# scoreTable = rbind(wk1Scores, wk2Scores, wk3Scores, wk4Scores, wk5Scores, wk6Scores, wk7Scores, wk8Scores, wk9Scores, wk10Scores, wk11Scores, wk12Scores)
# colnames(scoreTable) = currentData[,2]
# x = barplot(scoreTable,ylab="Score",xaxt="n", las=2, col=darkcols, ylim=c(0,50))
# text(cex=1, x=x, y=-5.7, currentData[,2], xpd=TRUE, srt=90)
# 
# ##Dynamic Plots##
# # library(tidyverse)
# # library(janitor)
# # 
# # dynamicData <- read_csv(paste0(baseDir, fileName))
