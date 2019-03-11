calculateNewScores = function(currentData, currentScores, tops, bottoms){
  
  ##Award points for loser
  predictedLosers = currentData[,ncol(currentData)]
  currentScores [which(predictedLosers == bottoms[1])] = currentScores [which(predictedLosers == bottoms[1])] + 3
  
  ##Award points for winner
  predictedEpisodeWinners = currentData[,3]
  currentScores [which(predictedEpisodeWinners == tops[1])] = currentScores [which(predictedEpisodeWinners == tops[1])] + 2
  
  ##Now look for predicted losers in the bottom two but not the eliminated queen
  currentScores [which(predictedLosers %in% bottoms[2:length(bottoms)])] = currentScores [which(predictedLosers %in% bottoms[2:length(bottoms)])] + 1
  #predictedLipSyncWinner = currentData[,ncol(currentData)-1]
  #currentScores [which(predictedLipSyncWinner %in% bottoms[1:length(bottoms)])] = currentScores [which(predictedLipSyncWinner %in% bottoms[2:length(bottoms)])] + 1
  for(i in 2:length(bottoms)){
    bottomCandidate = currentData[,ncol(currentData)-(i-1)]
    currentScores [which(bottomCandidate %in% bottoms)] = currentScores [which(bottomCandidate %in% bottoms)] + 1
  }
  
  
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
darkcols <- brewer.pal(nrow(data), "Paired")

##########
##Week 1##
##########
fileName = "s11_responses.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData)))
tops = c("Brooke Lynn Hytes", "A'keria Chanel Davenport", "Plastique Tiara", "Vanessa Vanjie Mateo")
bottoms = c("Soju", "Kahanna Montrese")
wk1Scores = calculateNewScores(currentData, currentScores, tops, bottoms)

x = barplot(newScores,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-0.8, currentData[,2], xpd=TRUE, srt=90)

##########
##Week 2##
##########

fileName = "s11_responses_soju_removed.csv"
revisedFileName = sub('\\.csv','', fileName)
data = read.csv(file = paste0(baseDir, fileName), header=T, sep=",", stringsAsFactors = FALSE)

currentData = data
currentScores = c(rep(0,nrow(currentData))) #wk1Scores
tops = c("Yvie Oddly","Scarlet Envy", "Plastique Tiara", "Shuga Cain")
#topsAlt = c("Scarlet Envy","Yvie Oddly", "Plastique Tiara", "Shuga Cain")
bottoms = c("Kahanna Montrese", "Mercedes Iman Diamond")
#wk2Scoresalt = calculateNewScores(currentData, currentScores, topsAlt, bottoms)
wk2Scores = calculateNewScores(currentData, currentScores, tops, bottoms)

x = barplot(wk1Scores+wk2Scores,ylab="Score",xaxt="n", las=2, col=darkcols)
text(cex=1, x=x, y=-0.8, currentData[,2], xpd=TRUE, srt=90)
