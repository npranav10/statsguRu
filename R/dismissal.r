###################################################################################################################################
#' Gets a Players's Dismissal Summary (whilst Batting)
#'
#' This function scraps exclusive dismissal summary (as a batsman) info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns DataFrame containing complete Batting Dismissals Summary of a Player
#' @export
#' @examples
#' sachin = getDismissalSummary(35320,11)

getDismissalSummary = function(PlayerID,MatchType){
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting;;view=dismissal_summary",sep="");
  htmltab(url,which = 4,rm_nodata_rows = FALSE)
}
###################################################################################################################################
#' Splits Dismissal Summary data into respective categories
#'
#' This function takes in "Dismissal Summary" dataframe  and returns a list comprising data segregated into different dismissal categories.
#' @param dataframe Output of getDismissalSummary().
#' @return Returns a list comprising data segregated into different dismissal categories
#' @export
#' @examples
#' sachin = getDismissalSummary(35320,11)
#' sachin1 = splitDismissalSummary(sachin)

splitDismissalSummary = function(dataframe){
  k=1;j=1;a=c();
  for (i in 1:nrow(dataframe)) {
    if(is.na(dataframe[i,1]))
    {
    a=append(a,i)
    }
  }
  #dataframe
  bowlingHand=dataframe[1:a[1]-1,]
  bowlingStyle=dataframe[(a[1]+1):(a[2]-1),]
  bowlingHandStyleComb = dataframe[(a[2]+1):(a[3]-1),]
  battingPosition = dataframe[(a[3]+1):(a[4]-1),]
  runsInterval = dataframe[(a[4]+1):(a[5]-1),]
  dismissalType = dataframe[(a[5]+1):nrow(dataframe),]
  temp = list("bowlingHand"=bowlingHand,"bowlingStyle"=bowlingStyle,"bowlingHandStyleComb"=bowlingHandStyleComb,"battingPosition"=battingPosition,"runsInterval"=runsInterval,"dismissalType"=dismissalType)
  return(temp)
}

###################################################################################################################################

#' Plots a pie chart of Dismissal Summary vs type and style of Bowlers
#'
#' This function takes in "Dismissal Summary" dataframe after splitDismissalSummary  and Plots a pie chart of Dismissal Summary vs type and style of Bowlers.
#' @param data Output of splitDismissalSummary().
#' @return Plots a pie chart of Dismissal Summary vs type and style of Bowlers
#' @export
#' @examples
#' sachin = getDismissalSummary(35320,11)
#' sachin1 = splitDismissalSummary(sachin)
#' dispBatsmanDismissalsByBowlerType(sachin1)

dispBatsmanDismissalsByBowlerType =function(data)
{
  data= data$bowlingHandStyleComb
  x = as.numeric(data$Dis)
  l = round(100*x/sum(x), 1)
  l = paste(l,"%",sep = "")
  pie3D(x,labels = l,radius = 1,main="Distribution of Dismissals vs Bowlers",mar = c(1,1,4,10));
  legend("topright", as.vector(data$Grouping), cex = 1,fill = rainbow(length(data$Dis)),xpd = TRUE,inset=c(-0.55,-0.05));

}

###################################################################################################################################

#' Plots a pie chart of Dismissal Summary of a Player
#'
#' This function takes in "Dismissal Summary" dataframe after splitDismissalSummary  and plots the Dismissal Summary of a Player as a pie chart.
#' @param data Output of splitDismissalSummary().
#' @return Plots a pie chart of Dismissal Summary of a player.
#' @export
#' @examples
#' sachin = getDismissalSummary(35320,11)
#' sachin1 = splitDismissalSummary(sachin)
#' dispBatsmanDismissals(sachin1)

dispBatsmanDismissals =function(data)
{
  data= cbind(0,data$dismissalType)
  for(i in 1:nrow(data))
  {
    data$'0'[i] =sum(as.numeric(data[i,5:13]))
  }
  x = data$'0'
  l = round(100*x/sum(x), 1)
  l = paste(l,"%",sep = "")
  pie3D(x,labels = l,radius = 1,main="Distribution of Dismissals",mar = c(1,4,4,10));
  legend("topright", as.vector(data$Grouping), cex = 1,fill = rainbow(length(data$Dis)),xpd = TRUE,inset=c(-0.55,-0.05));

}
