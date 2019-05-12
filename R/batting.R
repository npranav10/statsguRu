###################################################################################################################################
#' Gets a Players's Batting Summary
#'
#' This function scraps batting summary info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns DataFrame containing complete Batting Summary of a Player
#' @export
#' @examples
#' getBattingSummary(35320,11)

getBattingSummary = function(PlayerID,MatchType){
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting",sep="");
  htmltab(url,which = 4,rm_nodata_rows = FALSE)
}


###################################################################################################################################

#' Splits Batting Summary data into different categories
#'
#' This function takes in "Batting Summary" dataframe  and returns a list comprising data segregated into different categories that can inferred from a Players's Batting Summary.
#' @param dataframe Output of getBattingSummary().
#' @return Returns a list comprising data segregated into different categories of Batting Summary statistics
#' @export
#' @examples
#' splitBattingSummary(dataframe)

splitBattingSummary = function(dataframe){
  k=1;a=c();
  for (i in 1:nrow(dataframe)) {
    if(is.na(dataframe[i,1]))
    {
      a=append(a,i)
    }
  }
  #dataframe
  formats=dataframe[1:a[1]-1,]
  if(substring(dataframe[a[1]+1,1], 1,1)=='v')
  {
  oppCountry=dataframe[(a[1]+1):(a[2]-1),]
  hostCountry = dataframe[(a[2]+1):(a[3]-1),]
  hostContinent = dataframe[(a[3]+1):(a[4]-1),]
  hostStatus = dataframe[(a[4]+1):(a[5]-1),]
  yearWise = dataframe[(a[5]+2):a[6]-1,]
  #restdata = dataframe[a[6]+1:nrow(dataframe),]
  posPlayed = dataframe[(tail(a,1)+1):nrow(dataframe),]
  }
  else
  {
    oppCountry=dataframe[(a[2]+1):(a[3]-1),]
    hostCountry = dataframe[(a[3]+1):(a[4]-1),]
    hostContinent = dataframe[(a[4]+1):(a[5]-1),]
    hostStatus = dataframe[(a[5]+1):(a[6]-1),]
    yearWise = dataframe[(a[6]+2):a[7]-1,]
    #restdata = dataframe[a[6]+1:nrow(dataframe),]
    posPlayed = dataframe[(tail(a,1)+1):nrow(dataframe),]
  }
  temp = list("formats"=formats,"oppCountry"=oppCountry,"hostCountry"=hostCountry,"hostContinent"=hostContinent,
              "hostStatus"=hostStatus,"yearWise"=yearWise,"posPlayed"=posPlayed)
  return(temp)
}


###################################################################################################################################



#' Displays Barplot of Players's Batting Average by List of Oppostion teams
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average against every opposition team.
#' @param dataframe Output of splitBattingSummary.
#' @return Plots a player's batting average against every opposition team.
#' @export
#' @examples
#' dispBattingAveByOpposition(dataframe)

dispBattingAveByOpposition = function(data){

  data = data$oppCountry
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average vs Opp Country")
  }


###################################################################################################################################

#' Displays Barplot of Players's Batting Average by List of Host Country
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every host opposition team.
#' @param dataframe Output of splitBattingSummary.
#' @return Plots a player's batting average in Every host Country.
#' @export
#' @examples
#' dispBattingAveByCountry(dataframe)

dispBattingAveByHostCountry = function(data){
  data = data$hostCountry
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average in Host Country")
}
###################################################################################################################################


# #' Displays Barplot of Players's Batting Average by List of Cricket Grounds Played in, by the corresponding Player
# #'
# #' This function takes in "Dismissal Summary" dataframe modified after splitDismissalSummary and plots a player's batting average in every Cricket Ground Played in, by the corresponding Player.
# #' @param dataframe Output of splitDismissalSummary.
# #' @return Plots a player's batting average in Every Cricket Ground.
# #' @export
# #' @examples
# #' dispBattingAveByGround(dataframe)

# dispBattingAveByGround = function(data){
#   data = data$hostCountry
#   barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average in Host Country")
# }

###################################################################################################################################


#' Displays Barplot of Players's Batting Average by List of Continents Played by the Player
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every contitent.
#' @param dataframe Output of splitBattingSummary.
#' @return Plots a player's batting average in every contient.
#' @export
#' @examples
#' dispBattingAveByContinent(dataframe)

dispBattingAveByContinent = function(data){
  data = data$hostContinent
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average by Continent")
}


###################################################################################################################################

#' Displays Barplot of Players's Batting Average Year-wise
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's year-wise batting average.
#' @param dataframe Output of splitBattingSummary.
#' @return Plots a player's year-wise batting average.
#' @export
#' @examples
#' dispBattingAveByContinent(dataframe)

dispBattingAveByYears = function(data){
  data = data$yearWise
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average by Calendar Year")
}


###################################################################################################################################
#' Displays Barplot of Players's Batting according to the Player's Batting Position
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting position-wise batting average.
#' @param dataframe Output of splitBattingSummary.
#' @return Plots a player's batting position-wise batting average.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,11)
#' sachin1 = splitBattingSummary(sachin1)
#' dispBattingAveByPosPlayed(sachin1)

dispBattingAveByPosPlayed = function(data){
  data = data$posPlayed
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average by Position Played")
}


###################################################################################################################################

