###################################################################################################################################
#' Gets a Players's Overall Batting Summary
#'
#' This function scraps overall batting summary info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns dataframe containing overall Batting Summary of a Player
#' @export
#' @examples
#' sachin = getBattingSummary(35320,11)

getOverallBattingSummary = function(PlayerID,MatchType){
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting",sep="");
  htmltab(url,which = 3,rm_nodata_rows = FALSE)
}

###################################################################################################################################
#' Gets a Players's Batting Summary
#'
#' This function scraps batting summary info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns dataframe containing complete Batting Summary of a Player
#' @export
#' @examples
#' sachin = getBattingSummary(35320,11)

getBattingSummary = function(PlayerID,MatchType){
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting",sep="");
  htmltab(url,which = 4,rm_nodata_rows = FALSE)
}


###################################################################################################################################

#' Splits Batting Summary data into different categories
#'
#' This function takes in "Batting Summary" dataframe  and returns a list comprising data segregated into different categories that can inferred from a Players's Batting Summary.
#' @param data Output of getBattingSummary().
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns a list comprising data segregated into different categories of Batting Summary statistics
#' @export
#' @examples
#' sachin = getBattingSummary(35320,1)
#' sachin1 = splitBattingSummary(sachin,1)
#' dhoni = getBattingSummary(28081,2)
#' dhoni1 = splitBattingSummary(dhoni,2)
#' kohli = getBattingSummary(253802,3)
#' kohli1 = splitBattingSummary(kohli,3)

splitBattingSummary = function(data,MatchType){
  a=c();
  for (i in 1:nrow(data)) {
    if(is.na(data[i,1]))
    {
      a=append(a,i)
    }
  }
  if(MatchType==1 || MatchType==2 || MatchType==3)
  {
    if(substring(data[1,1], 1,1)=='v')
    {
      oppCountry=data[1:a[1]-1,]
      hostCountry=data[(a[1]+1):(a[2]-1),]
      hostContinent = data[(a[2]+1):(a[3]-1),]
      hostStatus = data[(a[3]+1):(a[4]-1),]
      yearWise = data[(a[4]+1):(a[5]-1),]
      #restdata = data[a[6]+1:nrow(data),]
      posPlayed = data[(tail(a,1)+1):nrow(data),]
    }
    else
    {
      oppCountry=data[(a[1]+1):(a[2]-1),]
      hostCountry=data[(a[2]+1):(a[3]-1),]
      hostContinent = data[(a[3]+1):(a[4]-1),]
      hostStatus = data[(a[4]+1):(a[5]-1),]
      yearWise = data[(a[5]+1):(a[6]-1),]
      #restdata = data[a[6]+1:nrow(data),]
      posPlayed = data[(tail(a,1)+1):nrow(data),]
    }
    temp = list("oppCountry"=oppCountry,"hostCountry"=hostCountry,"hostContinent"=hostContinent,
                "hostStatus"=hostStatus,"yearWise"=yearWise,"posPlayed"=posPlayed)
    return(temp)


  }
  else if(MatchType==11)
  {
    formats=data[1:a[1]-1,]
    if(substring(data[a[1]+1,1], 1,1)=='v')
    {
      oppCountry=data[(a[1]+1):(a[2]-1),]
      hostCountry = data[(a[2]+1):(a[3]-1),]
      hostContinent = data[(a[3]+1):(a[4]-1),]
      hostStatus = data[(a[4]+1):(a[5]-1),]
      yearWise = data[(a[5]+2):a[6]-1,]
      #restdata = data[a[6]+1:nrow(data),]
      posPlayed = data[(tail(a,1)+1):nrow(data),]
    }
    else
    {
      oppCountry=data[(a[2]+1):(a[3]-1),]
      hostCountry = data[(a[3]+1):(a[4]-1),]
      hostContinent = data[(a[4]+1):(a[5]-1),]
      hostStatus = data[(a[5]+1):(a[6]-1),]
      yearWise = data[(a[6]+2):a[7]-1,]
      #restdata = data[a[6]+1:nrow(data),]
      posPlayed = data[(tail(a,1)+1):nrow(data),]
    }
    temp = list("formats"=formats,"oppCountry"=oppCountry,"hostCountry"=hostCountry,"hostContinent"=hostContinent,
                "hostStatus"=hostStatus,"yearWise"=yearWise,"posPlayed"=posPlayed)
    return(temp)
  }

}


###################################################################################################################################



#' Displays Barplot of Players's Batting Average by List of Oppostion teams
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average against every opposition team.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting average against every opposition team.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,2)
#' sachin1 = splitBattingSummary(sachin,2)
#' dispBattingAveByOpposition(sachin1)

dispBattingAveByOpposition = function(data){

  data = data$oppCountry
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main="Batting Average vs Opp Country")
  }


###################################################################################################################################

#' Displays Barplot of Players's Batting Average by List of Host Country
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every host opposition team.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting average in Every host Country.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,11)
#' sachin1 = splitBattingSummary(sachin,11)
#' dispBattingAveByHostCountry(sachin1)

dispBattingAveByHostCountry = function(data){
  data = data$hostCountry
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main="Batting Average in Host Country")


  #abline(h=c$Ave,lwd=2, lty="dashed", col="red")
  #text(0.5, as.integer(c$Ave)+2, "Average", col = "black")
  }
###################################################################################################################################


# #' Displays Barplot of Players's Batting Average by List of Cricket Grounds Played in, by the corresponding Player
# #'
# #' This function takes in "Dismissal Summary" data modified after splitDismissalSummary and plots a player's batting average in every Cricket Ground Played in, by the corresponding Player.
# #' @param data Output of splitDismissalSummary.
# #' @return Plots a player's batting average in Every Cricket Ground.
# #' @export
# #' @examples
# #' dispBattingAveByGround(data)

# dispBattingAveByGround = function(data){
#   data = data$hostCountry
#   barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.7,col = rainbow(length(data$Ave)),main="Batting Average in Host Country")
# }

###################################################################################################################################


#' Displays Barplot of Players's Batting Average by List of Continents Played by the Player
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every contitent.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting average in every contient.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,1)
#' sachin1 = splitBattingSummary(sachin,1)
#' dispBattingAveByContinent(sachin1)

dispBattingAveByContinent = function(data){
  data = data$hostContinent
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main="Batting Average by Continent")
}


###################################################################################################################################

#' Displays Barplot of Players's Batting Average Year-wise
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's year-wise batting average.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's year-wise batting average.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,11)
#' sachin1 = splitBattingSummary(sachin,11)
#' dispBattingAveByYears(sachin1)

dispBattingAveByYears = function(data){
  data = data$yearWise
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main="Batting Average by Calendar Year")
}


###################################################################################################################################
#' Displays Barplot of Players's Batting according to the Player's Batting Position
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting position-wise batting average.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting position-wise batting average.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,2)
#' sachin1 = splitBattingSummary(sachin,2)
#' dispBattingAveByPosPlayed(sachin1)

dispBattingAveByPosPlayed = function(data){
  data = data$posPlayed
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main="Batting Average by Position Played")
}


###################################################################################################################################



#' Displays Scatterplot of a Players's Batting Average and Strike rate by List of Oppostion teams
#'
#' This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average and Strike Rate against every opposition team.
#' @param data Output of splitBattingSummary.
#' @return Plots a scatterplot pitting player's batting average and strike-rate against every opposition team.
#' @export
#' @examples
#' sachin = getBattingSummary(35320,2)
#' sachin1 = splitBattingSummary(sachin,2)
#' dispBattingAveSRByOpposition(sachin1)

dispBattingAveSRByOpposition = function(data){

  data = data$oppCountry
  data$Ave = as.numeric(data$Ave)
  data$SR = as.numeric(data$SR)
  data = na.omit(data)
  #with(plot(data$Ave~data$SR,data = data,xlim = c(min(data$SR)-10,max(data$SR)+10),
  #          ylim = c(min(data$Ave)-10,max(data$Ave)+10),pch = 19,xlab = "Strike-Rate",ylab = "Average",main = "Batting Average vs Strike-Rate"),
  #     text(data$SR,data$Ave, labels=data$Grouping, cex= 0.8,pos = 4))

  sp = ggplot(data = data, aes(x = data$Ave, y = data$SR)) + theme_bw() + geom_text_repel(aes(label = data$Grouping),
          box.padding = unit(0.5, "lines")) +geom_point(colour = "black", size = 3) +
    labs(x = "Batting Average",y= "Strike Rate",title = "Average vs Strike Rate") + theme(plot.title = element_text(hjust = 0.5))
  sp + geom_hline(yintercept=mean(data$SR),linetype="dashed", color = "blue") + geom_vline(xintercept=mean(data$Ave),linetype="dashed", color = "orange")


}
###################################################################################################################################
