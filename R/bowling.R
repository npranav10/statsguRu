###################################################################################################################################
#' Gets a Players's Bowling Summary
#'
#' This function scraps bowling summary info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns dataframe containing complete Batting Summary of a Player
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,11)

getBowlingSummary = function(PlayerID,MatchType){
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=bowling",sep="");
  htmltab(url,which = 4,rm_nodata_rows = FALSE)
}


###################################################################################################################################
#' Splits Bowling Summary data into different categories
#'
#' This function takes in "Bowling Summary" dataframe  and returns a list comprising data segregated into different categories that can inferred from a Players's Bowling Summary.
#' @param data Output of getBowlingSummary().
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns a list comprising data segregated into different categories of Bowling Summary statistics
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,1)
#' sachin1 = splitBowlingSummary(sachin,1)
#' dhoni = getBowlingSummary(28081,2)
#' dhoni1 = splitBowlingSummary(dhoni,2)
#' kohli = getBowlingSummary(253802,3)
#' kohli1 = splitBowlingSummary(kohli,3)

splitBowlingSummary = function(data,MatchType){
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
      bowlPosition = data[(tail(a,1)+1):nrow(data),]
    }
    else
    {
      oppCountry=data[(a[1]+1):(a[2]-1),]
      hostCountry=data[(a[2]+1):(a[3]-1),]
      hostContinent = data[(a[3]+1):(a[4]-1),]
      hostStatus = data[(a[4]+1):(a[5]-1),]
      yearWise = data[(a[5]+1):(a[6]-1),]
      #restdata = data[a[6]+1:nrow(data),]
      bowlPosition = data[(tail(a,1)+1):nrow(data),]
    }
    temp = list("oppCountry"=oppCountry,"hostCountry"=hostCountry,"hostContinent"=hostContinent,
                "hostStatus"=hostStatus,"yearWise"=yearWise,"bowlPosition"=bowlPosition)
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
      bowlPosition = data[(tail(a,1)+1):nrow(data),]
    }
    else
    {
      oppCountry=data[(a[2]+1):(a[3]-1),]
      hostCountry = data[(a[3]+1):(a[4]-1),]
      hostContinent = data[(a[4]+1):(a[5]-1),]
      hostStatus = data[(a[5]+1):(a[6]-1),]
      yearWise = data[(a[6]+2):a[7]-1,]
      #restdata = data[a[6]+1:nrow(data),]
      bowlPosition = data[(tail(a,1)+1):nrow(data),]
    }
    temp = list("formats"=formats,"oppCountry"=oppCountry,"hostCountry"=hostCountry,"hostContinent"=hostContinent,
                "hostStatus"=hostStatus,"yearWise"=yearWise,"bowlPosition"=bowlPosition)
    return(temp)
  }

}

###################################################################################################################################



#' Displays Barplot of Players's Wickets by List of Oppostion teams
#'
#' This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's sum of wickets taken against every opposition team.
#' @param data Output of splitBowlingSummary.
#' @return Plots a player's sum of wickets taken against every opposition team.
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,2)
#' sachin1 = getBowlingSummary(sachin,2)
#' dispWktsTakenByOpposition(sachin1)

dispWktsTakenByOpposition = function(data){

  data = data$oppCountry
  data$Wkts = as.numeric(data$Wkts)
  data = na.omit(data)
  data <- data[order(data$Wkts),]
  par(mar = c(2,8,4,1))
  bp = barplot(data$Wkts,names = as.vector(data$Grouping),las =2,cex.names = 1.1,col = rainbow(length(data$Wkts)),main="No. of Wkts vs Opposition",horiz = TRUE,axes = FALSE)
  text(0,bp, as.numeric(data$Wkts),cex=1,pos=4)
  }

###################################################################################################################################



#' Displays Barplot of Players's Wickets by List of Host Countries
#'
#' This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's sum of wickets taken against every opposition team.
#' @param data Output of splitBowlingSummary.
#' @return Plots a player's sum of wickets taken in every host country
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,2)
#' sachin1 = getBowlingSummary(sachin,2)
#' dispWktsTakenAtHostCountry(sachin1)

dispWktsTakenAtHostCountry = function(data){

  data = data$hostCountry
  data$Wkts = as.numeric(data$Wkts)
  data = na.omit(data)
  data <- data[order(data$Wkts),]
  par(mar = c(2,8,4,1))
  bp = barplot(data$Wkts,names = as.vector(data$Grouping),las =2,cex.names = 1.1,col = rainbow(length(data$Wkts)),main="Wkts Taken in a Host Country",horiz = TRUE,axes = FALSE)
  text(0,bp, as.numeric(data$Wkts),cex=1,pos=4)
}

###################################################################################################################################

#' Displays Barplot of Players's Year-wise Bowling Average
#'
#' This function takes in "Batting Summary" dataframe modified after splitBowlingSummary and plots a player's Year-wise Bowling Average.
#' @param data Output of splitBowlingSummary.
#' @return Plots a player's year-wise Bowling Average.
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,11)
#' sachin1 = splitBowlingSummary(sachin,11)
#' dispBowlingAveByYears(sachin1)

dispBowlingAveByYears = function(data){
  data = data$yearWise
  data$Grouping <- as.integer(gsub('[a-zA-Z]', '', data$Grouping))
  plot(data$Grouping,data$Ave, type="b", col="purple", lwd=5, pch=15, xlab="Year",ylab="Bowling Average", xaxt='n')
  axis(1,data$Grouping,data$Grouping)
  text(data$Ave,data$Grouping, cex=0.6, pos=4, col="red")
  title("Year Wise Bowling Average")

}

###################################################################################################################################

#' Displays Barplot of Players's Year-wise Bowling Economy Rate
#'
#' This function takes in "Batting Summary" dataframe modified after splitBowlingSummary and plots a player's Year-wise Bowling Economy Rate.
#' @param data Output of splitBowlingSummary.
#' @return Plots a player's year-wise Bowling Economy Rate
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,11)
#' sachin1 = splitBowlingSummary(sachin,11)
#' dispBowlingERByYears(sachin1)

dispBowlingERByYears = function(data){
  data = data$yearWise
  data$Grouping <- as.integer(gsub('[a-zA-Z]', '', data$Grouping))
  plot(data$Grouping,data$Econ, type="b", col="orange", lwd=5, pch=15, xlab="Year",ylab="Bowling Economy Rate", xaxt='n')
  axis(1,data$Grouping,data$Grouping)
  text(data$Econ,data$Grouping, cex=0.6, pos=4, col="red")
  title("Year Wise Bowling Economy Rate")

}

###################################################################################################################################



#' Displays Scatterplot of a Players's Bowling Average and Economy Rate by List of Oppostion teams
#'
#' This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's Bowling Average and Economy Rate against every opposition team.
#' @param data Output of splitBowlingSummary.
#' @return Plots a scatterplot pitting player's Bowling Average and Economy Rate against every opposition team.
#' @export
#' @examples
#' sachin = getBowlingSummary(35320,2)
#' sachin1 = splitBowlingSummary(sachin,2)
#' dispBowlingAveEconByOpposition(sachin1)

dispBowlingAveEconByOpposition = function(data){

  data = data$oppCountry
  data$Ave = as.numeric(data$Ave)
  data$Econ = as.numeric(data$Econ)
  data = na.omit(data)

  sp = ggplot(data = data, aes(x = data$Ave, y = data$Econ)) + theme_bw() + geom_text_repel(aes(label = data$Grouping),
                                                                                          box.padding = unit(0.5, "lines")) +geom_point(colour = "black", size = 3) +
    labs(x = "Bowling Average",y= "Economy Rate",title = "Average vs Economy Rate") + theme(plot.title = element_text(hjust = 0.5))
  sp + geom_hline(yintercept=mean(data$Econ),linetype="dashed", color = "blue") + geom_vline(xintercept=mean(data$Ave),linetype="dashed", color = "orange")


}
###################################################################################################################################
