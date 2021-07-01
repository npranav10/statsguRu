#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Gets a Players's Bowling Summary
#' @keywords Bowling
#' @description This function scraps bowling summary info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns dataframe containing complete Batting Summary of a Player
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' print(ashwin_BowlingSummary)

get_Summary_Bowling = function(PlayerID, MatchType){
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=bowling",sep="");
  data = htmltab::htmltab(url,which = 4,rm_nodata_rows = FALSE)
  return(data)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Splits Bowling Summary data into different categories
#' @keywords Bowling
#' @description This function takes in "Bowling Summary" dataframe  and returns a list comprising data segregated into different categories that can inferred from a Players's Bowling Summary.
#' @param data Output of statsguRu::get_Summary_Bowling().
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns a list comprising data segregated into different categories of Bowling Summary statistics
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)

split_Summary_Bowling = function(data, MatchType){
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
    temp = list("oppCountry" = oppCountry,
                "hostCountry" = hostCountry,
                "hostContinent" = hostContinent,
                "hostStatus" = hostStatus,
                "yearWise" = yearWise,
                "bowlPosition" = bowlPosition)
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
    temp = list("formats" = formats,
                "oppCountry" = oppCountry,
                "hostCountry" = hostCountry,
                "hostContinent" = hostContinent,
                "hostStatus" = hostStatus,
                "yearWise" = yearWise,
                "bowlPosition" = bowlPosition)
    return(temp)
  }

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Wickets by List of Oppostion teams
#' @keywords Bowling
#' @description This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's sum of wickets taken against every opposition team.
#' @param data Output of statsguRu::split_Summary_Bowling()
#' @return Plots a player's sum of wickets taken against every opposition team.
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' ashwin_BowlingSummary = statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)
#' statsguRu::dispWktsTakenByOpposition(data = ashwin_BowlingSummary)

dispWktsTakenByOpposition = function(data, plot_title = "Wickets taken against a Country"){

  data = data$oppCountry
  data$Wkts = as.numeric(data$Wkts)
  data = na.omit(data)
  data <- data[order(data$Wkts),]
  # par(mar = c(2,8,4,1))
  # bp = barplot(data$Wkts,names = as.vector(data$Grouping),las =2,cex.names = 1.1,col = rainbow(length(data$Wkts)),main="No. of Wkts vs Opposition",horiz = TRUE,axes = FALSE)
  # text(0,bp, as.numeric(data$Wkts),cex=1,pos=4)
  
  team_colors = statsguRu::getTeamColors()
  
  data$Grouping = gsub(pattern = "v ",replacement = "",x = data$Grouping)
  ggplot(data, aes(Wkts, reorder(Grouping, Wkts))) +
    geom_col(aes(fill = Grouping), color = "black" ,size = 0.15) + 
    scale_fill_manual("Result", values = team_colors)  +
    scale_color_manual("col", values = sapply(team_colors, calc_ContrastColor))+
    geom_text(aes(label = round(Wkts,1), color = Grouping),fontface = "bold", hjust = 1.1, 
              size = 7) + 
    ggtitle(label = plot_title) +
    guides(fill = F, color = F) + 
    theme(rect = element_blank(), line = element_blank(),
          axis.text.y = element_text(hjust = 1,size = 16, colour = "black",
                                     margin = margin(0,0,0,40, unit = "pt")),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(r = 10,unit = "pt"),
          plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0))
  
  }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Wickets by List of Host Countries
#' @keywords Bowling
#' @description This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's sum of wickets taken against every opposition team.
#' @param data Output of statsguRu::split_Summary_Bowling()
#' @return Plots a player's sum of wickets taken in every host country
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' ashwin_BowlingSummary = statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)
#' statsguRu::dispWktsTakenAtHostCountry(data = ashwin_BowlingSummary)

dispWktsTakenAtHostCountry = function(data, plot_title = "Wickets Taken in a Country"){

  data = data$hostCountry
  data$Wkts = as.numeric(data$Wkts)
  data = na.omit(data)
  data <- data[order(data$Wkts),]
  # par(mar = c(2,8,4,1))
  # bp = barplot(data$Wkts,names = as.vector(data$Grouping),las =2,cex.names = 1.1,col = rainbow(length(data$Wkts)),main="Wkts Taken in a Host Country",horiz = TRUE,axes = FALSE)
  # text(0,bp, as.numeric(data$Wkts),cex=1,pos=4)
  
  team_colors = statsguRu::getTeamColors()
  
  data$Grouping = gsub(pattern = "in ",replacement = "",x = data$Grouping)
  ggplot(data, aes(Wkts, reorder(Grouping, Wkts))) +
    geom_col(aes(fill = Grouping), color = "black" ,size = 0.15) + 
    scale_fill_manual("Result", values = team_colors)  +
    scale_color_manual("col", values = sapply(team_colors, calc_ContrastColor))+
    geom_text(aes(label = round(Wkts,1), color = Grouping),fontface = "bold", hjust = 1.1, 
              size = 7) + 
    ggtitle(label = plot_title) +
    guides(fill = F, color = F) + 
    theme(rect = element_blank(), line = element_blank(),
          axis.text.y = element_text(hjust = 1,size = 16, colour = "black",
                                     margin = margin(0,0,0,40, unit = "pt")),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(r = 10,unit = "pt"),
          plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0))
  
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Year-wise Bowling Average
#' @keywords Bowling
#' @description This function takes in "Batting Summary" dataframe modified after splitBowlingSummary and plots a player's Year-wise Bowling Average.
#' @param data Output of statsguRu::split_Summary_Bowling()
#' @return Plots a player's year-wise Bowling Average.
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' ashwin_BowlingSummary = statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)
#' statsguRu::dispBowlingAveByYears(data = ashwin_BowlingSummary)

dispBowlingAveByYears = function(data, plot_title = "Year Wise Bowling Average"){
  
  data = data$yearWise
  data$Grouping <- as.integer(gsub('[a-zA-Z]', '', data$Grouping))
  data$Ave = round(as.numeric(data$Ave),1)
  if(names(data[2])=="Span")
  {
    data = data[-2]
  }
  data = na.omit(data)
  plot(x = data$Grouping, y = data$Ave, type = "b", col = "purple", lwd = 2, pch = 15,
       ylim = c(min(data$Ave)-10,max(data$Ave)+10), xlab = "Year", ylab = "Bowling Average", xaxt = 'n', main = plot_title)
  axis(side = 1,data$Grouping,data$Grouping)
  text(data$Grouping, data$Ave, as.integer(data$Ave), cex = 1.2, pos = 3, col = "red")

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Year-wise Bowling Economy Rate
#' @keywords Bowling
#' @description This function takes in "Batting Summary" dataframe modified after splitBowlingSummary and plots a player's Year-wise Bowling Economy Rate.
#' @param data Output of statsguRu::split_Summary_Bowling()
#' @return Plots a player's year-wise Bowling Economy Rate
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' ashwin_BowlingSummary = statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)
#' statsguRu::dispBowlingERByYears(data = ashwin_BowlingSummary)

dispBowlingERByYears = function(data, plot_title = "Year Wise Bowling Economy Rate"){
  
  data = data$yearWise
  data$Econ = round(as.numeric(data$Econ),1)
  if(names(data[2])=="Span")
  {
    data = data[-2]
  }
  data = na.omit(data)
  data$Grouping <- as.integer(gsub('[a-zA-Z]', '', data$Grouping))
  plot(x = data$Grouping, y = data$Econ, type = "b", col = "orange", lwd = 2, pch = 19,
       ylim = c(min(data$Econ)-1,max(data$Econ)+1), xlab = "Year", ylab = "Bowling Economy Rate", xaxt='n', main = plot_title)
  axis(1,data$Grouping,data$Grouping)
  text(data$Grouping,as.numeric(data$Econ),data$Econ,cex=1.2, pos=3, col="brown")

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Scatterplot of a Players's Bowling Average and Economy Rate by List of Oppostion teams
#' @keywords Bowling
#' @description This function takes in "Bowling Summary" dataframe modified after splitBowlingSummary and plots a player's Bowling Average and Economy Rate against every opposition team.
#' @param data Output of statsguRu::split_Summary_Bowling()
#' @return Plots a scatterplot pitting player's Bowling Average and Economy Rate against every opposition team.
#' @export
#' @examples
#' ashwin_BowlingSummary = statsguRu::get_Summary_Bowling(PlayerID = 26421, MatchType = 1)
#' ashwin_BowlingSummary = statsguRu::split_Summary_Bowling(data = ashwin_BowlingSummary, MatchType = 1)
#' statsguRu::dispBowlingAveEconByOpposition(data = ashwin_BowlingSummary)

dispBowlingAveEconByOpposition = function(data, plot_title = "Average vs Economy Rate"){

  data = data$oppCountry
  data$Ave = as.numeric(data$Ave)
  data$Econ = as.numeric(data$Econ)
  data = na.omit(data)

  ggplot(data = data, aes(x = Ave, y = Econ)) +
    theme_bw() + 
    geom_text_repel(aes(label = Grouping), box.padding = unit(0.5, "lines")) +
    geom_point(colour = "black", size = 5) +
    labs(x = "Bowling Average", y = "Economy Rate", title = plot_title) +
    geom_hline(yintercept = mean(data$Econ), linetype = "dashed", color = "blue") + 
    geom_vline(xintercept = mean(data$Ave), linetype = "dashed", color = "orange") +
    theme(axis.text = element_text(hjust = 1,size = 16, colour = "black"),
          plot.background = element_rect(fill = "#ebebeb"),
          panel.background = element_rect(fill = "#ebebeb"),
          axis.title.x = element_text(size = 20, colour = "black",
                                      margin = margin(t = 10,b = 10,unit = "pt")),
          axis.title.y = element_text(size = 20, colour = "black",
                                      margin = margin(r = 10,l=10, unit = "pt")),
          plot.margin = margin(r = 10,unit = "pt"),
          plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0.5,
                                    margin = margin(b = 5,t=10, unit = "pt")))
  

}

