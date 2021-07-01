#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Gets a Players's Overall Batting Summary
#' @keywords Batting
#' @description This function scraps overall batting summary info from Statsguru and returns the data as a dataframe.
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns dataframe containing overall Batting Summary of a Player
#' @export
#' @examples
#' sachin_BattingSummary_Overall = statsguRu::get_Summary_Batting_Overall(PlayerID = 35320, MatchType = 11)
#' print(sachin_BattingSummary_Overall)

get_Summary_Batting_Overall = function(PlayerID, MatchType){
  
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting",sep="");
  data = htmltab::htmltab(url,which = 3,rm_nodata_rows = FALSE)
  return(data)
  
  # url = "https://stats.espncricinfo.com/ci/engine/player/35320.html?class=1;template=results;type=batting"
  # library(rvest)
  # battingSummaryPage <- xml2::read_html(url)
  # 
  # battingSummaryPage %>% 
  #   html_nodes("table") %>% 
  #   .[[4]] %>% 
  #   html_table() %>% 
  #   as.data.frame()
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Gets a Players's Batting Summary
#' @keywords Batting
#' @description This function scraps batting summary info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns dataframe containing complete Batting Summary of a Player
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 11)
#' print(sachin_BattingSummary)

get_Summary_Batting = function(PlayerID, MatchType){
  
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=batting",sep="");
  data = htmltab::htmltab(url,which = 4,rm_nodata_rows = FALSE)
  return(data)

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Splits Batting Summary data into different categories
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe  and returns a list comprising data segregated into different categories that can inferred from a Players's Batting Summary.
#' @param data Output of statsguRu::get_Summary_Batting().
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns a list comprising data segregated into different categories of Batting Summary statistics
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' statsguRu::split_Summary_Batting(data = sachin_BattingSummary, MatchType = 2)

split_Summary_Batting = function(data, MatchType){
  
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
    temp = list(
      "oppCountry" = oppCountry,
      "hostCountry" = hostCountry,
      "hostContinent" = hostContinent,
      "hostStatus" = hostStatus,
      "yearWise" = yearWise,
      "posPlayed" = posPlayed
    )
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
    temp = list(
      "formats" = formats,
      "oppCountry" = oppCountry,
      "hostCountry" = hostCountry,
      "hostContinent" = hostContinent,
      "hostStatus" = hostStatus,
      "yearWise" = yearWise,
      "posPlayed" = posPlayed
    )
    return(temp)
  }

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Batting Average by List of Oppostion teams
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average against every opposition team.
#' @param data Output of statsguRu::split_Summary_Batting()
#' @return Plots a player's batting average against every opposition team.
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
#' statsguRu::dispBattingAveByOpposition(data = sachin_BattingSummary)

dispBattingAveByOpposition = function(data, plot_title = "Batting Average vs Opp Country"){

  team_colors = statsguRu::getTeamColors()
  
  data = data$oppCountry
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  data = data %>% arrange(desc(Ave))
  data$Grouping = gsub(pattern = "v ",replacement = "",x = data$Grouping)
  ggplot(data, aes(Ave, reorder(Grouping, Ave))) +
    geom_col(aes(fill = Grouping), color = "black" ,size = 0.15) + 
    scale_fill_manual("Result", values = team_colors)  +
    scale_color_manual("col", values = sapply(team_colors, calc_ContrastColor))+
    geom_text(aes(label = round(Ave,1), color = Grouping),fontface = "bold", hjust = 1.1, 
              size = 7) + 
    labs(title = plot_title) +
    guides(fill = F, color = F) + 
    theme(rect = element_blank(), line = element_blank(),
          axis.text.y = element_text(hjust = 1,size = 16, colour = "black",
                                     margin = margin(0,0,0,40, unit = "pt")),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(r = 10,unit = "pt"),
          plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0))
  

  # 
  # bp = barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main="Batting Average vs Opp Country")
  # text(bp,0,as.integer(data$Ave),cex=1,pos=3)

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Batting Average by List of Host Country
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every host opposition team.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting average in Every host Country.
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
#' statsguRu::dispBattingAveByHostCountry(data = sachin_BattingSummary)

dispBattingAveByHostCountry = function(data, plot_title  = "Batting Average in Host Country"){
  
  data = data$hostCountry
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  
  team_colors = statsguRu::getTeamColors()
  
  data$Grouping = gsub(pattern = "in ",replacement = "",x = data$Grouping)
  ggplot(data, aes(Ave, reorder(Grouping, Ave))) +
    geom_col(aes(fill = Grouping), color = "black" ,size = 0.15) + 
    scale_fill_manual("Result", values = team_colors)  +
    scale_color_manual("col", values = sapply(team_colors, calc_ContrastColor))+
    geom_text(aes(label = round(Ave,1), color = Grouping),fontface = "bold", hjust = 1.1, 
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
  
  # bp = barplot(as.numeric(data$Ave),names = as.vector(data$Grouping),las =2,cex.names = 0.9,col = rainbow(length(data$Ave)),main=)
  # text(bp,0,as.integer(data$Ave),cex=1,pos=3)

  #abline(h=c$Ave,lwd=2, lty="dashed", col="red")
  #text(0.5, as.integer(c$Ave)+2, "Average", col = "black")
  }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Batting Average by List of Continents Played by the Player
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every contitent.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting average in every contient.
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
#' statsguRu::dispBattingAveByContinent(data = sachin_BattingSummary)

dispBattingAveByContinent = function(data, plot_title = "Batting Average by Continent"){
  
  data = data$hostContinent
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  bp = barplot(as.numeric(data$Ave), names = as.vector(data$Grouping),
               las = 2, cex.names = 0.9, col = rainbow(length(data$Ave)), main = plot_title)
  text(bp,0,as.integer(data$Ave),cex=1,pos=3)

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Batting Average Year-wise
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's year-wise batting average.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's year-wise batting average.
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
#' statsguRu::dispBattingAveByYears(data = sachin_BattingSummary)

dispBattingAveByYears = function(data, plot_title = "Year Wise Batting Average"){
  
  data = data$yearWise
  data$Ave = round(as.numeric(data$Ave),1)
  data$Grouping <- as.integer(gsub('[a-zA-Z]', '', data$Grouping))
  data$Span = NULL
  data = na.omit(data)
  plot(x = data$Grouping, y = data$Ave,
       type = "b", col = "orange", lwd = 2, pch = 19,
       ylim = c(min(data$Ave)-10,max(data$Ave)+10), xlab = "Year", ylab = "Batting Average", xaxt = 'n', main = plot_title)
  axis(1,data$Grouping,data$Grouping)
  text(data$Grouping, data$Ave, as.integer(data$Ave), cex = 1.2, pos = 3, col = "brown")

  # if(length(data$Grouping) <= 10){
  #   x_axis_breaks = seq(min(data$Grouping),max(data$Grouping))
  # } else {
  #   x_axis_breaks = seq(min(data$Grouping),max(data$Grouping), by = 2)
  # }
  #                       
  # ggplot(data = data, aes(x=Grouping, y=Ave)) +
  #   geom_line(linetype = "dashed", size = 0.5)+
  #   geom_point(size = 4) + 
  #   geom_label_repel(aes(label = Ave), position = position_dodge(1), size = 5,box.padding = 0)+
  #   scale_x_continuous(breaks = x_axis_breaks) +
  #   labs(x = "Year", y = "Batting Average")+ 
  #   theme(rect = element_blank(),
  #         axis.text.y = element_text(size = 14, colour = "black"),
  #         axis.text.x = element_text(size = 14, colour = "black"),
  #         axis.line.x = element_line(),
  #         axis.line.y = element_line(),
  #         axis.title.x = element_text(size = 20, colour = "black",margin = margin(t = 10, unit = "pt")),
  #         axis.title.y = element_text(size = 20, colour = "black",margin = margin(r = 10, unit = "pt")),
  #         plot.margin = margin(l = 10, b = 10,unit = "pt"),
  #         plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0))
  
}


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Barplot of Players's Batting according to the Player's Batting Position
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting position-wise batting average.
#' @param data Output of splitBattingSummary.
#' @return Plots a player's batting position-wise batting average.
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
#' statsguRu::dispBattingAveByPosPlayed(data = sachin_BattingSummary)

dispBattingAveByPosPlayed = function(data, plot_title = "Batting Average by Position Played"){
  
  data = data$posPlayed
  data$Ave = as.numeric(data$Ave)
  data = na.omit(data)
  # bp = barplot(height = as.numeric(data$Ave),
  #              names = gsub(pattern = " position",replacement = "",x = as.vector(data$Grouping)),
  #              las = 1,
  #              cex.names = 1.9,
  #              col = rainbow(length(data$Ave)),
  #              main = plot_title , yaxt = "n")
  # text(bp,1,as.integer(data$Ave),cex = 1.9,pos=3, col = as.vector(sapply(rainbow(length(data$Ave)), calc_ContrastColor)))
  
  data$Grouping = gsub(pattern = "position",replacement = "",x = data$Grouping)
  ggplot(data, aes(Ave, reorder(Grouping, -as.numeric(row.names(data))))) +
    geom_col(fill = "#ebebeb", color = "black" ,size = 0.15) + 
    geom_text(aes(label = round(Ave,1)), color = "black",fontface = "bold", hjust = 1.1, 
              size = 7) + 
    ggtitle(label = plot_title) +
    labs(x = "Batting Average", y = "Batting Position", title = plot_title)+ 
    guides(fill = F, color = F) + 
    theme(rect = element_blank(), line = element_blank(),
          axis.text.y = element_text(hjust = 1,size = 16, colour = "black",
                                     margin = margin(0,0,0,40, unit = "pt")),
          axis.title.x = element_text(size = 20, colour = "black",margin = margin(t = 10, unit = "pt")),
          axis.title.y = element_text(size = 20, colour = "black",margin = margin(r = 0, unit = "pt")),
          axis.text.x = element_blank(),
          plot.margin = margin(r = 10,unit = "pt"),
          plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0))
  }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Displays Scatterplot of a Players's Batting Average and Strike rate by List of Oppostion teams
#' @keywords Batting
#' @description This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average and Strike Rate against every opposition team.
#' @param data Output of splitBattingSummary.
#' @return Plots a scatterplot pitting player's batting average and strike-rate against every opposition team.
#' @export
#' @examples
#' sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
#' statsguRu::dispBattingAveSRByOpposition(data = sachin_BattingSummary)

dispBattingAveSRByOpposition = function(data, plot_title = "Average vs Strike Rate"){

  data = data$oppCountry
  if("SR" %in% colnames(data)) {
    
    data$Ave = as.numeric(data$Ave)
    data$SR = as.numeric(data$SR)
    data = na.omit(data)
    #with(plot(data$Ave~data$SR,data = data,xlim = c(min(data$SR)-10,max(data$SR)+10),
    #          ylim = c(min(data$Ave)-10,max(data$Ave)+10),pch = 19,xlab = "Strike-Rate",ylab = "Average",main = "Batting Average vs Strike-Rate"),
    #     text(data$SR,data$Ave, labels=data$Grouping, cex= 0.8,pos = 4))
    team_colors = statsguRu::getTeamColors()
    
    ggplot(data = data, aes(x = Ave, y = SR)) + 
      theme_bw() + 
      geom_text_repel(aes(label = Grouping), box.padding = unit(0.5, "lines")) +
      geom_point(aes(fill = gsub(pattern = "v ",replacement = "",x = Grouping)),color="black", size = 5) +
      scale_fill_manual("Result", values = team_colors)  +
      labs(x = "Batting Average", y = "Strike Rate", title = plot_title) +
      guides(color = F, fill = F)+
      geom_hline(yintercept=mean(data$SR),linetype="dashed", color = "blue") + 
      geom_vline(xintercept=mean(data$Ave),linetype="dashed", color = "orange") + 
      theme(axis.text = element_text(hjust = 1,size = 16, colour = "black"),
            plot.background = element_rect(fill = "#ebebeb"),
            
            panel.background = element_rect(fill = "#ebebeb"),
            axis.title.x = element_text(size = 20, colour = "black",margin = margin(t = 10,b = 10,unit = "pt")),
            axis.title.y = element_text(size = 20, colour = "black",margin = margin(r = 10,l=10, unit = "pt")),
            plot.margin = margin(r = 10,unit = "pt"),
            plot.title = element_text(colour = "black",  size = 20, face = "bold", hjust = 0.5,
                                      margin = margin(b = 5,t=10, unit = "pt")))
  } else {
    print("Strike Rate Data Not Availabe for this Player")
  }

}
