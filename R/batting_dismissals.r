#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Gets a Players's Dismissal Summary (whilst Batting)
#' @keywords Batting
#' @description This function scraps exclusive dismissal summary (as a batsman) info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns DataFrame containing complete Batting Dismissals Summary of a Player
#' @export
#' @examples
#' sachin_DismissalSummary_Batting = statsguRu::get_DismissalSummary_Batting(PlayerID = 35320, MatchType = 2)
#' print(sachin_DismissalSummary_Batting)

get_DismissalSummary_Batting = function(PlayerID, MatchType){
  
  url = paste(
    "http://stats.espncricinfo.com/ci/engine/player/",
    PlayerID,
    ".html?class=",
    MatchType,
    ";template=results;type=batting;;view=dismissal_summary",
    sep = ""
  )
  
  data = htmltab::htmltab(url, which = 4, rm_nodata_rows = FALSE)
  return(data)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Splits Dismissal Summary data into respective categories
#' @keywords Batting
#' @description This function takes in output dataframe of getDismissalSummary and returns a list comprising data segregated into different dismissal categories.
#' @param dataframe Output of getDismissalSummary().
#' @return Returns a list comprising data segregated into different dismissal categories
#' @export
#' @examples
#' sachin_DismissalSummary_Batting = statsguRu::get_DismissalSummary_Batting(PlayerID = 35320, MatchType = 2)
#' statsguRu::split_DismissalSummary_Batting(sachin_DismissalSummary_Batting)

split_DismissalSummary_Batting = function(dataframe){
  k = 1
  j = 1
  a = c()
  
  for (i in 1:nrow(dataframe)) {
    if (is.na(dataframe[i, 1]))
    {
      a = append(a, i)
    }
  }
  #dataframe
  bowlingHand = dataframe[1:a[1] - 1, ]
  bowlingStyle = dataframe[(a[1] + 1):(a[2] - 1), ]
  bowlingHandStyleComb = dataframe[(a[2] + 1):(a[3] - 1), ]
  battingPosition = dataframe[(a[3] + 1):(a[4] - 1), ]
  runsInterval = dataframe[(a[4] + 1):(a[5] - 1), ]
  dismissalType = dataframe[(a[5] + 1):nrow(dataframe), ]
  temp = list(
    "bowlingHand" = bowlingHand,
    "bowlingStyle" = bowlingStyle,
    "bowlingHandStyleComb" = bowlingHandStyleComb,
    "battingPosition" = battingPosition,
    "runsInterval" = runsInterval,
    "dismissalType" = dismissalType
  )
  return(temp)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Plots a pie chart of Dismissal Summary vs type and style of Bowlers
#' @keywords Batting
#' @description This function takes in output of splitDismissalSummary  and Plots a pie chart of Dismissal Summary vs type and style of Bowlers.
#' @param data Output of statsguRu::split_DismissalSummary_Batting().
#' @return Plots a pie chart of Dismissal Summary vs type and style of Bowlers
#' @export
#' @examples
#' sachin_DismissalSummary_Batting = statsguRu::get_DismissalSummary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_DismissalSummary_Batting = statsguRu::split_DismissalSummary_Batting(sachin_DismissalSummary_Batting)
#' statsguRu::dispBattingDismissalsByBowlerType(data = sachin_DismissalSummary_Batting)

dispBattingDismissalsByBowlerType = function(data, plot_title = "Distribution of Dismissals vs Bowlers") {
  
  data = data$bowlingHandStyleComb
  
  data = data %>% 
    filter(Grouping %in% c("right-arm pace", "right-arm spin","left-arm pace","left-arm spin"))
  
  x = as.numeric(data$Dis)
  l = round(100 * x / sum(x), 1)
  l = paste(l, "%", sep = "")
  pie3D(
    x,
    labels = l,
    radius = 1,
    main = plot_title,
    mar = c(1, 4, 4, 10)
  )
  
  legend(
    "topright",
    as.vector(data$Grouping),
    cex = 1,
    fill = rainbow(length(data$Dis)),
    xpd = TRUE,
    inset = c(-0.35, -0.05)
  )
  

}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Plots a pie chart of Dismissal Summary of a Player
#' @keywords Batting
#' @description This function takes in output of splitDismissalSummary and plots the Dismissal Summary of a Player as a pie chart.
#' @param data Output of splitDismissalSummary().
#' @return Plots a pie chart of Dismissal Summary of a player.
#' @export
#' @examples
#' sachin_DismissalSummary_Batting = statsguRu::get_DismissalSummary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_DismissalSummary_Batting = statsguRu::split_DismissalSummary_Batting(sachin_DismissalSummary_Batting)
#' statsguRu::dispDismissalsBatting(data = sachin_DismissalSummary_Batting)

dispDismissalsBatting = function(data, plot_title = "Distribution of Dismissals") {
  
  data= cbind(data$dismissalType,0)
  
  data = data %>% 
    filter(Grouping %in% c("caught","bowled","run out","leg before wicket","run out",
                           "stumped","not out","hit wicket"))
  for(i in 1:nrow(data))
  {
    if(colnames(data[2])=="Span")
      data$'0'[i] =sum(as.numeric(data[i,4:12]))
    else
      data$'0'[i] =sum(as.numeric(data[i,3:11]))
  }
  x = data$'0'
  l = round(100 * x / sum(x), 1)
  l = paste(l, "%", sep = "")
  
  pie3D(
    x,
    labels = l,
    radius = 1,
    main = plot_title,
    mar = c(1, 4, 4, 10)
  )
  
  legend(
    "topright",
    as.vector(data$Grouping),
    cex = 1,
    fill = rainbow(length(data$Dis)),
    xpd = TRUE,
    inset = c(-0.35, -0.05)
  )
  
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Plots a bar chart of Player's Batting average vs type and style of Bowlers
#' @keywords Batting
#' @description This function takes in output of splitDismissalSummary()  and Plots a bar chart of Player's Batting averagevs type and style of Bowlers.
#' @param data Output of statsguRu::split_DismissalSummary_Batting().
#' @return Plots a pie chart of Dismissal Summary vs type and style of Bowlers
#' @export
#' @examples
#' sachin_DismissalSummary_Batting = statsguRu::get_DismissalSummary_Batting(PlayerID = 35320, MatchType = 2)
#' sachin_DismissalSummary_Batting = statsguRu::split_DismissalSummary_Batting(sachin_DismissalSummary_Batting)
#' statsguRu::dispBattingAveByBowlerType(data = sachin_DismissalSummary_Batting)

dispBattingAveByBowlerType = function(data, plot_title = "Batting Average by Bowler-Type") {
  
  data = data$bowlingHandStyleComb
  
  data = data %>% 
    filter(Grouping %in% c("right-arm pace", "right-arm spin","left-arm pace","left-arm spin"))
  data$Grouping[data$Grouping == "right-arm pace"] = "Right-arm Pace"
  data$Grouping[data$Grouping == "right-arm spin"] = "Right-arm Spin"
  data$Grouping[data$Grouping == "left-arm pace"] = "Left-arm Pace"
  data$Grouping[data$Grouping == "left-arm spin"] = "Left-arm Spin"
  
  # data$Grouping = gsub(pattern = "position",replacement = "",x = data$Grouping)
  data$Ave = as.numeric(data$Ave)
  ggplot(data, aes(Ave, reorder(Grouping, -as.numeric(row.names(data))))) +
    geom_col(fill = "#ebebeb", color = "black" ,size = 0.15) + 
    geom_text(aes(label = round(Ave,1)), color = "black",fontface = "bold", hjust = 1.1, 
              size = 7) + 
    ggtitle(label = plot_title) +
    labs(x = "Batting Average", y = "Bowler Type", title = plot_title)+ 
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