#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Gets a Players's Wickets Summary (whilst Bowling)
#' @keywords Bowling
#' @description This function scraps exclusive dismissal summary (as a bowler) info from Statsguru and returns a dataframe
#' @param PlayerID ESPNCricinfo Player ID.
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns DataFrame containing complete Wickets Summary of a Bowler
#' @export
#' @examples
#' ashwin_DismissalSummary_Bowling = statsguRu::get_DismissalSummary_Bowling(PlayerID = 26421, MatchType = 1)
#' print(ashwin_DismissalSummary_Bowling)

get_DismissalSummary_Bowling = function(PlayerID, MatchType){
  
  url = paste("http://stats.espncricinfo.com/ci/engine/player/",PlayerID,".html?class=",MatchType,";template=results;type=bowling;;view=dismissal_summary",sep="");
  data = htmltab::htmltab(url,which = 4,rm_nodata_rows = FALSE)
  return(data)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Splits Wickets Summary data into different categories
#' @keywords Bowling
#' @description This function takes in "Wickets Summary" dataframe  and returns a list comprising data segregated into different categories that can inferred from a Players's Bowling Summary.
#' @param data Output of statsguRu::get_DismissalSummary_Bowling().
#' @param MatchType Type of Match Played (1 for Test; 2 for ODI; 3 for T20I ; 11 for All)
#' @return Returns a list comprising data segregated into different categories of Wickets Summary statistics
#' @export
#' @examples
#' ashwin_DismissalSummary_Bowling = statsguRu::get_DismissalSummary_Bowling(PlayerID = 26421, MatchType = 1)
#' statsguRu::split_DismissalSummary_Bowling(data = ashwin_DismissalSummary_Bowling, MatchType = 1)

split_DismissalSummary_Bowling = function(data, MatchType){

  a=c();
  for (i in 1:nrow(data)) {
    if(is.na(data[i,1]))
    {
      a=append(a,i)
    }
  }
  if((MatchType==1) || (MatchType==2) || (MatchType==3))
  {

    batsmanType=data[1:a[1]-1,]
    batsmanPos=data[(a[1]+1):(a[2]-1),]
    batsmanRunsInt = data[(a[2]+1):(a[3]-1),]
    batsmanDisType = data[(a[3]+1):nrow(data),]

  }

  temp = list("batsmanType"= batsmanType,
              "batsmanPos" = batsmanPos,
              "batsmanRunsInt" = batsmanRunsInt,
              "batsmanDisType" = batsmanDisType)
  return(temp)


}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Plots a pie chart of Dismissal Type Summary of a Bowler.
#' @keywords Bowling
#' @description This function takes in output of splitDismissalSummary and plots the Dismissal Type Summary of a Bowler as a pie chart.
#' @param data Output of statsguRu::split_DismissalSummary_Bowling().
#' @return Plots a pie chart of Dismissal Type Summary of a Bowler.
#' @export
#' @examples
#' ashwin_DismissalSummary_Bowling = statsguRu::get_DismissalSummary_Bowling(PlayerID = 26421, MatchType = 1)
#' ashwin_DismissalSummary_Bowling = statsguRu::split_DismissalSummary_Bowling(data = ashwin_DismissalSummary_Bowling, MatchType = 1)
#' statsguRu::dispBowlerDismissals(data = ashwin_DismissalSummary_Bowling)

dispBowlerDismissals = function(data, plot_title = "Distribution of Dismissals") {
  
  x = as.numeric(data$batsmanDisType$Dis)
  l = round(100*x/sum(x), 1)
  l = paste(l,"%",sep = "")
  pie3D(x, labels = l, radius = 1, main = plot_title, mar = c(1,4,4,10));
  legend("topright", as.vector(data$batsmanDisType$Grouping), cex = 1,fill = rainbow(length(data$batsmanDisType$Dis)),xpd = TRUE,inset=c(-0.35,-0.05));

}
