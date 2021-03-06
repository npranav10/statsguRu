% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batting.R
\name{dispBattingAveSRByOpposition}
\alias{dispBattingAveSRByOpposition}
\title{Displays Scatterplot of a Players's Batting Average and Strike rate by List of Oppostion teams}
\usage{
dispBattingAveSRByOpposition(data, plot_title = "Average vs Strike Rate")
}
\arguments{
\item{data}{Output of splitBattingSummary.}
}
\value{
Plots a scatterplot pitting player's batting average and strike-rate against every opposition team.
}
\description{
This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average and Strike Rate against every opposition team.
}
\examples{
sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
statsguRu::dispBattingAveSRByOpposition(data = sachin_BattingSummary)
}
\keyword{Batting}
