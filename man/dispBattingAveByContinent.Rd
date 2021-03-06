% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batting.R
\name{dispBattingAveByContinent}
\alias{dispBattingAveByContinent}
\title{Displays Barplot of Players's Batting Average by List of Continents Played by the Player}
\usage{
dispBattingAveByContinent(data, plot_title = "Batting Average by Continent")
}
\arguments{
\item{data}{Output of splitBattingSummary.}
}
\value{
Plots a player's batting average in every contient.
}
\description{
This function takes in "Batting Summary" dataframe modified after splitBattingSummary and plots a player's batting average in every contitent.
}
\examples{
sachin_BattingSummary = statsguRu::get_Summary_Batting(PlayerID = 35320, MatchType = 2)
sachin_BattingSummary = statsguRu::split_Summary_Batting(sachin_BattingSummary, MatchType = 2)
statsguRu::dispBattingAveByContinent(data = sachin_BattingSummary)
}
\keyword{Batting}
