#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Calculate the contrast color for a given color.
#' @keywords Misc
#' @description This function takes in a color (\code{input_color}) returns a color in contrast to the input color.
#' @param input_color ESPNCricinfo Player ID.
#' @return "black" or "white" depending on contrast of \code{input_color}
#' @export
#' @examples
#' calc_ContrastColor("white")
#' calc_ContrastColor("red")
#' calc_ContrastColor("blue")
#' calc_ContrastColor("black")

calc_ContrastColor = function (input_color) {
  R = col2rgb(input_color)[1]
  G = col2rgb(input_color)[2]
  B = col2rgb(input_color)[3]
  yiq = ((R * 299) + (G * 587) + (B * 114))/1000
  if (yiq >= 128) {
    return("black")
  }
  else {
    return("white")
  }
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#' @title Get Team Jersey Colors
#' @keywords Misc
#' @description This function returns a named vector containing hex code of all national team's jersey.
#' @return A named vector containing hex code of jersey colors.
#' @export
#' @examples
#' statsguRu::getTeamColors()

getTeamColors = function(){
  return(
    team_colors = c(
      "Australia" = "#ffdb00",
      "Afghanistan" = "#172ad5",
      "Africa XI" = "lightgreen",
      "Bangladesh" = "#17481e",
      "Bermuda" = "darkred",
      "Canada" = "#ca2029",
      "England" = "#45b1e3",
      "Hong Kong" = "#f9013d",
      "India" = "#2d7fbb",
      "Ireland" = "#00ff92",
      "ICC World XI" = "#00b1da",
      "Kenya" = "#447975",
      "Malaysia" = "#383e46",
      "Namibia" = "blue",
      "Netherlands" = "orange",
      "New Zealand" = "#000000",
      "Pakistan" = "#0e7e3d",
      "Singapore" = "black",
      "South Africa" = "#6cb24a",
      "Sri Lanka"  = "#0378f0",
      "U.A.E." = "#415355",
      "U.S.A." = "#3d6bbc",
      "West Indies" = "#813551",
      "World-XI" = "#00b1da",
      "Scotland" = "#393d5a",
      "Zimbabwe" = "#ff0000"
    ) 
  )
}