library(statsguRu)
library(shiny)
library(shinydashboard)

# See above for the definitions of ui and server

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      ".sidebar-menu li a { font-size: 30px;padding: 10px }
      #num
      {
      padding:10px;
      //display:inline-block;
      width:100%;height:50px;
      border:3px solid green;
      border-radius: 5px;font-size:140%
      }
      #num:invalid
      {border:3px solid red;}
      #num:focus
      {
      border: 3px solid green;
      }
      #do
      {
      padding:10px;
      width:100%;
      background-color:lightgreen;
      border:3px solid green;
      text-align:center;
      font-size:100%;border-radius: 5px;
      color: black;
      }
      #do:hover
      {
      background-color:#0ac942;
      }")
  )),

  dashboardPage(
    skin = "green",
    dashboardHeader(title = "statsguRu - A Shiny Dashboard for R Package dealing with Cricket Statistics",      titleWidth = 850),
    dashboardSidebar(sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "About",
        tabName = "about",
        badgeLabel = "new",
        badgeColor = "green"
      )
    ),
    width = 250),
    dashboardBody(
      fluidPage(
      tabItems(
      tabItem(
        tabName = "dashboard",
        navbarPage(
          "Tabs",
          tabPanel(
            "How to Use !",

            column(
              width = 4,h3(strong("ESPNcricinfo Player ID")),
              numericInput(
                inputId = "num",
                value = NULL,
                label = NULL
              ),

              h3(radioButtons(
                "MatchType",
                "Match Format:",
                c(
                  "Test" = "1",
                  "One-Day" = "2",
                  "T20I" = "3",
                  "Combined" = "11"
                )
              )),
              actionButton("do", strong("Generate Viz"))
            ),




            column(width = 5,
                   h3(
                     strong("Steps in using this Dashboard!"),
                     tags$ol(
                       tags$li(
                         h3(
                           "Grab the ESPNCricinfo Player ID of the player, you would like to analyze.
                           Some of the sample IDs are on the right!"
                         )
                         ),
                       tags$li(h3("Paste the ID in text box.")),
                       tags$li(h3(
                         "Select a Match Format from the given options"
                       )),
                       tags$li(
                         h3(
                           "Click the 'Generate' button to generate the visualizations.
                           (P.S: Hitting Enter/Return wont work yet! :) )"
                       )
                         ),
                       tags$li(h3(
                         "Switch Between the 3 tabs and have a look at the Stats in colour."
                       ))

                       )

            )),

            column(
              width = 3,
              h3(strong("Sample Player IDs:    ")),
              tags$ul(
                tags$li(h3("Sachin Tendulkar: 35320")),
                tags$li(h3("MS Dhoni: 28081")),
                tags$li(h3("Virat Kohli: 253802")),
                tags$li(h3("Sourav Ganguly: 28779")),
                tags$li(h3("Cheteshwar Pujara: 32540")),
                tags$li(h3("Babar Azam: 348144")),
                tags$li(h3("Vijay Shankar: 477021"))
              )
            ),
            column(width = 12,h3("Warning! Still in Beta Version (v 0.4.0)"),
              h3(strong("Some Bugs:    ")),
              tags$ul(
                tags$li(h3("App Crashes when a format,in which a player has not played,is chosen.Combined format works for any international player")),
                tags$li(h3("Currently this app is restricted to International Players,
                           entering any other player IDs will lead to app termination. You can reload the page then.")),
                tags$li(h3(HTML("<s>Vijay Shankar</s>"),"3D "," Pie Charts are best seen in bigger displays.")
                ))
            )


          ),
          tabPanel(
            "Batting Statistics",
            h3(
              "Visualize the Batting Summary of Players with their ESPNCricinfo Player IDs"
            ),
            fluidRow(
              # box(column(6,plotOutput(outputId="abc1", width="500px",height="400px")),status = "warning",collapsible = TRUE),
              # box(column(6,plotOutput(outputId="abc2", width="500px",height="400px")),status = "warning",collapsible = TRUE),
              # box(column(6,plotOutput(outputId="abc3", width="500px",height="400px")),status = "warning",collapsible = TRUE),
              # box(column(6,plotOutput(outputId="abc4", width="500px",height="400px")),status = "warning",collapsible = TRUE),
              # box(column(6,plotOutput(outputId="abc5", width="500px",height="400px")),status = "warning",collapsible = TRUE)
              box(
                plotOutput(outputId = "abc1"),
                status = "success",
                collapsible = TRUE,
                solidHeader = TRUE,
                title = "Batting Average vs Opposition"
              ),
              box(
                plotOutput(outputId = "abc2"),
                status = "success",
                collapsible = TRUE,
                solidHeader = TRUE,
                title = "Batting Average in Host Counties"
              ),
              box(
                plotOutput(outputId = "abc3"),
                status = "success",
                collapsible = TRUE,
                solidHeader = TRUE,
                title = "Batting Average by Position Played"
              ),
              box(
                plotOutput(outputId = "abc4"),
                status = "success",
                collapsible = TRUE,
                solidHeader = TRUE,
                title = "Year-Wise Batting Average"
              ),
              box(
                plotOutput(outputId = "abc5"),
                status = "success",
                collapsible = TRUE,
                solidHeader = TRUE,
                title = "Batting Dismissal Summary"
              ),
              box(
                plotOutput(outputId = "abc6"),
                status = "success",
                collapsible = TRUE,
                solidHeader = TRUE,
                title = "Batting Dismissal Summary vs Bowler Type"
              ),
              column(
                width = 12,
                h1("Upcoming Topics"),
                tags$ul(tags$li(h3(
                  "Dismissals in Batting Runs Interval"
                )),
                tags$li(h3(
                  "Impact of Toss decision on Batting"
                )),
                tags$li(
                  h3("Batting Performance in Major Tournaments")
                ),
                tags$li(
                  h3("Batting Performance in Major Grounds")
                ),
                tags$li(h3("...")))
              )
            )
          ),
          tabPanel("Bowling Statistics", h1("Coming Soon! Keep Checking the tab")),
          tabPanel("Fielding Statistics", h1("Coming Soon! Keep Checking the tab"))
        )
      ),

      tabItem(tabName = "about",
              column(width = 12,
                     HTML(
                      "<h1><strong>Maintainer:</strong><h1>
                       <h3>Pranav Nagarajan</h3>
                       <h3>Contact : npranav10@gmail.com</h3>
                       <h3>GitHub : <a href='https://github.com/npranav10'>https://github.com/npranav10</a></h3>
                       <h3>LinkedIn : <a href='in.linkedin.com/in/npranav'>in.linkedin.com/in/npranav</a></h3>
                       <h1><strong>Package Used:</strong><h1>
                       <h3>statsguRu</h3>
                       <h1><strong>Package Author:</strong><h1>
                       <h3>Pranav Nagarajan</h3>
                       <h3>URL : <a href='https://github.com/npranav10/statsguRu'>https://github.com/npranav10/statsguRu</a></h3>
                       <h3>Will be published in CRAN soon!</h3>
                     ")

                     )
              )
      ))
    ))
  )

# Define server logic required to draw a histogram ----
server <- function(input, output)
{
  observeEvent(input$do, withProgress(message = 'Making plot', value = 0,
                                      {
                                        mt = input$MatchType
                                        id = as.integer(input$num)

                                        temp1 = getBattingSummary(id, mt)

                                        temp1 = splitBattingSummary(temp1, mt)
                                        # validate(
                                        #   need(tryCatch({temp1 = splitBattingSummary(temp1, mt)},error=function(error_message)
                                        #     { return(FALSE)  },finally={showNotification("This Player hasnt played in the selected format.")})
                                        #
                                        #
                                        #        )
                                        # )
                                        temp2 = getDismissalSummary(id, mt)
                                        # validate(
                                        #   need(tryCatch({temp1 = splitDismissalSummary(temp1, mt)},error=function(error_message) { return(FALSE)  }),
                                        #
                                        #          showNotification("This Player hasnt played in the selected format.")
                                        #        )
                                        # )
                                        temp2 = splitDismissalSummary(temp2)
                                        output$abc1 <-
                                          renderPlot(dispBattingAveByOpposition(temp1))
                                        incProgress(0.16)
                                        output$abc4 <-
                                          renderPlot(dispBattingAveByYears(temp1))
                                        incProgress(0.16)
                                        output$abc3 <-
                                          renderPlot(dispBattingAveByPosPlayed(temp1))
                                        incProgress(0.16)
                                        output$abc2 <-
                                          renderPlot(dispBattingAveByHostCountry(temp1))
                                        incProgress(0.16)
                                        output$abc5 <-
                                          renderPlot(dispBatsmanDismissals(temp2))
                                        incProgress(0.16)
                                        output$abc6 <-
                                          renderPlot(dispBatsmanDismissalsByBowlerType(temp2))
                                        incProgress(0.2)

                                      }))


}


shinyApp(ui = ui, server = server)
