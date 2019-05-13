library(statsguRu)
library(shiny)

# See above for the definitions of ui and server

# Define UI for app that draws a histogram ----
ui <- fluidPage(tags$head(
  tags$style(HTML(".sidebar-menu li a { font-size: 30px;padding: 10px }"))
),

                dashboardPage(skin = "green",
                  dashboardHeader(
                    title = "statsguRu - A Shiny Dashboard for R Package dealing with Cricket Statistics",      titleWidth = 850
                  ),
                  dashboardSidebar(sidebarMenu(
                    menuItem("Dashboard", tabName = "dashboard",badgeLabel = "new", badgeColor = "green"),
                    menuItem("About", tabName = "about", badgeLabel = "new", badgeColor = "green")
                  ),width = 400,
                  column(width = 12,h1("Warning! Still in Beta Version")),
                    textInput(inputId = "num",
                              h3("ESPNcricinfo Player ID")),
                    column(width = 12,
                    h3(radioButtons("MatchType", "Match Format:",
                                 c("Test" = "1",
                                   "One-Day" = "2",
                                   "T20I" = "3",
                                   "Combined" = "11")))),
                    column(width = 12,actionButton("do", strong("Click to Update !")),style="color: #fff; border-color: #2e6da4"),

                    column(width = 12,style='padding:15px;',h3(strong("Sample Player IDs:    "))),
                    column(width = 12,
                           tags$ul(
                             tags$li(h3("Sachin Tendulkar: 35320")),
                             tags$li(h3("MS Dhoni: 28081")),
                             tags$li(h3("Virat Kohli: 253802")),
                             tags$li(h3("Sourav Ganguly: 28779")),
                             tags$li(h3("Cheteshwar Pujara: 32540")),
                             tags$li(h3("Babar Azam: 348144")),
                             tags$li(h3("Vijay Shankar: 477021"))
                           ))




                  ),
                  dashboardBody(
                    tabItems(
                      tabItem(tabName = "dashboard",
                              tabsetPanel(type = "tabs",
                                          tabPanel("Batting Statistics",
                                                   h3("Visualize the Batting Summary of Players with their ESPNCricinfo Player IDs"),
                                                   fluidRow(
                                                     # box(column(6,plotOutput(outputId="abc1", width="500px",height="400px")),status = "warning",collapsible = TRUE),
                                                     # box(column(6,plotOutput(outputId="abc2", width="500px",height="400px")),status = "warning",collapsible = TRUE),
                                                     # box(column(6,plotOutput(outputId="abc3", width="500px",height="400px")),status = "warning",collapsible = TRUE),
                                                     # box(column(6,plotOutput(outputId="abc4", width="500px",height="400px")),status = "warning",collapsible = TRUE),
                                                     # box(column(6,plotOutput(outputId="abc5", width="500px",height="400px")),status = "warning",collapsible = TRUE)
                                                     box(plotOutput(outputId="abc1"),status = "warning",collapsible = TRUE,solidHeader=TRUE,title = "Batting Average vs Opposition"),
                                                     box(plotOutput(outputId="abc2"),status = "primary",collapsible = TRUE,solidHeader=TRUE,title = "Batting Average in Host Counties"),
                                                     box(plotOutput(outputId="abc3"),status = "success",collapsible = TRUE,solidHeader=TRUE,title = "Batting Average by Position Played"),
                                                     box(plotOutput(outputId="abc4"),status = "info",collapsible = TRUE,solidHeader=TRUE,title = "Year-Wise Batting Average"),
                                                     box(plotOutput(outputId="abc5"),status = "danger",collapsible = TRUE,solidHeader=TRUE,title = "Batting Dismissal Summary"),
                                                     box(plotOutput(outputId="abc6"),status = "danger",collapsible = TRUE,solidHeader=TRUE,title = "Batting Dismissal Summary vs Bowler Type"),
                                                     column(width = 12,
                                                            h1("Upcoming Topics"),
                                                            tags$ul(
                                                              tags$li(h3("Dismissals in Batting Runs Interval")),
                                                              tags$li(h3("Impact of Toss decision on Batting")),
                                                              tags$li(h3("Batting Performance in Major Tournaments")),
                                                              tags$li(h3("..."))

                                                            ))
                                                   )),
                                          tabPanel("Bowling Statistics", h1("Coming Soon! Keep Checking the tab")),
                                          tabPanel("Fielding Statistics",h1("Coming Soon! Keep Checking the tab"))
                              )
                      ),

                      tabItem(tabName = "about",
                              column(width = 12,h2(strong("About This Shiny App :"))),
                              column(width = 12,h2("A Dashboard to see visualizations of a Cricketer's profile")),
                              column(width = 12,h2(strong("Package Used :"))),
                              column(width = 12,h2("statsguRu")),
                              column(width = 12,h2("https://github.com/npranav10/statsguRu")),
                              column(width = 12,h2("Maintainer : Pranav Nagarajan ")),
                              column(width = 12,h2("Contact : npranav10@gmail.com "))
                      )
                    )


                  )
                ))


# Define server logic required to draw a histogram ----
server <- function(input, output)
{
  observeEvent(input$do,
               {
                 mt = input$MatchType
                 id = as.integer(input$num)
                 temp1 = getBattingSummary(id,mt)
                 temp1 = splitBattingSummary(temp1,mt)
                 temp2 = getDismissalSummary(id,mt)
                 temp2 = splitDismissalSummary(temp2)
                 output$abc1 <- renderPlot(dispBattingAveByOpposition(temp1))
                 output$abc4 <- renderPlot(dispBattingAveByYears(temp1))
                 output$abc3 <- renderPlot(dispBattingAveByPosPlayed(temp1))
                 output$abc2 <- renderPlot(dispBattingAveByHostCountry(temp1))
                 output$abc5 <- renderPlot(dispBatsmanDismissals(temp2))
                 output$abc6 <- renderPlot(dispBatsmanDismissalsByBowlerType(temp2))

               }
  )

}


shinyApp(ui = ui, server = server)
