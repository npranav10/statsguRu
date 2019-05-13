library(statsguRu)
library(shiny)

# See above for the definitions of ui and server

# Define UI for app that draws a histogram ----
ui <- fluidPage(tags$head(
  tags$style(HTML('#do1{background-color:orange}'))
),

                dashboardPage(skin = "green",
                  dashboardHeader(
                    title = "statsguRu - A Shiny Dashboard for R Package dealing with Cricket Statistics",      titleWidth = 850
                  ),
                  dashboardSidebar(width = 400,
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
                      box(plotOutput(outputId="abc6"),status = "danger",collapsible = TRUE,solidHeader=TRUE,title = "Batting Dismissal Summary vs Bowler Type")
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
