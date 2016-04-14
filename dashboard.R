## dashboard.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(googleVis)

setwd("~/admin/freelance/trifinance/code/viz")

ui <- dashboardPage(
  dashboardHeader(title = "DQSS dashboard"),
  dashboardSidebar(),
    # sidebarMenu(
    #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    #   menuItem("Business Rules", tabName = "Business Rules", icon = icon("th"))),width = 150),
  dashboardBody(
    #tabItems(
      # dashboard
      #tabItem(tabName = "dashboard",
        # Boxes need to be put in a row (or column)
        #fluidRow(
        #column(width = 3, textOutput("summaryBR"))
          #column(width = 9, uiOutput("sliderBR"))
        #),
        # fluidRow(
        #   box(plotOutput("totalBR", height = 300, width = 1200))
        # ),
        fluidRow(
          column(width = 1,
            checkboxInput("unhandled", "unhandled", TRUE)
            ,checkboxInput("confirmed", "confirmed", TRUE)
            ,checkboxInput("whitelisted", "whitelisted", TRUE)
            ,checkboxInput("failed", "failed", TRUE)),
          column(width = 9,htmlOutput("totalBRvis"))
        )
        # ,fluidRow(
        #   htmlOutput("totalBRvis")
        # )
      )
    #)
  #)
)

server <- function(input, output) {
  load(file = "testPOError.RData")
  
  #Consider failed BR only
  BRs <- unique(po_current_errors$BR.number)
  BRs <- BRs[order(BRs)]
  DEs <- unique(po_current_errors$Data.Element)
  DEs <- DEs[order(DEs)]
  
  #Business rules section
  # output$summaryBR <- renderText({ 
  #   paste("Number of failed Business Rules:", length(BRs),
  #         "Number of failed rules:", sum(po_current_errors$Failed))
  # })
  # output$sliderBR <- renderUI({
  #   ticks  <- c(10,20,30)
  #   sliderInput("sliderBR", "Select number of failed BR in plot:", min=1, max=length(BRs), value=length(BRs), step=1)
  # })
  #totalBR with ggplot
  # output$totalBR <- renderPlot({
  #   df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
  #   if(nrow(df) >0){
  #     ggplot(data=df, aes(reorder(BR, -count), count, group = 1)) + geom_bar(stat="identity") + 
  #       ggtitle("Totale huidige uitval per BR") + xlab("Business Rule nummer") + ylab("Aantal regels")
  #   } else{
  #     return()
  #   }
  # })
  #totalBR with googlevis
  output$totalBRvis <- renderGvis({
    df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
    yvariables <- c()
    if (!is.null(input$whitelisted) && input$whitelisted){
      yvariables <- c(yvariables, "whitelisted")
    }
    if (!is.null(input$failed) && input$failed){
      yvariables <- c(yvariables, "failed")
    }
    if (!is.null(input$confirmed) && input$confirmed){
      yvariables <- c(yvariables, "confirmed")
    }
    if (!is.null(input$unhandled) && input$unhandled){
      yvariables <- c(yvariables, "unhandled")
    }
    if (length(yvariables) > 0){
      gvisColumnChart(df, xvar="BR", yvar=yvariables
                      , options=list(
                      fontSize=9, isStacked=TRUE,
                      vAxes="[{title:'Aantal regels', viewWindowMode:'explicit', fontSize:16}]",#logScale: true viewWindow:{min:0, max:160}
                      hAxes="[{title:'Business Rule nummer', textPosition: 'out', fontSize:16}]",
                      title="Totale huidige uitval per BR", width=1600, height=800,
                      chartArea="{left:70,top:40,width:'90%',height:'80%'}",
                      titleTextStyle="{color:'black',fontName:'Courier',fontSize:16}",
                      bar="{groupWidth:'95%'}")
                   )
    } else { return()}
  })
}

aggregateAndSelectBR <- function(inputDF, sliderInput, cbInput){
  df <- aggregate(list(errors=po_current_errors$Errors, whitelisted=po_current_errors$Whitelisted,
                       failed=po_current_errors$Failed,confirmed=po_current_errors$Confirmed,
                       unhandled=po_current_errors$Unhandled),by=list(BR = po_current_errors$BR.number), FUN=sum)
  df <- df[order(df$errors, decreasing = TRUE),]
  if (!is.null(sliderInput)){
    selectedLength <- sliderInput
    df <- df[1:selectedLength,]
  }
  df$BR <- gsub("PO_0", "", df$BR)
  colnames(df) <- c("BR", "errors", "whitelisted", "failed", "confirmed", "unhandled")
  return(df) 
}  


# format Data Element string to be able to sort
replaceDEString <- function(inputString){
  if (nchar(inputString) == 3){
    return(paste0(substring(inputString, 0, 2), "0", substring(inputString, 3, 3)))
  } else{
    return(inputString)
  }
}

#create shiny app
shinyApp(ui, server)