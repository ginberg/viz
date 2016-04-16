## dashboard.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(googleVis)

#setwd("~/admin/freelance/trifinance/code/viz")
load(file = "testPOError.RData")
types <- c("unhandled","confirmed","failed","whitelisted")
selected<- c("unhandled","confirmed","failed")

dbHeader <- dashboardHeader(title = "DQSS dashboard")
#print(dbHeader)
dbHeader$children[[2]]$children <- tags$a(href='http://deltalloyd.com', tags$img(src='dl.png'))

ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(
    selectInput("system", "System",
                c("Portia" = "po",
                  "Front Arena" = "fa",
                  "Globes" = "gl",
                  "VIS" = "vi")),
    checkboxGroupInput("types","Types",types, selected = selected)
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    htmlOutput("totalBRvis")
  )
)

server <- function(input, output) {
  load(file = "testPOError.RData")
  
  #Consider failed BR only
  BRs <- unique(po_current_errors$BR.number)
  BRs <- BRs[order(BRs)]
  DEs <- unique(po_current_errors$Data.Element)
  DEs <- DEs[order(DEs)]
  
  #totalBR with googlevis
  output$totalBRvis <- renderGvis({
    #print(input$system)
    if (input$system == "po"){
      df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
    } else if  (input$system == "fa"){
      df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
    } else if  (input$system == "gl"){
      df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
    } else if  (input$system == "vi"){
      return()
    }
    if (!is.null(input$types)){
      gvisColumnChart(df, xvar="BR", yvar=rev(input$types)
                      , options=list(
                        fontSize=9, isStacked=TRUE,
                        vAxes="[{title:'Aantal regels', viewWindowMode:'explicit', fontSize:16}]",#logScale: true viewWindow:{min:0, max:160}
                        hAxes="[{title:'Business Rule nummer', textPosition: 'out', fontSize:16}]",
                        title="Totale huidige uitval per BR", width=1500, height=800,
                        chartArea="{left:50,top:10,width:'90%',height:'90%'}",
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