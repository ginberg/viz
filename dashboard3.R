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
    checkboxGroupInput("types","Types",types, selected = selected),
    uiOutput("sliderBR")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabsetPanel(
      tabPanel("Tab1", plotOutput("totalBR", height = 400, width = 1200, hover = hoverOpts(id ="plot_hover")), verbatimTextOutput("hover_info")), 
      tabPanel("Tab2", htmlOutput("totalBRvis"))
      )
    
    #plotOutput("totalBR", height = 400, width = 1200)
    #htmlOutput("totalBRvis")
  )
)

server <- function(input, output) {
  load(file = "testPOError.RData")
  
  #Consider failed BR only
  BRs <- unique(po_current_errors$BR.number)
  BRs <- BRs[order(BRs)]
  DEs <- unique(po_current_errors$Data.Element)
  DEs <- DEs[order(DEs)]
  
  #df <- "old"
  
  output$sliderBR <- renderUI({
     ticks  <- c(10,20,30)
     sliderInput("sliderBR", "Select failed BR", min=1, max=length(BRs), value=length(BRs), step=1)
   })
  
  adjustPlot<- reactive({
    #Adjust dataframes based on input from slider
    aggregateAndSelectBR(po_current_errors, input$sliderBR)
  })
  
  #totalBR with googlevis
  output$totalBRvis <- renderGvis({
    #print(input$system)
    if (input$system == "po"){
      df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
    } else if  (input$system == "fa"){
      df <- aggregateAndSelectBR(po_current_errors, input$slider)
    } else if  (input$system == "gl"){
      df <- aggregateAndSelectBR(po_current_errors, input$slider)
    } else if  (input$system == "vi"){
      return()
    }
    if (!is.null(input$types) && nrow(adjustPlot()) != 0){
      chart <- gvisColumnChart(adjustPlot(), xvar="BR", yvar=rev(input$types)
                      , options=list(
                        fontSize=9, isStacked=TRUE,
                        vAxes="[{title:'Aantal regels', viewWindowMode:'explicit', fontSize:16}]",#logScale: true viewWindow:{min:0, max:160}
                        hAxes="[{title:'Business Rule nummer', textPosition: 'out', fontSize:16}]",
                        title="Totale huidige uitval per BR", width=1500, height=800,
                        chartArea="{left:50,top:10,width:'90%',height:'90%'}",
                        titleTextStyle="{color:'black',fontName:'Courier',fontSize:16}",
                        bar="{groupWidth:'95%'}")
      )
      return(chart)
    } else { return()}
  })
  
  #totalBR with ggplot
  output$totalBR <- renderPlot({
    df <- aggregateAndSelectBR(po_current_errors, input$sliderBR)
    df <- melt(df[,c('BR', rev(input$types))],id.vars = 1)
    df$variable <- factor(df$variable, levels = rev(levels(df$variable)))
    colnames(df) <- c("BR", "category", "value")
    assign("df", df, envir = .GlobalEnv)
    if(nrow(df) >0){
      ggplot(data=df, aes(x = BR, y= value, fill = category, order = as.numeric(category))) + geom_bar(stat="identity") +
        ggtitle("Totale huidige uitval per BR") + xlab("Business Rule nummer") + ylab("Aantal regels") +
        scale_fill_manual(values = c("whitelisted" = "gray", "failed" = "red", "confirmed" = "blue", "unhandled" = "orange"))
    } else{
      return()
    }
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    if(!is.null(hover)){
      point <- nearPoints(df, hover)
      if (nrow(point) == 0) return(NULL)
      print(point)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Car: </b>", rownames(point), "<br/>",
                      "<b> mpg: </b>", point$mpg, "<br/>",
                      "<b> hp: </b>", point$hp, "<br/>",
                      "<b> Distance from left: </b>", left_px, "<b>, from top: </b>", top_px)))
      )
    }
  })
}

aggregateAndSelectBR <- function(inputDF, sliderInput){
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