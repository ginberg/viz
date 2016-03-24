## dashboard.R ##
library(shinydashboard)
library(ggplot2)
library(googleVis)

ui <- dashboardPage(
  dashboardHeader(title = "DQSS dashboard"),
  dashboardSidebar(sidebarMenu
    (
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Business Rules", tabName = "Business Rules", icon = icon("th"))
    ),
    width = 150),
  dashboardBody(
    #tabItems(
      # dashboard
      #tabItem(tabName = "dashboard",
        # Boxes need to be put in a row (or column)
        fluidRow(
          column(width = 3, textOutput("summaryBR")),
          column(width = 9, uiOutput("sliderBR"))
        ),
        fluidRow(
          box(plotOutput("totalBR", height = 300, width = 1200))
        ),
        fluidRow(
          htmlOutput("totalBRvis")
        ),
        fluidRow(
          column(width = 3, textOutput("summaryDE")),
          column(width = 9, uiOutput("sliderDE"))
        ),
        fluidRow(
          box(plotOutput("totalDE", height = 300, width = 1200))
        ),
        fluidRow(
          box(plotOutput("plotBR", height = 250)),
          box(uiOutput("selectBR"))
        ),
        fluidRow(
          box(plotOutput("plotDE", height = 250)),
          box(uiOutput("selectDE"))
        )
      )
    #)
  #)
)

server <- function(input, output) {
  if (exists("changed_to_subdir") & changed_to_subdir){
    dashboard_input_path <- "output/shiny"
  } else{
    dashboard_input_path <- "../output/shiny"
  }
  po1 <- read.csv(file.path(dashboard_input_path, "Portia_del4a_dqa.csv"), stringsAsFactors = FALSE, header = TRUE)
  po2 <- read.csv(file.path(dashboard_input_path, "Portia_del4b_dqa.csv"), stringsAsFactors = FALSE, header = TRUE)
  po <- rbind(po1, po2)
  po_failed <- po[po$Failed > 0,]
  po_failed$Data.Element <- sapply(po_failed$Data.Element, replaceDEString)
  po_current_failed <- po_failed[po_failed$Source.Date == "2016-02-19",]
  
  #Consider failed BR only
  BRs <- unique(po_failed$BR.number)
  BRs <- BRs[order(BRs)]
  DEs <- unique(po_failed$Data.Element)
  DEs <- DEs[order(DEs)]
  
  #Business rules section
  output$summaryBR <- renderText({ 
    paste("Number of failed Business Rules:", length(BRs),
          "Number of failed rules:", sum(po_current_failed$Failed))
  })
  output$sliderBR <- renderUI({
    ticks  <- c(10,20,30)
    sliderInput("sliderBR", "Select number of failed BR in plot:", min=1, max=length(BRs), value=length(BRs), step=1)
  })
  #totalBR with ggplot
  output$totalBR <- renderPlot({
    df <- aggregateAndSelectBR(po_current_failed, input$sliderBR)
    if(nrow(df) >0){
      ggplot(data=df, aes(reorder(BR, -count), count, group = 1)) + geom_bar(stat="identity") + 
        ggtitle("Totale huidige uitval per BR") + xlab("Business Rule nummer") + ylab("Aantal regels")
    } else{
      return()
    }
  })
  #totalBR with googlevis
  output$totalBRvis <- renderGvis({
    df <- aggregateAndSelectBR(po_current_failed, input$sliderBR)
    gvisColumnChart(df, xvar="BR", yvar="count", options=list(
                    title="Hello World", width=1200, height=400,
                    chartArea="{left:70,top:40,width:'90%',height:'90%'}",
                    titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}", bar="{groupWidth:'100%'}"))
  })

  output$plotBR <- renderPlot({
    if(is.null(input$selectBR))
      return()
    df <- po_failed[po_failed$BR.number == input$selectBR,]
    if(nrow(df) >0){
      ggplot(data=df, aes(x=Source.Date, y=Failed, group = 1)) + geom_point() + geom_line() + ggtitle(paste("Verloop uitval", input$selectBR)) 
    } else{
      return()
    }
  })
  #time series
  output$selectBR <- renderUI({
    selectInput("selectBR", "selectBR", as.list(BRs))
  })

  
  #DE section
  output$summaryDE <- renderText({ 
    paste("Number of failed Data Elements:", length(DEs),
          "Number of failed rules:", sum(po_current_failed$Failed))
  })
  output$sliderDE <- renderUI({
    sliderInput("sliderDE", "Select number of failed DE in plot:", min=1, max=length(DEs), value=length(DEs), step=1)
  })
  output$selectDE <- renderUI({
    selectInput("selectDE", "selectDE", as.list(DEs))
  })
  output$totalDE <- renderPlot({
    df <- aggregate(po_current_failed$Failed, by=list(DE= po_current_failed$Data.Element), FUN=sum)
    df <- df[order(df$x, decreasing = TRUE),]
    if (!is.null(input$sliderDE)){
      selectedLength <- input$sliderDE
      df <- df[1:selectedLength,]
    }
    if(nrow(df) >0){
      ggplot(data=df, aes(reorder(DE, -x), y=x, group = 1)) + geom_bar(stat="identity") + 
        ggtitle("Totale huidige uitval per DE") + xlab("Data Element nummer") + ylab("Aantal regels")
    } else{
      return()
    }
  })
  output$plotDE <- renderPlot({
    if(is.null(input$selectDE))
      return()
    df <- po_failed[po_failed$Data.Element == input$selectDE,]
    df <- aggregate(df$Failed, by=list(Date=df$Source.Date), FUN=sum)
    if(nrow(df) >0){
      ggplot(data=df, aes(x=Date, y=x, group = 1)) + geom_point() + geom_line() + ggtitle(paste("Verloop uitval", input$selectDE)) 
    } else{
      return()
    }
  })
  
  output$summaryTOT <- renderText({ 
    paste("Number of failed Business Rules:", length(BRs),
          "Number of failed rules:", sum(po_current_failed$Failed))
  })
}

aggregateAndSelectBR <- function(inputDF, sliderInput){
  df <- aggregate(inputDF$Failed, by=list(BR=inputDF$BR.number), FUN=sum)
  df <- df[order(df$x, decreasing = TRUE),]
  if (!is.null(sliderInput)){
    selectedLength <- sliderInput
    df <- df[1:selectedLength,]
  }
  df$BR <- gsub("PO_0", "", df$BR)
  colnames(df) <- c("BR", "count")
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