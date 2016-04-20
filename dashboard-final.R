## dashboard.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(googleVis)
library(reshape2)

#setwd("~/admin/freelance/trifinance/code/viz")
types <- c("unhandled","confirmed","whitelisted")
selected<- c("unhandled","confirmed","whitelisted")

# types2 <- c("unhandled","failed","whitelisted")
# types2 <- c("unhandled","whitelisted", "whitelisted")


dbHeader <- dashboardHeader(title = "DQSS dashboard")
#print(dbHeader)
#dbHeader$children[[2]]$children <- tags$a(href='http://deltalloyd.com', tags$img(src='dl.png'))

ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(
    selectInput("system", "System",
                c("Portia" = "po",
                  "Front Arena" = "fa",
                  "Globes" = "gl",
                  "VIS" = "vi")),
    checkboxGroupInput("types","Types",types, selected = selected),
    uiOutput("sliderBR"),
    uiOutput("sliderDE")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabsetPanel(
      tabPanel("BR", plotOutput("totalBR", height = 600, width = 1200)),
      tabPanel("DE", plotOutput("totalDE", height = 600, width = 1200))
    )
  )
)

server <- function(input, output) {
  load(file = "testPOError.RData")
  load(file = "testFAError.RData")
  names(fa_current_errors) <- tolower(names(fa_current_errors))
  names(po_current_errors) <- tolower(names(po_current_errors))

  output$sliderBR <- renderUI({
    BRs = adjustBR()
    sliderInput("sliderBR", "Select BR size", min=1, max=length(BRs), value=length(BRs), step=1)
  })
  
  output$sliderDE <- renderUI({
    DEs = adjustDE()
    sliderInput("sliderDE", "Select DE size", min=1, max=length(DEs), value=length(DEs), step=1)
  })
  
  #totalBR with ggplot
  output$totalBR <- renderPlot({
    df <- adjustPlot()
    if(!is.null(df) && nrow(df) >0){
      ggplot(data=df, aes(x = BR, y= value, fill = category, order = as.numeric(category))) + geom_bar(stat="identity") +
        ggtitle("Totale huidige uitval per BR") + xlab("Business Rule nummer") + ylab("Aantal regels") +
        scale_fill_manual(values = c("whitelisted" = "gray", "confirmed" = "blue", "unhandled" = "red")) +
        scale_x_discrete(limits=unique(df$BR))
    } else {return()}
  })
  
  #totalDE with ggplot
  output$totalDE <- renderPlot({
    df <- adjustPlotDE()
    if(!is.null(df) && nrow(df) >0){
      ggplot(data=df, aes(x = DE, y= value, fill = category, order = as.numeric(category))) + geom_bar(stat="identity") +
        ggtitle("Totale huidige uitval per DE") + xlab("Data Element") + ylab("Aantal regels") +
        scale_fill_manual(values = c("whitelisted" = "gray", "confirmed" = "blue", "unhandled" = "red")) +
        scale_x_discrete(limits=unique(df$DE))
    }
  })
  
  adjustBR<- reactive({
    #Adjust BRs based on system input
    if(input$system == 'po'){
      BRs <- unique(po_current_errors$br.number)
    }else if(input$system == 'fa'){
      BRs <- unique(fa_current_errors$br.number)
    }
    BRs <- BRs[order(BRs)]
    BRs
  })
  
  adjustDE<- reactive({
    #Adjust DEs based on system input
    if(input$system == 'po'){
      DEs <- unique(po_current_errors$data.element)
    }else if(input$system == 'fa'){
      DEs <- unique(fa_current_errors$data.element)
    }
    DEs <- DEs[order(DEs)]
    DEs
  })
  
  adjustPlot<- reactive({
    #Adjust BRs based on system input
    if(input$system == 'po'){
      df <- aggregateAndSelectBR(po_current_errors, input$sliderBR, input$types)
    }else if(input$system == 'fa'){
      df <- aggregateAndSelectBR(fa_current_errors, input$sliderBR, input$types)
    }
    if(!is.null(input$types)){
      df <- melt(df[,c('BR', rev(input$types))],id.vars = 1)
      df$variable <- factor(df$variable, levels = rev(levels(df$variable)))
      colnames(df) <- c("BR", "category", "value")
      return(df)
    } else{
      return()
    }
  })
  
  adjustPlotDE<- reactive({
    #Adjust BRs based on system input
    if(input$system == 'po'){
      df <- aggregateAndSelectDE(po_current_errors, input$sliderDE, input$types)
    }else if(input$system == 'fa'){
      df <- aggregateAndSelectDE(fa_current_errors, input$sliderDE, input$types)
    }
    if(!is.null(input$types)){
      df <- melt(df[,c('DE', rev(input$types))],id.vars = 1)
      df$variable <- factor(df$variable, levels = rev(levels(df$variable)))
      colnames(df) <- c("DE", "category", "value")
      return(df)
    } else{
      return()
    }
  })
  
}

aggregateAndSelectBR <- function(inputDF, sliderInput, inputTypes){
  df <- aggregate(list(errors=inputDF$errors, whitelisted=inputDF$whitelisted,
                       failed=inputDF$failed,confirmed=inputDF$confirmed,
                       unhandled=inputDF$unhandled),by=list(BR = inputDF$br.number), FUN=sum)
  #order depending on types
  df <- orderDF(df, inputTypes)
  if (!is.null(sliderInput)){
    selectedLength <- sliderInput
    df <- df[1:selectedLength,]
  }
  df$BR <- gsub("PO_0", "", df$BR)
  df$BR <- gsub("FA_0", "", df$BR)
  colnames(df) <- c("BR", "errors", "whitelisted", "failed", "confirmed", "unhandled")
  return(df) 
}  

aggregateAndSelectDE <- function(inputDF, sliderInput, inputTypes){
  df <- aggregate(list(errors=inputDF$errors, whitelisted=inputDF$whitelisted,
                       failed=inputDF$failed,confirmed=inputDF$confirmed,
                       unhandled=inputDF$unhandled),by=list(BR = inputDF$data.element), FUN=sum)
  df <- orderDF(df, inputTypes)
  if (!is.null(sliderInput)){
    selectedLength <- sliderInput
    df <- df[1:selectedLength,]
  }
  colnames(df) <- c("DE", "errors", "whitelisted", "failed", "confirmed", "unhandled")
  print(df)
  return(df) 
}

orderDF <- function(df, inputTypes){
  if(identical(inputTypes, types)){
    df <- df[order(df$errors, decreasing = TRUE),]
  }else if(identical(inputTypes, c("unhandled", "confirmed"))){
    df <- df[order(df$failed, decreasing = TRUE),]
  }else if(identical(inputTypes, c("confirmed", "whitelisted"))){
    df <- df[order(df$confirmed + df$whitelisted, decreasing = TRUE),]
  }else if(identical(inputTypes, c("unhandled", "whitelisted"))){
    df <- df[order(df$unhandled + df$whitelisted, decreasing = TRUE),]
  }else if(identical(inputTypes, c("unhandled"))){
    df <- df[order(df$unhandled, decreasing = TRUE),]
  }else if(identical(inputTypes, c("confirmed"))){
    df <- df[order(df$confirmed, decreasing = TRUE),]
  }else if(identical(inputTypes, c("whitelisted"))){
    df <- df[order(df$whitelisted, decreasing = TRUE),]
  }
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