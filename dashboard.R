## dashboard.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(googleVis)
library(reshape2)
library(plyr)
library(scales)
library(DT)

dashboard_input_path <- "Z:/Shared Data/5501_ProjectDataQuality/3_Output Data/dashboard"
portia_files <- c("Portia_del4a_dqa.csv","Portia_del4b_dqa.csv")
portia_current_date <- "2016-04-12"
fa_files <- c("FrontArena_del4b_dqa.csv") #"FrontArena_del4a_dqa.csv", 
fa_current_date <- "2016-04-13"
globes_files <- c("Globe$_del4b_dqa.csv")
globes_current_date <- "2016-04-26"
vis_files <- c("VIS_del4b_dqa.csv")
vis_current_date <- "2016-02-19"

errors <- "errors"
failed <- "failed"
whitelisted <- "whitelisted"
blacklisted <- "blacklisted"
unhandled <- "unhandled"
types <- c(unhandled, blacklisted, whitelisted)
selected<- c(unhandled, blacklisted, whitelisted)

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
      tabPanel("BR", 
               fluidRow(plotOutput("totalBR", height = 600, width = 1400)),
               fluidRow(
                 box(plotOutput("plotBR", height = 250)),
                 box(uiOutput("selectBR"), dataTableOutput("detailsBR"))
               )),
      tabPanel("DE", 
               fluidRow(plotOutput("totalDE", height = 600, width = 1400)),
               fluidRow(
                 box(plotOutput("plotDE", height = 250)),
                 box(uiOutput("selectDE"), dataTableOutput("detailsDE"))
               )
      )
    )
  )
)

server <- function(input, output) {
  po_errors <- renameColumns(getDFErrors(dashboard_input_path, portia_files))
  po_current_errors <- getDFByDate(po_errors, portia_current_date)
  fa_errors <- renameColumns(getDFErrors(dashboard_input_path, fa_files))
  fa_current_errors <- getDFByDate(fa_errors, fa_current_date)
  gl_errors <- renameColumns(getDFErrors(dashboard_input_path, globes_files))
  gl_current_errors <- getDFByDate(gl_errors, globes_current_date)
  vis_errors <- renameColumns(getDFErrors(dashboard_input_path, vis_files))
  vis_current_errors <- getDFByDate(vis_errors, vis_current_date)

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
        scale_fill_manual(values = c(whitelisted = "gray", blacklisted = "blue", unhandled = "red")) +
        scale_x_discrete(limits=unique(df$BR))
    } else {return()}
  })
  
  output$detailsBR <- DT::renderDataTable({
    df <- adjustDataset()
    df <- df[df$BR.number == input$selectBR, ]
    df <- df[,c("BR.number", "Data.Element", "Source.Date", errors, whitelisted, failed, blacklisted, unhandled)]
    colnames(df) <- c("BR", "DE", "Date", "total", whitelisted, "failed", blacklisted, unhandled)
    rownames(df) <- NULL
    DT::datatable(df, options = list(paging = FALSE, searching = FALSE, info = FALSE))
  })

  #totalDE with ggplot
  output$totalDE <- renderPlot({
    df <- adjustPlotDE()
    if(!is.null(df) && nrow(df) >0){
      ggplot(data=df, aes(x = DE, y= value, fill = category, order = as.numeric(category))) + geom_bar(stat="identity") +
        ggtitle("Totale huidige uitval per DE") + xlab("Data Element") + ylab("Aantal regels") +
        scale_fill_manual(values = c(whitelisted = "gray", blacklisted = "blue", unhandled = "red")) +
        scale_x_discrete(limits=unique(df$DE))
    }
  })
  
  output$detailsDE <- DT::renderDataTable({
    df <- adjustDataset()
    df <- df[df$Data.Element == input$selectDE, ]
    df <- df[,c("BR.number", "Data.Element", "Source.Date", "errors", whitelisted, "failed", blacklisted, "unhandled")]
    colnames(df) <- c("BR", "DE", "Date", "total", whitelisted, "failed", blacklisted, unhandled)
    rownames(df) <- NULL
    DT::datatable(df, options = list(paging = FALSE, searching = FALSE, info = FALSE))
  })
  
  adjustBR<- reactive({
    #Adjust BRs based on system input
    df <- getCurrentErrors(input$system)
    df <- subsetDF(df, input$types)
    BRs <- unique(df$BR.number)
    return(BRs[order(BRs)])
  })
  
  adjustDE<- reactive({
    #Adjust DEs based on system input
    df <- getCurrentErrors(input$system)
    df <- subsetDF(df, input$types)
    DEs <- unique(df$Data.Element)
    return(DEs[order(DEs)])
  })
  
  adjustPlot<- reactive({
    #Adjust BRs based on system input
    df <- aggregateAndSelectBR(getCurrentErrors(input$system), input$sliderBR, input$types)
    if(!is.null(input$types)){
      df <- subsetDF(df, input$types)
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
    df <- aggregateAndSelectDE(getCurrentErrors(input$system), input$sliderDE, input$types)
    if(!is.null(input$types)){
      df <- subsetDF(df, input$types)
      df <- melt(df[,c('DE', rev(input$types))],id.vars = 1)
      df$variable <- factor(df$variable, levels = rev(levels(df$variable)))
      colnames(df) <- c("DE", "category", "value")
      return(df)
    } else{
      return()
    }
  })
  
  adjustDataset<- reactive({
    #Adjust BRs based on system input
    return(getErrors(input$system))
  })
  
  #time series BR - selection and plot
  output$selectBR <- renderUI({
    selectInput("selectBR", "selectBR", as.list(adjustBR()))
  })
  output$plotBR <- renderPlot({
    df <- adjustDataset()
    createBRDiffPlot(df, input$selectBR)
  })
  
  #time series DE
  output$selectDE <- renderUI({
    selectInput("selectDE", "selectDE", as.list(adjustDE()))
  })
  output$plotDE <- renderPlot({
    df <- adjustDataset()
    createDEDiffPlot(df, input$selectDE)
  })
  
  getCurrentErrors <- function(inputSystem){
    if(inputSystem == 'po'){
      df <- po_current_errors
    }else if(inputSystem == 'fa'){
      df <- fa_current_errors
    }else if(inputSystem == 'gl'){
      df <- gl_current_errors
    }else if(inputSystem == 'vi'){
      df <- vis_current_errors
    }
    return(df)
  }
  
  getErrors <- function(inputSystem){
    if(inputSystem == 'po'){
      df <- po_errors
    }else if(inputSystem == 'fa'){
      df <- fa_errors
    }else if(inputSystem == 'gl'){
      df <- gl_errors
    }else if(inputSystem == 'vi'){
      df <- vis_errors
    }
    return(df)
  }
  
}

#aggregate failed of DF by BR and select the top x where x is specified in slider
#notice: don' t double count lines for each dataelement, so use mean instead of sum
aggregateAndSelectBR <- function(inputDF, sliderInput, inputTypes){
  df <- aggregate(list(errors=inputDF$errors, whitelisted=inputDF$whitelisted,
                       failed=inputDF$failed,blacklisted=inputDF$blacklisted,
                       unhandled=inputDF$unhandled),by=list(BR = inputDF$BR.number), FUN=mean)
  #order depending on types
  df <- orderDF(df, inputTypes)
  if (!is.null(sliderInput)){
    selectedLength <- sliderInput
    df <- df[1:selectedLength,]
  }
  colnames(df) <- c("BR", "errors", whitelisted, "failed", blacklisted, unhandled)
  return(df) 
}  

aggregateAndSelectDE <- function(inputDF, sliderInput, inputTypes){
  df <- aggregate(list(errors=inputDF$errors, whitelisted=inputDF$whitelisted,
                       failed=inputDF$failed,blacklisted=inputDF$blacklisted,
                       unhandled=inputDF$unhandled),by=list(BR = inputDF$Data.Element), FUN=sum)
  df <- orderDF(df, inputTypes)
  if (!is.null(sliderInput)){
    selectedLength <- sliderInput
    df <- df[1:selectedLength,]
  }
  colnames(df) <- c("DE", "errors", whitelisted, "failed", blacklisted, unhandled)
  return(df) 
}

#get failed elements from given files
getDFErrors <- function(path, filenames){
  df <- data.frame()
  for (filename in filenames){
    dfRead <- read.csv(file.path(path, filename), stringsAsFactors = FALSE, header = TRUE)  
    df <- rbind(df, dfRead)
  }
  df_failed <- df[df$Errors > 0,]
  df_failed$Data.Element <- sapply(df_failed$Data.Element, replaceDEString)
  df_failed$Source.Date <- as.Date(df_failed$Source.Date, "%Y-%m-%d")
  return(df_failed)
}

#get df elements for given date
getDFByDate <- function(df, sourceDate){
  df$Data.Element <- sapply(df$Data.Element, replaceDEString)
  df_current <- subset(df, Source.Date == as.Date(sourceDate))
  return(df_current)
}

#subset DF by count of inputypes
subsetDF <- function(df, inputTypes){
  if(identical(inputTypes, types)){
    df <- df[df$errors > 0,]
  }else if(identical(inputTypes, c(unhandled, blacklisted))){
    df <- df[df$failed > 0,]
  }else if(identical(inputTypes, c(blacklisted, whitelisted))){
    df <- df[df$blacklisted + df$whitelisted > 0,]
  }else if(identical(inputTypes, c(unhandled, whitelisted))){
    df <- df[df$unhandled + df$whitelisted > 0,]
  }else if(identical(inputTypes, c(unhandled))){
    df <- df[df$unhandled > 0,]
  }else if(identical(inputTypes, c(blacklisted))){
    df <- df[df$blacklisted > 0,]
  }else if(identical(inputTypes, c(whitelisted))){
    df <- df[df$whitelisted > 0,]
  }
  return(df)
}

#order DF based on input types
orderDF <- function(df, inputTypes){
  if(identical(inputTypes, types)){
    df <- df[order(df$errors, decreasing = TRUE),]
  }else if(identical(inputTypes, c(unhandled, blacklisted))){
    df <- df[order(df$failed, decreasing = TRUE),]
  }else if(identical(inputTypes, c(blacklisted, whitelisted))){
    df <- df[order(df$blacklisted + df$whitelisted, decreasing = TRUE),]
  }else if(identical(inputTypes, c(unhandled, whitelisted))){
    df <- df[order(df$unhandled + df$whitelisted, decreasing = TRUE),]
  }else if(identical(inputTypes, c(unhandled))){
    df <- df[order(df$unhandled, decreasing = TRUE),]
  }else if(identical(inputTypes, c(blacklisted))){
    df <- df[order(df$blacklisted, decreasing = TRUE),]
  }else if(identical(inputTypes, c(whitelisted))){
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

#create time series plot of BR
createBRDiffPlot <- function(df, selectInput){
  if(is.null(selectInput))
    return()
  dfSelected <- df[df$BR.number == selectInput,]
  dfSelected <- rename(dfSelected, c("Source.Date" = "Date", "failed" = "failures"))
  if(nrow(df) >0){
    return(ggplot(data=dfSelected, aes(x=Date, y=failures, group = 1)) + geom_point() + geom_line() + ggtitle(paste("Failures over time", selectInput)) +
             scale_x_date(labels = date_format("%b-%Y")))
  } else{
    return()
  }
}

#create time series plot of DE
createDEDiffPlot <- function(df, selectInput){
  if(is.null(selectInput))
    return()
  dfSelected <- df[df$Data.Element == selectInput,]
  dfSelected <- aggregate(dfSelected$failed, by=list(Date=dfSelected$Source.Date), FUN=sum)
  dfSelected <- rename(dfSelected, c("x" = "failures"))
  if(nrow(dfSelected) >0){
    return(ggplot(data=dfSelected, aes(x=Date, y=failures, group = 1)) + geom_point() + geom_line() + 
             ggtitle(paste("Failures over time", selectInput)) + scale_x_date(labels = date_format("%b-%Y"))) 
  } else{
    return()
  }
}

#rename columns
renameColumns <- function(df){
  colnames(df)[colnames(df) == 'Errors'] <- 'errors'
  colnames(df)[colnames(df) == 'Whitelisted'] <- 'whitelisted'
  colnames(df)[colnames(df) == 'Failed'] <- 'failed'
  colnames(df)[colnames(df) == 'Blacklisted'] <- 'blacklisted'
  colnames(df)[colnames(df) == 'Unhandled'] <- 'unhandled'
  df$BR.number <- gsub("PO_0", "", df$BR.number)
  df$BR.number <- gsub("FA_0", "", df$BR.number)
  df$BR.number <- gsub("GL_", "", df$BR.number)
  return(df)
}

#create shiny app
shinyApp(ui, server)