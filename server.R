### ---------------------------------------------------------------------------
### --- Advanced Search Extension Dashboard
### --- Script: server.R, v. Beta 0.1
### --- Technical Wishlist, WMDE
### ---------------------------------------------------------------------------

### --- Setup
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(xts)
library(dygraphs)
library(ggvis)
library(DT)
library(ggplot2)
library(scales)

### --- Server (Session) Scope
### --------------------------------
setwd('/srv/shiny-server/TW_AdvancedSearchExtension/data')

### --- Fetch data
specSearchKeys <- readRDS("specSearchKeys.Rds")
metrics <- readRDS("metrics.Rds")
projects <- readRDS("projects.Rds")
data24Overview <- readRDS("data24Overview.Rds")
previousDay <- readRDS("previousDay.Rds")
pDay <- readRDS("pDay.Rds")
dataWeekOverview <- readRDS("dataWeekOverview.Rds")
previousWeek <- readRDS("previousWeek.Rds")
lastWeek <- readRDS("lastWeek.Rds")
lastMonth <- readRDS("lastMonth.Rds")
dataMonthOverview <- readRDS("dataMonthOverview.Rds")
previousMonth <- readRDS("previousMonth.Rds")
data24 <- readRDS("data24.Rds")
dataWeek <- readRDS("dataWeek.Rds")
dataMonth <- readRDS("dataMonth.Rds")
keyOccurWeek <- readRDS("keyOccurWeek.Rds")
keyOccurMonth <- readRDS("keyOccurMonth.Rds")
keyOccur3Months <- readRDS("keyOccur3Months.Rds")
specSearchData <- readRDS("specSearchData.Rds")
specSearchKeywordsData <- readRDS("specSearchKeywordsData.Rds")


### --- shinyServer
shinyServer(function(input, output, session) {
  
  
  ### ----------------------------------
  ### --- TAB: Overview
  ### ----------------------------------
  
  ### --- SELECT: update select 'selectMetric_gO24'
  updateSelectizeInput(session,
                       'selectMetric_gO24',
                       choices = metrics,
                       selected = 'Searches',
                       server = TRUE)

  ### --- SELECT: update select 'selectMetric_gOWeek'
  updateSelectizeInput(session,
                       'selectMetric_gOWeek',
                       choices = metrics,
                       selected = 'Searches',
                       server = TRUE)

  ### --- SELECT: update select 'selectMetric_gOMonth'
  updateSelectizeInput(session,
                       'selectMetric_gOMonth',
                       choices = metrics,
                       selected = 'Searches',
                       server = TRUE)
  
  ### --- gO24_plot
  gO24 <- reactive({
    d <- data24Overview %>% select('Timestamp', ifelse((input$selectMetric_gO24 == ''), 'Searches', input$selectMetric_gO24)) 
    colnames(d) <- c('Timestamp', 'Metric')
    d %>%  ggvis(x = ~as.character(Timestamp), y = ~Metric) %>%
      layer_lines(stroke := "firebrick") %>% 
      layer_ribbons(y = ~d$Metric, y2 = -min(d$Metric),fill := "firebrick", fillOpacity := 0.1) %>% 
      layer_points(stroke := "firebrick", fill := "white", size := 25) %>% 
      add_tooltip(function(data){
        paste0("<b>", data[, 2], "</b>")
      }, "hover") %>% 
      add_axis("x", 
               title = "", 
               properties = axis_props(labels = list(angle = 90,
                                                     align = "left", baseline = "middle"))) %>% 
      set_options(width = "85%", height = "350px")
    })
  gO24 %>% bind_shiny('gO24_plot')
  
  ### --- valueBox: gO24_previousDayBox
  # output$gO24_previousDayBox
  output$gO24_previousDayBox <- renderValueBox({
    w <- which(names(previousDay) %in% input$selectMetric_gO24)
    if (length(w) > 0) {
      valueBox(
        value = as.character(previousDay[w]),
        subtitle = paste0("Yesterday: ", pDay),
        icon = icon("bars", lib = "font-awesome"),
        color = "red"
      )
    } else {
      valueBox(
        value = 0,
        subtitle = paste0("Yesterday: ", pDay),
        icon = icon("bars", lib = "font-awesome"),
        color = "red"
      )
    }
  }) # END output$gO24_previousDayBox
  
  ### --- gOWeek_plot
  gOWeek <- reactive({
    d <- dataWeekOverview %>% select('Timestamp', ifelse((input$selectMetric_gOWeek == ''), "Searches", input$selectMetric_gOWeek))
    colnames(d) <- c('Timestamp', 'Metric')
    d %>%  ggvis(x = ~as.character(Timestamp), y = ~Metric) %>%
      layer_lines(stroke := "darkblue") %>%
      layer_ribbons(y = ~d$Metric, y2 = -min(d$Metric),fill := "darkblue", fillOpacity := 0.1) %>%
      layer_points(stroke := "darkblue", fill := "white", size := 25) %>% 
      add_tooltip(function(data){
        paste0("<b>", data[, 2], "</b>")
      }, "hover") %>% 
      add_axis("x", 
               title = "", 
               properties = axis_props(labels = list(angle = 90,
                                                     align = "left", baseline = "middle"))) %>% 
      set_options(width = "85%", height = "350px")
  })
  gOWeek %>% bind_shiny('gOWeek_plot')
  
  ### --- valueBox: gOWeek_previousWeekBox
  # output$gOWeek_previousWeekBox
  output$gOWeek_previousWeekBox <- renderValueBox({
    w <- which(names(previousWeek) %in% input$selectMetric_gOWeek)
    if (length(w) > 0) {
      valueBox(
        value = as.character(previousWeek[w]),
        subtitle = paste0("Last week: ", lastWeek),
        icon = icon("bars", lib = "font-awesome"),
        color = "light-blue"
      )
    } else {
      valueBox(
        value = 0,
        subtitle = paste0("Last week: ", lastWeek),
        icon = icon("bars", lib = "font-awesome"),
        color = "light-blue"
      )
    }
  }) # END output$gOWeek_previousWeekBox
  
  ### --- gOMonth_plot
  gOMonth <- reactive({
    d <- dataMonthOverview %>% select('Timestamp', ifelse((input$selectMetric_gOMonth == ''), "Searches", input$selectMetric_gOMonth))
    colnames(d) <- c('Timestamp', 'Metric')
    d %>%  ggvis(x = ~as.character(Timestamp), y = ~Metric) %>%
      layer_lines(stroke := "darkorange") %>% 
      layer_ribbons(y = ~d$Metric, y2 = -min(d$Metric),fill := "darkorange", fillOpacity := 0.1) %>%
      layer_points(stroke := "darkorange", fill := "white", size := 25) %>%
      add_tooltip(function(data){
        paste0("<b>", data[, 2], "</b>")
      }, "hover") %>% 
      add_axis("x", 
               title = "", 
               properties = axis_props(labels = list(angle = 90,
                                                     align = "left", baseline = "middle"))) %>% 
      set_options(width = "85%", height = "350px")
  })
  gOMonth %>% bind_shiny('gOMonth_plot')
  
  ### --- valueBox: gOWeek_previousMonthBox
  # output$gOWeek_previousMonthBox
  output$gOWeek_previousMonthBox <- renderValueBox({
    w <- which(names(previousMonth) %in% input$selectMetric_gOMonth)
    if (length(w) > 0) {
      valueBox(
        value = as.character(previousMonth[[w]]),
        subtitle = paste0("Previous Month: ", month.name[as.numeric(lastMonth)]),
        icon = icon("bars", lib = "font-awesome"),
        color = "yellow"
      )
    } else {
      valueBox(
        value = 0,
        subtitle = paste0("Previous Month: ", month.name[as.numeric(lastMonth)]),
        icon = icon("bars", lib = "font-awesome"),
        color = "yellow"
      )
    }
  }) # END output$gOWeek_previousMonthBox
  
  
  
  ### ----------------------------------
  ### --- TAB: Projects
  ### ----------------------------------
  
  ### --- SELECT: update select 'selectMetric_gO24_proj'
  updateSelectizeInput(session,
                       'selectMetric_gO24_proj',
                       choices = metrics,
                       selected = 'Searches',
                       server = TRUE)
  
  ### --- SELECT: update select 'selectMetric_gOWeek_proj'
  updateSelectizeInput(session,
                       'selectMetric_gOWeek_proj',
                       choices = metrics,
                       selected = 'Searches',
                       server = TRUE)
  
  ### --- SELECT: update select 'selectMetric_gOMonth_proj'
  updateSelectizeInput(session,
                       'selectMetric_gOMonth_proj',
                       choices = metrics,
                       selected = 'Searches',
                       server = TRUE)
  
  ### --- SELECT: update select 'selectProject_gO24_proj'
  updateSelectizeInput(session,
                       'selectProject_gO24_proj',
                       choices = projects,
                       selected = 'enwiki',
                       server = TRUE)
  
  ### --- SELECT: update select 'selectProject_week_proj'
  updateSelectizeInput(session,
                       'selectProject_week_proj',
                       choices = projects,
                       selected = 'enwiki',
                       server = TRUE)
  
  ### --- SELECT: update select 'selectProject_3months_proj'
  updateSelectizeInput(session,
                       'selectProject_3months_proj',
                       choices = projects,
                       selected = 'enwiki',
                       server = TRUE)
  
  ### --- gO24_plot projects
  gO24_proj <- reactive({
    d <- data24 %>%
      filter(Wiki %in% input$selectProject_gO24_proj) %>%
      select('Timestamp', ifelse((input$selectMetric_gO24_proj == ''), 'Searches', input$selectMetric_gO24_proj))
    colnames(d) <- c('Timestamp', 'Metric')
    d %>%  ggvis(x = ~as.character(Timestamp), y = ~Metric) %>%
      layer_lines(stroke := "firebrick") %>% 
      layer_ribbons(y = ~d$Metric, y2 = -min(d$Metric),fill := "firebrick", fillOpacity := 0.1) %>%
      layer_points(stroke := "firebrick", fill := "white", size := 25) %>% 
      add_tooltip(function(data){
        paste0("<b>", data[, 2], "</b>")
      }, "hover") %>% 
      add_axis("x", 
               title = "", 
               properties = axis_props(labels = list(angle = 90,
                                                     align = "left", baseline = "middle"))) %>% 
      set_options(width = "85%", height = "350px")
  })
  gO24_proj %>% bind_shiny('gO24_proj_plot')
  
  ### --- valueBox: gO24_previousDayBox_proj
  # output$gO24_previousDayBox_proj
  output$gO24_previousDayBox_proj <- renderValueBox({
    d <- data24 %>%
      filter(Wiki %in% input$selectProject_gO24_proj) %>% 
      select(ifelse((input$selectMetric_gO24_proj == ''), 'Searches', input$selectMetric_gO24_proj)) 
    d <- sum(d[, 1])
    if (length(d) > 0) {
      valueBox(
        value = d,
        subtitle = paste0("Yesterday: ", pDay),
        icon = icon("bars", lib = "font-awesome"),
        color = "red"
      )
    } else {
      valueBox(
        value = 0,
        subtitle = paste0("Yesterday: ", pDay),
        icon = icon("bars", lib = "font-awesome"),
        color = "red"
      )
    }
    
  }) # END output$gO24_previousDayBox
  
  ### --- gOWeek_plot_proj
  gOWeek_proj <- reactive({
    d <- dataWeek %>%
      filter(Wiki %in% input$selectProject_week_proj) %>%
      select('Timestamp', ifelse((input$selectMetric_gOWeek_proj == ''), 'Searches', input$selectMetric_gOWeek_proj))
    colnames(d) <- c('Timestamp', 'Metric')
    d %>%  ggvis(x = ~as.character(Timestamp), y = ~Metric) %>%
      layer_lines(stroke := "darkblue") %>% 
      layer_ribbons(y = ~d$Metric, y2 = -min(d$Metric),fill := "darkblue", fillOpacity := 0.1) %>%
      layer_points(stroke := "darkblue", fill := "white", size := 25) %>%
      add_tooltip(function(data){
        paste0("<b>", data[, 2], "</b>")
      }, "hover") %>% 
      add_axis("x", 
               title = "", 
               properties = axis_props(labels = list(angle = 90,
                                                     align = "left", baseline = "middle"))) %>% 
      set_options(width = "85%", height = "350px")
  })
  gOWeek_proj %>% bind_shiny('gOWeek_plot_proj')
  
  ### --- valueBox: gOWeek_previousWeekBox
  # output$gOWeek_previousWeekBox
  output$gOWeek_previousWeekBox_proj <- renderValueBox({
    d <- dataWeek %>% 
      filter(Wiki %in% input$selectProject_week_proj) %>% 
      select(ifelse((input$selectMetric_gOWeek_proj == ''), 'Searches', input$selectMetric_gOWeek_proj)) 
    d <- sum(d[, 1])
    if (length(d) > 0) {
      valueBox(
        value = d,
        subtitle = paste0("Last week: ", lastWeek),
        icon = icon("bars", lib = "font-awesome"),
        color = "light-blue"
      )
    } else {
      valueBox(
        value = 0,
        subtitle = paste0("Last week: ", lastWeek),
        icon = icon("bars", lib = "font-awesome"),
        color = "light-blue"
      )
    }
  }) # END output$gOWeek_previousWeekBox_proj
  
  ### --- gOMonth_plot
  gOMonth_proj <- reactive({
    d <- dataMonth %>%
      filter(Wiki %in% input$selectProject_3months_proj) %>%
      select('Timestamp', ifelse((input$selectMetric_gOMonth_proj == ''), 'Searches', input$selectMetric_gOMonth_proj))
    colnames(d) <- c('Timestamp', 'Metric')
    d %>%  ggvis(x = ~as.character(Timestamp), y = ~Metric) %>% 
      layer_lines(stroke := "darkorange") %>% 
      layer_ribbons(y = ~d$Metric, y2 = -min(d$Metric),fill := "darkorange", fillOpacity := 0.1) %>%
      layer_points(stroke := "darkorange", fill := "white", size := 25) %>%
      add_tooltip(function(data){
        paste0("<b>", data[, 2], "</b>")
      }, "hover") %>% 
      add_axis("x", 
               title = "", 
               properties = axis_props(labels = list(angle = 90,
                                                     align = "left", baseline = "middle"))) %>% 
      set_options(width = "85%", height = "350px")
  })
  gOMonth_proj %>% bind_shiny('gOMonth_plot_proj')
  
  ### --- valueBox: gOWeek_previousMonthBox
  # output$gOWeek_previousMonthBox
  output$gOWeek_previousMonthBox_proj <- renderValueBox({
    d <- dataMonth %>% 
      filter(Wiki %in% input$selectProject_3months_proj) %>% 
      select(ifelse((input$selectMetric_gOMonth_proj == ''), 'Searches', input$selectMetric_gOMonth_proj)) 
    d <- d[3, 1]
    if (length(d) > 0) {
      valueBox(
        value = d,
        subtitle = paste0("Previous Month: ", month.name[as.numeric(lastMonth)]),
        icon = icon("bars", lib = "font-awesome"),
        color = "yellow"
      )
    } else {
      valueBox(
        value = 0,
        subtitle = paste0("Previous Month: ", month.name[as.numeric(lastMonth)]),
        icon = icon("bars", lib = "font-awesome"),
        color = "yellow"
      )
    }
  }) # END output$gOWeek_previousMonthBox
  
  
  ### --- Correlations Tab
  
  ### --- output$keyOccurWeek
  output$keyOccurWeek <- DT::renderDataTable({
    datatable(keyOccurWeek,
              options = list(
                dom = 't',
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  # - output$keyOccurWeekCorPlot
  output$keyOccurWeekCorPlot <- renderPlot({
    keyOccurWeekCor <- melt(keyOccurWeek)
    keyOccurWeekCor$Keywords <- gsub("Event_", "", keyOccurWeekCor$Keywords)
    keyOccurWeekCor$variable <- gsub("Event_", "", keyOccurWeekCor$variable)
    ggplot(data = keyOccurWeekCor, aes(variable, Keywords, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "blue", 
                           space = "Lab", 
                           name = "Co-occurence") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 10, hjust = 1)) +
      theme(axis.text.y = element_text(angle = 0, vjust = 1, 
                                       size = 10, hjust = 1)) +
      coord_fixed() + 
      geom_text(aes(variable, Keywords, label = value), color = "black", size = 4) +
      theme(legend.position = "none") + 
      xlab('') + ylab('')
  })  %>% withProgress(message = 'Generating data',
                       min = 0,
                       max = 1,
                       value = 1, {incProgress(amount = 1)})
  
  
  ### --- output$keyOccurMonth
  output$keyOccurMonth <- DT::renderDataTable({
    datatable(keyOccurMonth,
              options = list(
                dom = 't', 
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  # - output$keyOccurMonthCorPlot
  output$keyOccurMonthCorPlot <- renderPlot({
    keyOccurMonthCor <- melt(keyOccurMonth)
    keyOccurMonthCor$Keywords <- gsub("Event_", "", keyOccurMonthCor$Keywords)
    keyOccurMonthCor$variable <- gsub("Event_", "", keyOccurMonthCor$variable)
    ggplot(data = keyOccurMonthCor, aes(variable, Keywords, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "blue", 
                          space = "Lab", 
                          name = "Co-occurence") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 10, hjust = 1)) +
      theme(axis.text.y = element_text(angle = 0, vjust = 1, 
                                       size = 10, hjust = 1)) +
      coord_fixed() + 
      geom_text(aes(variable, Keywords, label = value), color = "black", size = 4) +
      theme(legend.position = "none") + 
      xlab('') + ylab('')
  })  %>% withProgress(message = 'Generating data',
                       min = 0,
                       max = 1,
                       value = 1, {incProgress(amount = 1)})
  
  ### --- output$keyOccur3Months
  output$keyOccur3Months <- DT::renderDataTable({
    datatable(keyOccur3Months,
              options = list(
                dom = 't',
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  # - output$keyOccur3MonthsCorPlot
  output$keyOccur3MonthsCorPlot <- renderPlot({
    keyOccur3MonthsCor <- melt(keyOccur3Months)
    keyOccur3MonthsCor$Keywords <- gsub("Event_", "", keyOccur3MonthsCor$Keywords)
    keyOccur3MonthsCor$variable <- gsub("Event_", "", keyOccur3MonthsCor$variable)
    ggplot(data = keyOccur3MonthsCor, aes(variable, Keywords, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "blue", 
                          space = "Lab", 
                          name = "Co-occurence") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 10, hjust = 1)) +
      theme(axis.text.y = element_text(angle = 0, vjust = 1, 
                                       size = 10, hjust = 1)) +
      coord_fixed() + 
      geom_text(aes(variable, Keywords, label = value), color = "black", size = 4) +
      theme(legend.position = "none") + 
      xlab('') + ylab('')
  })  %>% withProgress(message = 'Generating data',
                       min = 0,
                       max = 1,
                       value = 1, {incProgress(amount = 1)})
  
  ### ----------------------------------
  ### --- TAB: Special:Search
  ### ----------------------------------
  
  output$SpecialSearchCount <- renderDygraph({
    
    # - plot data.frame
    pFrame <- specSearchData %>% 
      select(SpecialSearch_Count, Date)
    pFrame$Date <- as.POSIXct(pFrame$Date)
    pFrame <- xts(x = pFrame$SpecialSearch_Count, order.by = pFrame$Date)
    # - dygraph
    dygraph(pFrame) %>%
      dySeries("V1", label = "Count") %>%
      dyOptions(colors = "indianred", 
                maxNumberWidth = 20, 
                stackedGraph = F, 
                fillGraph = TRUE, 
                fillAlpha = 0.4, 
                drawPoints = TRUE, 
                pointSize = 2) %>%
      dyLegend(show = "follow") %>%
      dyRangeSelector(height = 50)
  })
  
  output$AdvancedSearchCount <- renderDygraph({
    # - plot data.frame
    pFrame <- specSearchData %>% 
      select(AdvancedSearch_Count, Date)
    pFrame$Date <- as.POSIXct(pFrame$Date)
    pFrame <- xts(x = pFrame$AdvancedSearch_Count, order.by = pFrame$Date)
    # - dygraph
    dygraph(pFrame) %>%
      dySeries("V1", label = "Count") %>%
      dyOptions(colors = "darkorange", 
                maxNumberWidth = 20, 
                stackedGraph = F, 
                fillGraph = TRUE, 
                fillAlpha = 0.4, 
                drawPoints = TRUE, 
                pointSize = 2) %>%
      dyLegend(show = "follow") %>%
      dyRangeSelector(height = 50)
  })
  
  output$AdvancedSearchPercent <- renderDygraph({
    # - plot data.frame
    pFrame <- specSearchData %>% 
      select(Advanced_Percent, Date)
    pFrame$Date <- as.POSIXct(pFrame$Date)
    pFrame <- xts(x = pFrame$Advanced_Percent, order.by = pFrame$Date)
    # - dygraph
    dygraph(pFrame) %>%
      dySeries("V1", label = "Count") %>%
      dyOptions(colors = "deepskyblue", 
                maxNumberWidth = 20, 
                digitsAfterDecimal = 10,
                stackedGraph = F, 
                fillGraph = TRUE, 
                fillAlpha = 0.4, 
                drawPoints = TRUE, 
                pointSize = 2) %>%
      dyLegend(show = "follow") %>%
      dyRangeSelector(height = 50)
  })
  
  output$SpecialSearchOnlyCount <- renderDygraph({
    # - plot data.frame
    pFrame <- specSearchData %>% 
      select(SpecialSearchOnly_Count, Date)
    pFrame$Date <- as.POSIXct(pFrame$Date)
    pFrame <- xts(x = pFrame$SpecialSearchOnly_Count, order.by = pFrame$Date)
    # - dygraph
    dygraph(pFrame) %>%
      dySeries("V1", label = "Count") %>%
      dyOptions(colors = "indianred", 
                maxNumberWidth = 20, 
                stackedGraph = F, 
                fillGraph = TRUE, 
                fillAlpha = 0.4, 
                drawPoints = TRUE, 
                pointSize = 2) %>%
      dyLegend(show = "follow") %>%
      dyRangeSelector(height = 50)
  })
  
  ### ----------------------------------
  ### --- TAB: Search Keywords
  ### ----------------------------------
  
  ### --- SELECT: update select 'keywords'
  updateSelectizeInput(session,
                       'keywords',
                       choices = specSearchKeys,
                       selected = specSearchKeys[round(runif(1, 1, length(specSearchKeys)))],
                       server = TRUE)
  
  output$SpecialSearchKeywords <- renderDygraph({
    # - plot data.frame
    pFrame <- specSearchKeywordsData %>% 
      filter(keyword %in% input$keywords)
    pFrame$date <- as.POSIXct(pFrame$date)
    pFrame <- pFrame %>% 
      select(specialsearchcount, date)
    pFrame <- xts(x = pFrame$specialsearchcount, order.by = pFrame$date)
    # - dygraph
    dygraph(pFrame) %>%
      dySeries("V1", label = "Count") %>%
      dyOptions(colors = "indianred", 
                maxNumberWidth = 20, 
                stackedGraph = F, 
                fillGraph = TRUE, 
                fillAlpha = 0.4, 
                drawPoints = TRUE, 
                pointSize = 2) %>%
      dyLegend(show = "follow") %>%
      dyRangeSelector(height = 50)
  })
  
  output$AdvancedSearchKeywords <- renderDygraph({
    # - plot data.frame
    pFrame <- specSearchKeywordsData %>% 
      filter(keyword %in% input$keywords)
    pFrame$date <- as.POSIXct(pFrame$date)
    pFrame <- pFrame %>% 
      select(advancedextensioncount, date)
    pFrame <- xts(x = pFrame$advancedextensioncount, order.by = pFrame$date)
    # - dygraph
    dygraph(pFrame) %>%
      dySeries("V1", label = "Count") %>%
      dyOptions(colors = "deepskyblue", 
                maxNumberWidth = 20, 
                stackedGraph = F, 
                fillGraph = TRUE, 
                fillAlpha = 0.4, 
                drawPoints = TRUE, 
                pointSize = 2) %>%
      dyLegend(show = "follow") %>%
      dyRangeSelector(height = 50)
  })

  }) ### --- END shinyServer




