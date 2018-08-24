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
dataSet <- read.csv('asExtensionUpdate.csv',
                    header = T, 
                    row.names = 1,
                    stringsAsFactors = F)
### --- constant: projects
projects <- unique(dataSet$wiki)
### --- dataSet$timestamp as.Posixct
dataSet$timestamp <- as.POSIXct(dataSet$timestamp,  tz = "Europe/Berlin")

### --- find keyword co-occurrences
### --- Extract last month data
start1Month <- tail(dataSet$timestamp, 1)
start1Month <- seq(as.Date(start1Month), length = 2, by = "-1 months")[2]
start1Month <- as.POSIXct(paste0(as.character(start1Month), "00:00:00"), tz = "Europe/Berlin")
keywordsMonth <- dataSet[which(dataSet$timestamp >= start1Month), ] %>% 
  select(starts_with('event'))
colnames(keywordsMonth) <- str_to_title(colnames(keywordsMonth))
keyOccurMonth <- matrix(0, nrow = length(colnames(keywordsMonth)), ncol = length(colnames(keywordsMonth)))
for (i in 1:length(colnames(keywordsMonth))) {
  for (j in 1:length(colnames(keywordsMonth))) {
    keyOccurMonth[i, j] <- sum(keywordsMonth[, i] + keywordsMonth[, j] == 2) 
  } 
}
diag(keyOccurMonth) <- 0
keyOccurMonth <- as.data.frame(keyOccurMonth)
colnames(keyOccurMonth) <- colnames(keywordsMonth)
keyOccurMonth$Keywords <- colnames(keyOccurMonth)

### --- find keyword co-occurrences
### --- Extract last week data
start1Week <- tail(dataSet$timestamp, 1)
start1Week <- seq(as.Date(start1Week), length = 2, by = "-6 day")[2]
start1Week <- as.POSIXct(paste0(as.character(start1Week), "00:00:00"), tz = "Europe/Berlin")
keywordsWeek <- dataSet[which(dataSet$timestamp >= start1Week), ] %>% 
  select(starts_with('event'))
colnames(keywordsWeek) <- str_to_title(colnames(keywordsWeek))
keyOccurWeek <- matrix(0, nrow = length(colnames(keywordsWeek)), ncol = length(colnames(keywordsWeek)))
for (i in 1:length(colnames(keywordsWeek))) {
  for (j in 1:length(colnames(keywordsWeek))) {
    keyOccurWeek[i, j] <- sum(keywordsWeek[, i] + keywordsWeek[, j] == 2) 
  } 
}
diag(keyOccurWeek) <- 0
keyOccurWeek <- as.data.frame(keyOccurWeek)
colnames(keyOccurWeek) <- colnames(keywordsWeek)
keyOccurWeek$Keywords <- colnames(keyOccurWeek)

### --- find keyword co-occurrences
### --- Extract all (3 months) data
keywords3Months <- dataSet %>% 
  select(starts_with('event'))
colnames(keywords3Months) <- str_to_title(colnames(keywords3Months))
keyOccur3Months <- matrix(0, nrow = length(colnames(keywords3Months)), ncol = length(colnames(keywords3Months)))
for (i in 1:length(colnames(keywords3Months))) {
  for (j in 1:length(colnames(keywords3Months))) {
    keyOccur3Months[i, j] <- sum(keywords3Months[, i] + keywords3Months[, j] == 2) 
  } 
}
diag(keyOccur3Months) <- 0
keyOccur3Months <- as.data.frame(keyOccur3Months)
colnames(keyOccur3Months) <- colnames(keywords3Months)
keyOccur3Months$Keywords <- colnames(keyOccur3Months)


### --- wrangle dataSet
dataSet$timestamp <- as.character(dataSet$timestamp)
dataSet <- dataSet %>% 
  select(timestamp, wiki, starts_with('event'))
dataSet$year <- str_sub(dataSet$timestamp, 1, 4)
dataSet$month <- str_sub(dataSet$timestamp, 6, 7)
dataSet$day <- str_sub(dataSet$timestamp, 9, 10)
dataSet$hour <- str_sub(dataSet$timestamp, 12, 13)
dataSet$minute <- str_sub(dataSet$timestamp, 15, 16)
dataSet$second <- str_sub(dataSet$timestamp, 18, 19)
dataSet <- dataSet %>%
  arrange(year, month, day, hour, minute, second)
# - lower resolution to one hour:
dataSet$minute <- "00"
dataSet$second <- "00"
dates <- paste(dataSet$year, dataSet$month, dataSet$day, sep = "-")
times <- paste(dataSet$hour, dataSet$minute, dataSet$second, sep = ":")
dataSet$timestamp <- paste(dates, times, sep = " ")
dataSet$timestamp <- as.POSIXlt(dataSet$timestamp, tz = "Europe/Berlin")

### --- Extract 24h data
data24 <- dataSet[which(dataSet$timestamp >= (dataSet$timestamp[dim(dataSet)[1]] - 24*60*60)), ]
# - aggregate:
data24$timestamp <- as.character(data24$timestamp)
nSearches24 <- data24 %>%  
  group_by(wiki, timestamp) %>%
  summarise(searches = n())
nSearches24 <- as.numeric(nSearches24$searches)
data24 <- data24 %>%
  group_by(wiki, timestamp) %>%
  summarise_at(vars(starts_with('event')), sum)
data24$searches <- nSearches24
data24 <- arrange(data24, timestamp, wiki)
data24$timestamp <- as.POSIXct(data24$timestamp, tz = "GMT")
# - fill in; full timestamp sequence
tsSeq <- seq(from = data24$timestamp[1], length.out = 25, by = "hours")
data24Full <- expand.grid(tsSeq, unique(data24$wiki))
colnames(data24Full) <- c('timestamp', 'wiki')
data24Full$wiki <- as.character(data24Full$wiki)
data24 <- left_join(data24Full, data24, 
                        by = c('timestamp', 'wiki'))
data24[is.na(data24)] <- 0
colnames(data24) <- str_to_title(colnames(data24))
rm(data24Full)
data24Overview <- data24 %>%
  select(-Wiki) %>% 
  group_by(Timestamp) %>% 
  summarise_all(sum)

### --- Extract weekly data
dataWeek <- dataSet[which(dataSet$timestamp >= (dataSet$timestamp[dim(dataSet)[1]] - 6*24*60*60)), ]
# - lower resolution to day
dataWeek$timestamp <- as.character(dataWeek$timestamp)
dataWeek$timestamp <- sapply(dataWeek$timestamp, function(x) {
  strsplit(x, split = " ", fixed = T)[[1]][1]
})
# - aggregate:
nSearchesWeek <- dataWeek %>%  
  group_by(wiki, timestamp) %>%
  summarise(searches = n())
nSearchesWeek <- as.numeric(nSearchesWeek$searches)
dataWeek <- dataWeek %>%
  group_by(wiki, timestamp) %>%
  summarise_at(vars(starts_with('event')), sum)
dataWeek$searches <- nSearchesWeek
dataWeek <- arrange(dataWeek, timestamp, wiki)
dataWeek$timestamp <- as.POSIXct(dataWeek$timestamp, tz = "Europe/Berlin")
# - fill in; full timestamp sequence
tsSeq <- seq(from = dataWeek$timestamp[1], length.out = 7, by = "days")
dataWeekFull <- expand.grid(tsSeq, unique(dataWeek$wiki))
colnames(dataWeekFull) <- c('timestamp', 'wiki')
dataWeekFull$wiki <- as.character(dataWeekFull$wiki)
dataWeek <- left_join(dataWeekFull, dataWeek, 
                    by = c('timestamp', 'wiki'))
dataWeek[is.na(dataWeek)] <- 0
colnames(dataWeek) <- str_to_title(colnames(dataWeek))
rm(dataWeekFull)
dataWeekOverview <- dataWeek %>%
  select(-Wiki) %>% 
  group_by(Timestamp) %>% 
  summarise_all(sum)

### --- Extract monthly data
dataMonth <- dataSet[which(dataSet$timestamp >= (dataSet$timestamp[dim(dataSet)[1]] - 3*30*24*60*60)), ]
# - lower resolution to month
dataMonth$timestamp <- as.character(dataMonth$timestamp)
dataMonth$timestamp <- sapply(dataMonth$timestamp, function(x) {
  paste(strsplit(
    strsplit(x, split = " ", fixed = T)[[1]][1],
    split = "-",
    fixed = T)[[1]][1:2],
    collapse = "-")
})
# - aggregate:
nSearchesMonth <- dataMonth %>%  
  group_by(wiki, timestamp) %>%
  summarise(searches = n())
nSearchesMonth <- as.numeric(nSearchesMonth$searches)
dataMonth <- dataMonth %>%
  group_by(wiki, timestamp) %>%
  summarise_at(vars(starts_with('event')), sum, rm.na = T)
dataMonth$searches <- nSearchesMonth
dataMonth <- arrange(dataMonth, timestamp, wiki)
dataMonth$timestamp <- paste0(dataMonth$timestamp, "-01")
dataMonth$timestamp <- as.POSIXct(dataMonth$timestamp, tz = "Europe/Berlin")
# - fill in; full timestamp sequence
tsSeq <- seq(from = dataMonth$timestamp[1], length.out = 3, by = "months")
dataMonthFull <- expand.grid(tsSeq, unique(dataMonth$wiki))
colnames(dataMonthFull) <- c('timestamp', 'wiki')
dataMonthFull$wiki <- as.character(dataMonthFull$wiki)
dataMonth <- left_join(dataMonthFull, dataMonth, 
                      by = c('timestamp', 'wiki'))
dataMonth[is.na(dataMonth)] <- 0
colnames(dataMonth) <- str_to_title(colnames(dataMonth))
dataMonth$Timestamp <- sapply(dataMonth$Timestamp, function(x) {
  gsub("-01$", "", x)
})
rm(dataMonthFull)
dataMonthOverview <- dataMonth %>%
  select(-Wiki) %>% 
  group_by(Timestamp) %>% 
  summarise_all(sum)

### --- previous day statistics
previousDay <- data24Overview %>% 
  select(-Timestamp) %>% 
  colSums()
names(previousDay) <- str_to_title(names(previousDay))
pDay <- as.POSIXlt(Sys.time(), tz = "Europe/Berlin")
pDay$mday <- pDay$mday - 1
pDay <- paste0(as.character(pDay$mday), ". ", month.name[month(pDay)])

### --- previous week statistics
previousWeek <- dataWeekOverview %>% 
  select(-Timestamp) %>% 
  colSums
names(previousWeek) <- str_to_title(names(previousWeek))
lastWeek <- paste0(dataWeek$Timestamp[1], " - ", tail(dataWeek$Timestamp, 1))

### --- previous month statistics
previousMonth <- list()
lastMonth <- strftime(Sys.time() - 4*7*24*60*60, format = "%m")
w <- which(strftime(dataSet$timestamp, format = "%m") == lastMonth)
previousMonth$searches <- length(w)
wCols <- which(grepl("^event", colnames(dataSet)))
wSums <- colSums(dataSet[w, wCols])
previousMonth <- append(previousMonth, wSums)
names(previousMonth) <- str_to_title(names(previousMonth))

### --- Special:Search Data
specSearchData <- fread('cirrusSearchUpdate.csv', 
                        header = T)
specSearchData$V1 <- NULL
# - fix month and day, produce date
specSearchData$month <- sapply(specSearchData$month, function(x) {
  if (nchar(x) == 1) {
    paste0('0', x)
  } else {x}
})
specSearchData$day <- sapply(specSearchData$day, function(x) {
  if (nchar(x) == 1) {
    paste0('0', x)
  } else {x}
})
specSearchData$date <- paste(specSearchData$year, specSearchData$month, specSearchData$day, sep = "-")
specSearchData[, 1:3] <- NULL
colnames(specSearchData) <- c('SpecialSearch_Count', 'AdvancedSearch_Count', 'Date')
specSearchData$Advanced_Percent <- specSearchData$AdvancedSearch_Count/specSearchData$SpecialSearch_Count*100
specSearchData$SpecialSearchOnly_Count <- specSearchData$SpecialSearch_Count - specSearchData$AdvancedSearch_Count

### --- Special:Search Keywords Data
specSearchKeywordsData <- fread('cirrusSearchUpdateKeywords.csv',
                                header = T)
specSearchKeywordsData$V1 <- NULL
# - fix month and day, produce date
specSearchKeywordsData$month <- sapply(specSearchKeywordsData$month, function(x) {
  if (nchar(x) == 1) {
    paste0('0', x)
  } else {x}
})
specSearchKeywordsData$day <- sapply(specSearchKeywordsData$day, function(x) {
  if (nchar(x) == 1) {
    paste0('0', x)
  } else {x}
})
specSearchKeywordsData$date <- paste(specSearchKeywordsData$year, specSearchKeywordsData$month, specSearchKeywordsData$day, sep = "-")
specSearchKeywordsData[, 1:3] <- NULL
# - filling-in
specSearchKeywordsData_specSearch <- specSearchKeywordsData %>% 
  select(date, keyword, specialsearchcount) %>% 
  spread(key = date, value = specialsearchcount, fill = 0) %>% 
  gather(key = date, 
         value = specialsearchcount,
         -keyword)
specSearchKeywordsData_advExt <- specSearchKeywordsData %>% 
  select(date, keyword, advancedextensioncount) %>% 
  spread(key = date, value = advancedextensioncount, fill = 0) %>% 
  gather(key = date, 
         value = advancedextensioncount,
         -keyword)
specSearchKeywordsData <- left_join(specSearchKeywordsData_specSearch, specSearchKeywordsData_advExt, 
                                    by = c('keyword', 'date'))

# - keywords to select:
specSearchKeys <- unique(specSearchKeywordsData$keyword)

# - Event_ keywords to select:
metrics <- names(previousMonth)

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
                       selected = projects[round(runif(1, 1, length(projects)))],
                       server = TRUE)
  
  ### --- SELECT: update select 'selectProject_week_proj'
  updateSelectizeInput(session,
                       'selectProject_week_proj',
                       choices = projects,
                       selected = projects[round(runif(1, 1, length(projects)))],
                       server = TRUE)
  
  ### --- SELECT: update select 'selectProject_3months_proj'
  updateSelectizeInput(session,
                       'selectProject_3months_proj',
                       choices = projects,
                       selected = projects[round(runif(1, 1, length(projects)))],
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




