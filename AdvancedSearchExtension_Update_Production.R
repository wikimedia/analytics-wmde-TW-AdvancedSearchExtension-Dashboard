#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Advanced Search Extension Update
### --- Script: AdvancedSearchExtension_Update_Production.R
### --- Author: Goran S. Milovanovic, Data Analyst, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- The script fetches data hourly from log.AdvancedSearchRequest_17379859
### --- and produces the tsv file used on Labs (wikidataconcepts.eqiad.wmflabs)
### --- to update the Extension Report Dashboard.
### ---------------------------------------------------------------------------
### --- RUN FROM: /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/
### --- on crontab
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- AdvancedSearchExtension_Update_Production.R is free software: 
### --- you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- AdvancedSearchExtension_Update_Production.R is distributed in 
### --- the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with AdvancedSearchExtension_Update_Production.R. 
### --- If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- generate asExtensionUpdate.csv update

### --- Setup
library(jsonlite)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)

### -----------------------------------------------------------
### --- produce asExtensionUpdate.csv
### -----------------------------------------------------------

setwd('/home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension')
# - toReport:
print(paste0("Regular SQL update for: Advanced Search Extension on ... ", as.character(Sys.time())))
  

### --- Enlist all Advanced Search Extension schemata
sqlLogIn <- 'mysql --defaults-file=/etc/mysql/conf.d/analytics-research-client.cnf -h analytics-slave.eqiad.wmnet -A -e'
query <- '"use log; show tables from log like \'%AdvancedSearchRequest%\';"'
outFile <- '> /srv/home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/asSchemata.tsv'
sQuery <- paste(sqlLogIn, query, outFile, sep = " ")
system(sQuery, wait = T)
asSchemata <- read.delim('asSchemata.tsv', sep = "\t")
colnames(asSchemata) <- 'table'

### --- Find timespans in Advanced Search Extension schemata
for (i in 1:dim(asSchemata)[1]) {
  query <- paste0('"use log; select min(timestamp), max(timestamp) from ', 
                  asSchemata$table[i],
                  ';"')
  outFile <- paste0('> /srv/home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/timestamps_', 
                    asSchemata$table[i],
                    '.tsv')
  sQuery <- paste(sqlLogIn, query, outFile, sep = " ")
  system(sQuery, wait = T)
}
lF <- list.files()
lF <- lF[grepl("timestamps_", lF)]
timestamps <- vector(mode = "list", length = length(lF))
for (i in 1:length(lF)) {
  timestamps[[i]] <- read.delim(lF[i], sep = "\t", stringsAsFactors = F)
}
timestamps <- rbindlist(timestamps)
timestamps$schema <- gsub(".tsv", "", lF)
timestamps$schema <- gsub("timestamps_", "", timestamps$schema)

### --- Inspect timespans in Advanced Search Extension schemata for NULLs
wNULL <- apply(as.data.frame(timestamps), 1, function(x) {sum(x == "NULL")})
wNULL <- wNULL >= 1
timestamps <- timestamps[!wNULL]

### --- order Advanced Search Extension schemata
timestamps$rank <- order(timestamps$`max.timestamp.`)
# - as.POSIXct: timestamps$`min.timestamp.`, timestamps$`max.timestamp.`
timestamps$`min.timestamp.` <- as.POSIXct(timestamps$`min.timestamp.`, 
                                          tz = "UTC", 
                                          format = "%Y%m%d%H%M%S")
timestamps$`max.timestamp.` <- as.POSIXct(timestamps$`max.timestamp.`, 
                                          tz = "UTC", 
                                          format = "%Y%m%d%H%M%S")
# - UTC to CET
attr(timestamps$`min.timestamp.`, "tzone") <- "Europe/Berlin"
attr(timestamps$`max.timestamp.`, "tzone") <- "Europe/Berlin"

### --- select Advanced Search Extension schemata to fetch
start3Months <- max(timestamps$`max.timestamp.`)
start3Months <- seq(as.Date(start3Months), length = 2, by = "-3 months")[2]
start3Months <- as.POSIXct(paste0(as.character(start3Months), "00:00:00"), tz = "Europe/Berlin")
wStart <- which(timestamps$`min.timestamp.` <= start3Months)
if (length(wStart) == 0) {
  timestamps$selected <- T
} else {
  timestamps$selected <- logical(length(wStart))
  wFirstSelectedMin <- which(wStart)[length(which(wStart))]
  timestamps$selected[wFirstSelectedMin:length(timestamps$selected)] <- T
}
timestamps <- filter(timestamps, selected == T)

### --- fetch selected Advanced Search Extension schemata
updateFile <- vector(mode = "list", length = dim(timestamps)[1])
for (i in 1:dim(timestamps)[1]) {
  query <- paste0('"use log; select * from ', 
                  timestamps$schema[i],
                  ';"')
  outFile <- paste0('> /srv/home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/tempDataScheme.tsv')
  sQuery <- paste(sqlLogIn, query, outFile, sep = " ")
  system(sQuery, wait = T)
  updateFile[[i]] <- fread('tempDataScheme.tsv', sep = "\t")
}
updateFile <- rbindlist(updateFile, use.names = T, fill = T)

### --- select and filter updateFile 
updateFile <- updateFile %>% 
  dplyr::select(id, timestamp, userAgent, wiki, starts_with('event_'))
# - filter bots
updateFile <- dplyr::filter(updateFile,
                            grepl('"is_bot": false', updateFile$userAgent, fixed = T))
# - remove userAgent
updateFile <- dplyr::select(updateFile, -userAgent)
# - timestamp to PosixCt and timezone = Europe/Berlin
updateFile$timestamp <- as.POSIXct(as.character(updateFile$timestamp),
                                   tz = "UTC",
                                   format = "%Y%m%d%H%M%S")
attr(updateFile$timestamp, "tzone") <- "Europe/Berlin"
# - keep three months of data only
start3Months <- tail(updateFile$timestamp, 1)
start3Months <- seq(as.Date(start3Months), length = 2, by = "-3 months")[2]
start3Months <- as.POSIXct(paste0(as.character(start3Months), "00:00:00"), tz = "Europe/Berlin")
updateFile <- dplyr::filter(updateFile,
                            timestamp >= start3Months)
# - remove NAs
updateFile[is.na(updateFile)] <- 0

### --- save updateFile
write.csv(updateFile, 'asExtensionUpdate.csv')

### -----------------------------------------------------------
### --- collect from wmf_raw.CirrusSearchRequestSet
### -----------------------------------------------------------

### --- PART 1: Special:Search/Advanced Extension Search

### --- check update
lF <- list.files()
if ("cirrusSearchUpdate.csv" %in% lF) {
  
  # - load cirrusSearchUpdate.csv
  cirrusSearchUpdate <- read.csv('cirrusSearchUpdate.csv',
                                 header = T,
                                 check.names = F,
                                 row.names = 1,
                                 stringsAsFactors = F)
  # - extract the time of the latest update from cirrusSearchUpdate.csv
  updateY <- tail(cirrusSearchUpdate, 1)$year
  updateM <- tail(cirrusSearchUpdate, 1)$month
  updateD <- tail(cirrusSearchUpdate, 1)$day
  updateDate <- paste(updateY, updateM, updateD, sep = "-")
  updateDate <- as.POSIXct(updateDate)
  updateDate <- updateDate + 60*60*24
  updateDate <- as.integer(strsplit(as.character(updateDate),
                                    split = "-")[[1]])
  # - HiveQL

  # - toReport:
  print(paste0("Running regular update for: ", paste(updateDate, collapse = "-")))

  # - get SpecialSearchCount data:
  hiveQuery <- paste0('"SELECT year, month, day, COUNT(*) AS SpecialSearchCount FROM wmf_raw.CirrusSearchRequestSet WHERE year = ',
                      updateDate[1],
                      ' AND month = ',
                      updateDate[2],
                      ' AND day = ',
                      updateDate[3],
                      ' AND (payload[\'referer\'] LIKE \'%wiki%\') GROUP BY year, month, day;"')
  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdate1.tsv")
  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for SpecialSearchCount data: ", queryCom))

  # - get AdvancedExtensionCount data:
  hiveQuery <- paste0('"SELECT year, month, day, COUNT(*) AS AdvancedExtensionCount FROM wmf_raw.CirrusSearchRequestSet WHERE year = ',
                      updateDate[1],
                      ' AND month = ',
                      updateDate[2],
                      ' AND day = ',
                      updateDate[3],
                      ' AND (payload[\'referer\'] LIKE \'%wiki%\') AND (payload[\'queryString\'] LIKE \'%advancedSearch-current%\') GROUP BY year, month, day;"')
  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdate2.tsv")
  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - to Report:
  print(paste0("Report from running HiveQL for AdvancedExtensionCount data: ", queryCom))

  # - combine data sets and store cirrusSearchUpdate.csv
  cirrusSearchUpdate_1 <- read.delim('cirrusSearchUpdate1.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  cirrusSearchUpdate_2 <- read.delim('cirrusSearchUpdate2.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  cirrusUpdate <- data.frame(year = cirrusSearchUpdate_1$year,
                             month = cirrusSearchUpdate_1$month,
                             day = cirrusSearchUpdate_1$day,
                             specialsearchcount = cirrusSearchUpdate_1$specialsearchcount,
                             advancedextensioncount = cirrusSearchUpdate_2$advancedextensioncount,
                             stringsAsFactors = F)
  # - add update and store:
  cirrusSearchUpdate <- rbind(cirrusSearchUpdate, cirrusUpdate)
  write.csv(cirrusSearchUpdate, "cirrusSearchUpdate.csv")
  # - toReport:
  print(paste0("Completed cirrusSearchUpdate.csv regular update for: ", paste(updateDate, collapse = "-")))
  
  } else {

  # - toReport:
  print("This is the initial intake. Running HiveQL queries and producing the cirrusSearchUpdate.csv file now.")

  # - get SpecialSearchCount data:
  hiveQuery <- '"SELECT year, month, day, hour, COUNT(*) AS SpecialSearchCount FROM wmf_raw.CirrusSearchRequestSet
                  WHERE (year = 2017 OR year = 2018) AND (payload[\'referer\'] LIKE \'%wiki%\') GROUP BY year, month, day, hour;"'
  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdate1.tsv")
  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for SpecialSearchCount data: ", queryCom))
  
  ### --- wait for 10 minutes
  print("Pausing for 10 minutes.")
  Sys.sleep(10*60)

  # - get AdvancedExtensionCount data:
  hiveQuery <- '"SELECT year, month, day, hour, COUNT(*) AS AdvancedExtensionCount FROM wmf_raw.CirrusSearchRequestSet
  WHERE ((year = 2017 OR year = 2018) AND (payload[\'referer\'] LIKE \'%wiki%\') AND
  (payload[\'queryString\'] LIKE \'%advancedSearch-current%\')) GROUP BY year, month, day, hour;"'
  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdate2.tsv")
  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for AdvancedExtensionCount data: ", queryCom))
  
  ### --- wait for 10 minutes
  print("Pausing for 10 minutes.")
  Sys.sleep(10*60)

  # - combine data sets and store cirrusSearchUpdate.csv
  cirrusSearchUpdate_1 <- read.delim('cirrusSearchUpdate1.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  cirrusSearchUpdate_1$date <- paste(cirrusSearchUpdate_1$year,
                                     cirrusSearchUpdate_1$month,
                                     cirrusSearchUpdate_1$day,
                                     cirrusSearchUpdate_1$hour,
                                     sep = "-")
  cirrusSearchUpdate_2 <- read.delim('cirrusSearchUpdate2.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  cirrusSearchUpdate_2$date <- paste(cirrusSearchUpdate_2$year,
                                     cirrusSearchUpdate_2$month,
                                     cirrusSearchUpdate_2$day,
                                     cirrusSearchUpdate_2$hour,
                                     sep = "-")
  # - join
  cirrusSearchUpdate <- left_join(cirrusSearchUpdate_1,
                                  select(cirrusSearchUpdate_2, advancedextensioncount, date),
                                  by = 'date')
  cirrusSearchUpdate$date <- NULL
  # - arrange
  cirrusSearchUpdate <- arrange(cirrusSearchUpdate, year, month, day, hour)
  cirrusSearchUpdate$specialsearchcount[is.na(cirrusSearchUpdate$specialsearchcount)] <- 0
  cirrusSearchUpdate$advancedextensioncount[is.na(cirrusSearchUpdate$advancedextensioncount)] <- 0
  # - aggreate daily and store
  cirrusSearchUpdate <- cirrusSearchUpdate %>%
    select(year, month, day, specialsearchcount, advancedextensioncount) %>%
    group_by(year, month, day) %>%
    summarise(specialsearchcount = sum(specialsearchcount),
              advancedextensioncount = sum(advancedextensioncount))
  # - delete previous day (probably incomplete)
  cirrusSearchUpdate <- cirrusSearchUpdate[-dim(cirrusSearchUpdate)[1], ]
  # - store:
  write.csv(cirrusSearchUpdate, "cirrusSearchUpdate.csv")
}

### --- PART 2: Special:Search/Advanced Extension KEYWORDS

### --- check update
lF <- list.files()
if ("cirrusSearchUpdateKeywords.csv" %in% lF) {
  
  # - load cirrusSearchUpdate.csv
  cirrusSearchUpdate <- read.csv('cirrusSearchUpdateKeywords.csv',
                                 header = T,
                                 check.names = F,
                                 row.names = 1,
                                 stringsAsFactors = F)
  # - extract the time of the latest update from cirrusSearchUpdate.csv
  updateY <- tail(cirrusSearchUpdate, 1)$year
  updateM <- tail(cirrusSearchUpdate, 1)$month
  updateD <- tail(cirrusSearchUpdate, 1)$day
  updateDate <- paste(updateY, updateM, updateD, sep = "-")
  updateDate <- as.POSIXct(updateDate)
  updateDate <- updateDate + 60*60*24
  updateDate <- as.integer(strsplit(as.character(updateDate),
                                    split = "-")[[1]])
  # - HiveQL

  # - toReport:
  print(paste0("Running regular keywords update for: ", paste(updateDate, collapse = "-")))

  # - get SpecialSearchCount data:
  hiveQuery <- paste0('"SELECT year, month, day, asyn as keyword, count(*) as count
                      FROM wmf_raw.CirrusSearchRequestSet csr
                      LATERAL VIEW EXPLODE(requests) req AS areq
                      LATERAL VIEW EXPLODE(split(areq.payload[\'syntax\'], \',\')) syn as asyn
                      WHERE year = ',
                      updateDate[1],
                      ' AND month = ',
                      updateDate[2],
                      ' AND day = ',
                      updateDate[3],
                      ' AND areq.queryType = \'full_text\' AND csr.source = \'web\' AND
                        (csr.payload[\'referer\'] LIKE \'%wiki%\')
                      GROUP BY year, month, day, asyn;"')

  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdateKeywords1.tsv")

  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for SpecialSearchCount data: ", queryCom))

  # - get AdvancedExtensionCount data:

  hiveQuery <- paste0('"SELECT year, month, day, asyn as keyword, count(*) as count
                      FROM wmf_raw.CirrusSearchRequestSet csr
                      LATERAL VIEW EXPLODE(requests) req AS areq
                      LATERAL VIEW EXPLODE(split(areq.payload[\'syntax\'], \',\')) syn as asyn
                      WHERE year = ',
                      updateDate[1],
                      ' AND month = ',
                      updateDate[2],
                      ' AND day = ',
                      updateDate[3],
                      ' AND areq.queryType = \'full_text\' AND csr.source = \'web\' AND
                        (csr.payload[\'queryString\'] LIKE \'%advancedSearch-current%\') AND
                        (csr.payload[\'referer\'] LIKE \'%wiki%\')
                      GROUP BY year, month, day, asyn;"')

  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdateKeywords2.tsv")

  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for AdvancedExtensionCount data: ", queryCom))

  # - combine data sets and store cirrusSearchUpdate.csv
  cirrusSearchUpdate_1 <- read.delim('cirrusSearchUpdateKeywords1.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  cirrusSearchUpdate_2 <- read.delim('cirrusSearchUpdateKeywords2.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  # - join
  cirrusUpdate <- full_join(cirrusSearchUpdate_1,
                            cirrusSearchUpdate_2,
                            by = c('year', 'month', 'day', 'keyword'))
  colnames(cirrusUpdate)[5:6] <- c('specialsearchcount', 'advancedextensioncount')
  # - add update and store:
  cirrusSearchUpdate <- rbind(cirrusSearchUpdate, cirrusUpdate)
  write.csv(cirrusSearchUpdate, "cirrusSearchUpdateKeywords.csv")
  
} else {

  # - toReport:
  print("This is the initial intake. Running HiveQL Keywords queries and producing the cirrusSearchUpdateKeywords.csv file now.")

  # - get SpecialSearchCount data:

  hiveQuery <- '"SELECT year, month, day, asyn as keyword, count(*) as count FROM wmf_raw.CirrusSearchRequestSet csr
                  LATERAL VIEW EXPLODE(requests) req AS areq
                  LATERAL VIEW EXPLODE(split(areq.payload[\'syntax\'], \',\')) syn as asyn
                WHERE (year = 2017 OR year = 2018) AND
                  areq.queryType = \'full_text\' AND
                  csr.source = \'web\' AND
                  (csr.payload[\'referer\'] LIKE \'%wiki%\')
                GROUP BY year, month, day, asyn;"'

  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdateKeywords1.tsv")

  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for Keywords in SpecialSearchCount data: ", queryCom))

  ### --- wait for 10 minutes
  print("Pausing for 10 minutes.")
  Sys.sleep(10*60)

  # - get AdvancedExtensionCount data:
  hiveQuery <- '"SELECT year, month, day, asyn as keyword, count(*) as count FROM wmf_raw.CirrusSearchRequestSet csr
                    LATERAL VIEW EXPLODE(requests) req AS areq
                    LATERAL VIEW EXPLODE(split(areq.payload[\'syntax\'], \',\')) syn as asyn
                WHERE (year = 2017 OR year = 2018) AND
                  areq.queryType = \'full_text\' AND
                  csr.source = \'web\' AND
                  (csr.payload[\'referer\'] LIKE \'%wiki%\') AND
                  (csr.payload[\'queryString\'] LIKE \'%advancedSearch-current%\')
                GROUP BY year, month, day, asyn;"'
  hiveQLQueryCommand <- paste0("/usr/local/bin/beeline --silent=true --incremental=true -e ",
                               hiveQuery,
                               " > /home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/cirrusSearchUpdateKeywords2.tsv")
  queryCom <- system(command = hiveQLQueryCommand, wait = TRUE)

  # - toReport:
  print(paste0("Report from running HiveQL for Keywords AdvancedExtensionCount data: ", queryCom))

  # - combine data sets and store cirrusSearchUpdate.csv
  cirrusSearchUpdate_1 <- read.delim('cirrusSearchUpdateKeywords1.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)

  cirrusSearchUpdate_2 <- read.delim('cirrusSearchUpdateKeywords2.tsv',
                                     sep = "\t",
                                     quote = "",
                                     check.names = F,
                                     stringsAsFactors = F)
  # - join
  cirrusSearchUpdate <- full_join(cirrusSearchUpdate_1,
                                  cirrusSearchUpdate_2,
                                  by = c('year', 'month', 'day', 'keyword'))
  colnames(cirrusSearchUpdate)[5:6] <- c('specialsearchcount', 'advancedextensioncount')
  # - arrange
  cirrusSearchUpdate$specialsearchcount[is.na(cirrusSearchUpdate$specialsearchcount)] <- 0
  cirrusSearchUpdate$advancedextensioncount[is.na(cirrusSearchUpdate$advancedextensioncount)] <- 0
  cirrusSearchUpdate <- arrange(cirrusSearchUpdate, year, month, day, keyword)
  # - delete previous day (probably incomplete)
  cirrusSearchUpdate <- cirrusSearchUpdate[-dim(cirrusSearchUpdate)[1], ]
  # - store:
  write.csv(cirrusSearchUpdate, "cirrusSearchUpdateKeywords.csv")
}

### ------------------------------------------------------------------
### --- Preprocessing dashboard datasets
### ------------------------------------------------------------------

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

### --- store Rds files
saveRDS(specSearchKeys, "specSearchKeys.Rds")
saveRDS(metrics, "metrics.Rds")
saveRDS(projects, "projects.Rds")
saveRDS(data24Overview, "data24Overview.Rds")
saveRDS(previousDay, "previousDay.Rds")
saveRDS(pDay, "pDay.Rds")
saveRDS(dataWeekOverview, "dataWeekOverview.Rds")
saveRDS(previousWeek, "previousWeek.Rds")
saveRDS(lastWeek, "lastWeek.Rds")
saveRDS(lastMonth, "lastMonth.Rds")
saveRDS(dataMonthOverview, "dataMonthOverview.Rds")
saveRDS(previousMonth, "previousMonth.Rds")
saveRDS(data24, "data24.Rds")
saveRDS(dataWeek, "dataWeek.Rds")
saveRDS(dataMonth, "dataMonth.Rds")
saveRDS(keyOccurWeek, "keyOccurWeek.Rds")
saveRDS(keyOccurMonth, "keyOccurMonth.Rds")
saveRDS(keyOccur3Months, "keyOccur3Months.Rds")
saveRDS(specSearchData, "specSearchData.Rds")
saveRDS(specSearchKeywordsData, "specSearchKeywordsData.Rds")

### ------------------------------------------------------------------
### - Copy files to public data sets:

system(command = 'cp /srv/home/goransm/RScripts/TechnicalWishes/AdvancedSearchExtension/* /srv/published-datasets/wmde-analytics-engineering/TechnicalWishes/AdvancedSearchExtension/',
       wait = T)
