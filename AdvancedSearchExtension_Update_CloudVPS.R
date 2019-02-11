#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Advanced Search Extension Dashboard, v2.0 Beta 0.1
### --- Script: AdvancedSearchExtension_Update_CloudVPS.R, v. Beta 0.1
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- AdvancedSearchExtension_Update_CloudVPS.R runs on cron from 
### --- wikidataconcepts.eqiad.wmflabs checking changes in 
### --- https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/TechnicalWishes/AdvancedSearchExtension/
### --- onto which the /srv/published-datasets/wdcm from stat1007 is mapped.
### --- The script updates the WMDE Advanced Search Extension Dashboard
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wikidata Concepts Monitor (WDCM)
### ---
### --- WDCM is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- WDCM is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with WDCM. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

# - libraries
library(httr)
library(curl)
library(stringr)
library(XML)

# - toReport:
print(paste0("TW: Advanced Search Extension update started on: ", as.character(Sys.time())))

### --- Config File
### --- Read WDCM paramereters
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")
params <- xmlParse(paste0(fPath, 'config_AdvancedSearchExtension_Update_CloudVPS.xml'))
params <- xmlToList(params)

# - toReport:
print(paste0("Download data: Step 1. ", as.character(Sys.time())))

### --- data
h <- new_handle()
# - set curl options:
handle_setopt(h,
              copypostfields = "TW Advanced Search Extension Dashboard");
handle_setheaders(h,
                  "Cache-Control" = "no-cache"
)
# - download:
curl_download(url = params$asExtensionUpdateRemotePath,
              handle = h, 
              destfile = params$asExtensionUpdateLocalPath)

# - toReport:
print(paste0("Download data: Step 2. cirrusSearchUpdate.csv ", as.character(Sys.time())))

# - cirrusSearchUpdate.csv
# - set curl options:
handle_setopt(h,
              copypostfields = "TW Advanced Search Extension Dashboard");
handle_setheaders(h,
                  "Cache-Control" = "no-cache"
)
# - download:
curl_download(url = params$cirrusSearchUpdateRemotePath,
              handle = h, 
              destfile = params$cirrusSearchUpdateLocalPath)

# - toReport:
print(paste0("Download data: Step 3. cirrusSearchUpdateKeywords.csv ", as.character(Sys.time())))

# - cirrusSearchUpdateKeywords.csv
# - set curl options:
handle_setopt(h,
              copypostfields = "TW Advanced Search Extension Dashboard");
handle_setheaders(h,
                  "Cache-Control" = "no-cache"
)
# - download:
curl_download(url = params$cirrusSearchUpdateKeywordsRemotePath,
              handle = h, 
              destfile = params$cirrusSearchUpdateKeywordsLocalPath)

# - toReport:
print(paste0("Download data: Step 4. Rds files ", as.character(Sys.time())))

# - download .Rds data:
# - list files:
url <- params$updateRemoteDir
page <- as.character(GET(url))
links <- str_extract_all(page, "<a href=.+>.+</a>")
links <- sapply(links, function(x) {str_extract_all(x, ">.+<")})
links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
links <- links[3:length(links)]
links <- links[grepl(".Rds", links, fixed = T)]
for (i in 1:length(links)) {
  # - download:
  curl_download(url = 
                  paste0(params$updateRemoteDir,
                         links[i]),
                handle = h, 
                destfile = paste0(params$updateLocalDir, 
                                  links[i])
  )
}

### --- copy data:

# - toReport:
print(paste0("TW: Advanced Search Extension copy files in production started on: ", as.character(Sys.time())))

system(paste0('sudo cp ', 
              params$updateLocalDir, '* ', 
              params$updateProductionDir), 
       wait = T)

# - toReport:
print(paste0("TW: Advanced Search Extension update completed on: ", as.character(Sys.time())))