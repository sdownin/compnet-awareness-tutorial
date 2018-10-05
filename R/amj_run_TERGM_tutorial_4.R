####################################################
##
## Competition Network Analysis Tutorial
##
## Part 4. Compute Period Networks and Covarites Lists
##
####################################################

##===============================
## If the required libraries are not installed on your computer, run:
## >  install.packages(c('btergm','parallel','texreg'))
##-------------------------------
library(btergm)
library(parallel)
library(texreg)
library(lubridate)
library(plyr)
library(reshape2)

##===============================
## SET YOUR DIRECTORIES:
##   This is the path to the folder where you saved the data file.
##   If you are using a Windows PC, use double backslash path separators "..\\dir\\subdir\\.."
##-------------------------------
## working dir
work_dir <- 'C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet-awareness-tutorial'
# data_dir <- '/set/your/data/directory/here'
data_dir <- file.path(work_dir, 'data')
## new data directory name
owler_data_dir <- file.path(data_dir,'owler_data')
## new data directory name
cb_data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"

##==================================================
## Run data loading and prep scripts
##--------------------------------------------------
source(file.path(work_dir,'R','amj_awareness_functions.R'))    ## aaf: compnet awareness functions
source(file.path(work_dir,'R','amj_tutorial_cb_data_prep.R'))  ## cb:  CrunchBase dataframes object

print(summary(aaf))
print(summary(cb))


## missing value strings to convert to <NA> type in imported data file
na.strings <- c('NA', 'na', '')


## load edge list 
ow.el <- read.csv(file.path(data_dir, 'owler_edge_list.csv'), na.strings = na.strings, stringsAsFactors = F)

## load vertex list 
ow.vt <- read.csv(file.path(data_dir, 'owler_vertex_list.csv'), na.strings = na.strings, stringsAsFactors = F)

print(head(ow.vt))


## create ipo date column
ow.vt$ipo_date2 <- ymd(ow.vt$ipo_date)

##=====================================================
##  Limit owler firms to those also in CrunchBase
##   - necessary if you want to use other data attributes from CrunchBase
##   - otherwise, to keep all owler firms not in CrunchBase, you will need to find
##     the data for all missing variables manually; example:  firm branches (region and opening date)
##-----------------------------------------------------
##=====================================================
##
## NOTE: Need to run Part 3 script again
##       edgelist previously had wrong target format
##       due to mistake:  
##        `el` was written to csv file instead of `el2`
##
##------------------------------------------------------

## Which owler companies are also in CrunchBase
idx.vt.ow.cb <- which(ow.vt$company_name_unique %in% cb$co$company_name_unique)

## use owler firms that are in CrunchBase
vt <- ow.vt[idx.vt.ow.cb, ]
## use owledger competitive relations were both firms are in CrunchBase
idx.el.ow.cb <- which(vt$company_name_unique %in% ow.el[,'source'] 
                      |  vt$company_name_unique %in% ow.el[,'target'])
el <- ow.el[idx.el.ow.cb, ]

print(dim(el))
print(head(el,15))











