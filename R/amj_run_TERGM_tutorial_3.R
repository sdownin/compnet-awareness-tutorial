####################################################
##
## Competition Network Analysis Tutorial
##
## Part 3. Adding New Data
##
####################################################

##===============================
## If the required libraries are not installed on your computer, run:
## >  install.packages(c('btergm','parallel','texreg'))
##-------------------------------
library(btergm)
library(parallel)
library(texreg)


##===============================
## SET YOUR DATA DIRECTORY:
##   This is the path to the folder where you saved the data file.
##   If you are using a Windows PC, use double backslash path separators "..\\dir\\subdir\\.."
##-------------------------------
# data_dir <- '/set/your/data/directory/here'
data_dir <- 'C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet-awareness-tutorial\\data'
## new data directory name
owler_data_dir <- 'owler_data'

##
#  Assign company_name_unique if missing
#     from two input columns (company_name, company_name_unique)
#  @param [string[]|NA[]] x  Vector of two strings|NAs (company_name, company_name_unique)
#  @return [string company_name_unique
##
assignCompanyNameUnique <- function(x=NA)
{
  # cat(sprintf('1: %s, 2: %s\n', x[1], x[2]))
  if (all(is.na(x)))
    return(NA)
  name <- ifelse(!is.na(x[2]), x[2], x[1]) ## use company_name_unique if exists, else company_name
  name <- str_to_lower(name)  ## to lowercase 
  ## replace non-alphanumeric sequences with a dash "-" and then return
  return(str_replace_all(name, pattern = '[^A-Za-z0-9]+', replacement = '-'))
}

## load RDS data file into memory as a list of networks
data_file <- file.path(data_dir, 'tutorial_d2_competition_network_sample.rds')
nets.all <- readRDS(data_file)
len <- length(nets.all)

## set number of time periods
nPeriods <- 8  ## any number from 3 to 11 is OK, but use 8 to compare results with example

## subset network periods
nets <- nets.all[(len-nPeriods+1):len]



## check network structure
net <- nets[[1]]
print(net)

dim(net[,])
print(net[1:4,1:4])


## missing value strings to convert to <NA> type in imported data file
na.strings <- c('NA', 'na', '')

##====================================
## Add New Data from Owler Data Files
##------------------------------------

## new data directory
owler_dir <- file.path(data_dir,owler_data_dir)

## init new dataframes
vt <- data.frame()  #vertices
el <- data.frame()  #edge list

## loop over data files in data directory
for (file in dir(owler_dir, pattern = '\\.csv$')) {
  cat(sprintf('\n\ndata file %s\n', file))  ## echo progress
  
  ## load data
  full_file_path <- file.path(owler_dir, file)
  df <- read.csv(full_file_path, stringsAsFactors = F, na.strings = na.strings)
  
  ## competitor columns
  compcols <- names(df)[grep('competitor_\\d{1,}',x = names(df))]

  ## keep columns that are NOT missing all competiors 
  rows.keep <- apply(df[,compcols], 1, function(x) !all(is.na(x)))
  df <- df[rows.keep,]
  
  ## assign company_name_unique if not exists
  tmp_names_df <- df[, c('name', 'company_name_unique')]
  df$company_name_unique <- apply(tmp_names_df, 1, assignCompanyNameUnique)
  
  ## if no competitor data columns or missing company_name_unique column, 
  ## then skip to next data file
  if (!('company_name_unique' %in% names(df)))
    next
  
  ## append verices
  vt <- rbind(vt, df)
  
  ## loop over firms in data file
  for (i in 1:nrow(df)) {
    
    ## firm i 
    firm_i <- df[i, 'company_name_unique']

    ## select competitors of firm i 
    firm_i_comps <- unlist(df[i, compcols]) ## unlist from data.frame to vector
    
    ## skip rows with no competitors included
    if (all(sapply(firm_i_comps, is.na)))
      next
    
    ## loop over each  competitor j of firm i
    for (j in 1:length(firm_i_comps)) {
      
      ## competitor j's name
      comp_j_name <- unname(firm_i_comps[j])
      ## competitor j's company_name_unique
      comp_j <- vt$company_name_unique[vt$name==comp_j_name]
      
      ## skip is missing
      if (length(comp_j)==0) 
        next
      if(all(is.na(comp_j_name)) | all(is.na(comp_j))) 
        next
      
      # ## echo progress check 
      # cat(sprintf('%s firm %s, competitor %s\n', i, firm_i, comp_j))  ## echo progress
      
      ## append competitor relation      
      tmp_el <- data.frame(source=firm_i, target=comp_j, rank=j, weight=1)
      el <- rbind(el, tmp_el)          
      
    }
    
    if (i %% 50 == 0) cat(sprintf('firm %s %s\n', i, firm_i))  ## echo progress
    
  }
  
}


## check vertices
dim(vt)
head(vt)

## check edge list
dim(el)
head(el, 20)

## write edge list to csv file
el_file <- file.path(data_dir, 'owler_edge_list.csv')
write.csv(el, file = el_file, row.names = F)

## write vertex list to csv file
vt_file <- file.path(data_dir, 'owler_vertex_list.csv')
write.csv(vt, file = vt_file, row.names = F)




