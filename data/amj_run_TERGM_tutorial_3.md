---
title: "amj_run_TERGM_tutorial_3.R"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---



# Part 3: Updating Data



```
## Loading required package: xergm.common
```

```
## Loading required package: ergm
```

```
## Loading required package: statnet.common
```

```
## 
## Attaching package: 'statnet.common'
```

```
## The following object is masked from 'package:base':
## 
##     order
```

```
## Loading required package: network
```

```
## network: Classes for Relational Data
## Version 1.13.0 created on 2015-08-31.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##                     Mark S. Handcock, University of California -- Los Angeles
##                     David R. Hunter, Penn State University
##                     Martina Morris, University of Washington
##                     Skye Bender-deMoll, University of Washington
##  For citation information, type citation("network").
##  Type help("network-package") to get started.
```

```
## 
## ergm: version 3.8.0, created on 2017-08-18
## Copyright (c) 2017, Mark S. Handcock, University of California -- Los Angeles
##                     David R. Hunter, Penn State University
##                     Carter T. Butts, University of California -- Irvine
##                     Steven M. Goodreau, University of Washington
##                     Pavel N. Krivitsky, University of Wollongong
##                     Martina Morris, University of Washington
##                     with contributions from
##                     Li Wang
##                     Kirk Li, University of Washington
##                     Skye Bender-deMoll, University of Washington
## Based on "statnet" project software (statnet.org).
## For license and citation information see statnet.org/attribution
## or type citation("ergm").
```

```
## NOTE: Versions before 3.6.1 had a bug in the implementation of the
## bd() constriant which distorted the sampled distribution somewhat.
## In addition, Sampson's Monks datasets had mislabeled vertices. See
## the NEWS and the documentation for more details.
```

```
## 
## Attaching package: 'xergm.common'
```

```
## The following object is masked from 'package:ergm':
## 
##     gof
```

```
## Loading required package: ggplot2
```

```
## Package:  btergm
## Version:  1.9.0
## Date:     2017-03-30
## Authors:  Philip Leifeld (University of Glasgow)
##           Skyler J. Cranmer (The Ohio State University)
##           Bruce A. Desmarais (Pennsylvania State University)
```

```
## Version:  1.36.23
## Date:     2017-03-03
## Author:   Philip Leifeld (University of Glasgow)
## 
## Please cite the JSS article in your publications -- see citation("texreg").
```


Set parameters for the analysis and load the data into memory:

```r
## analysis parameters
firm_i <- 'qualtrics'  ## focal firm
d <- 2                 ## ego network theshold (order)

## load RDS data file into memory as a list of networks
# data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
data_file <- file.path(data_dir, 'tutorial_d2_competition_network_sample.rds')
nets.all <- readRDS(data_file)
len <- length(nets.all)

## set number of time periods
nPeriods <- 8  ## any number from 3 to 11 is OK, but use 8 to compare results with example

## subset network periods
nets <- nets.all[(len-nPeriods+1):len]
```



```r
net <- nets[[1]]
print(net)
```

```
##  Network attributes:
##   vertices = 180 
##   directed = FALSE 
##   hyper = FALSE 
##   loops = FALSE 
##   multiple = FALSE 
##   bipartite = FALSE 
##   mmc:
##                  0 0.0833333333333333              0.125 
##              32344                  2                  2 
##  0.166666666666667               0.25  0.333333333333333 
##                  2                 20                  4 
##                0.5 
##                 26 
##   dist: 180x180 matrix
##   similarity: 180x180 matrix
##   coop:
##     0 
## 32400 
##   coop_bin:
##     0 
## 32400 
##   coop_past:
##     0 
## 32400 
##   coop_past_bin:
##     0 
## 32400 
##   total edges= 92 
##     missing edges= 0 
##     non-missing edges= 92 
## 
##  Vertex attribute names: 
##     acquired_name acquired_on acquired_uuid acquired_vids age category_group_list category_list cent_deg cent_eig cent_pow_n0_0 cent_pow_n0_1 cent_pow_n0_2 cent_pow_n0_3 cent_pow_n0_4 cent_pow_n0_5 city closed_on closed_year com_edgebetween com_fastgreedy com_infomap com_labelprop com_multilevel com_walktrap company_cusip company_cusip_6 company_gvkey company_name company_sic company_uuid constraint country_code domain employee_count founded_on founded_year genidx_edgebetween genidx_fastgreedy genidx_infomap genidx_labelprop genidx_multilevel genidx_walktrap id ipo_status njobs_edgebetween njobs_fastgreedy njobs_infomap njobs_labelprop njobs_multilevel njobs_walktrap orig.vid region state_code status_update vertex.names weight 
## 
##  Edge attribute names: 
##     relation_began_on relation_ended_on weight
```


```r
dim(net[,])
```

```
## [1] 180 180
```

```r
print(net[1:4,1:4])
```

```
##                123contactform abroad101 actuate allegiance
## 123contactform              0         0       0          0
## abroad101                   0         0       0          0
## actuate                     0         0       0          0
## allegiance                  0         0       0          0
```



```r
## missing value strings to convert to <NA> type in imported data file
na.strings <- c('NA', 'na', '')

##====================================
## Add New Data from Owler Data Files
##------------------------------------

## new data directory name
ower_data_dir <- 'owler_data'

## new data directory
owler_dir <- file.path(data_dir,ower_data_dir)

## init new dataframes
vt <- data.frame()  #vertices
el <- data.frame()  #edge list

## loop over data files in data directory
for (file in dir(owler_dir, pattern = '\\.csv$')) {
  cat(sprintf('\n\ndata file %s\n', file))  ## echo progress
  
  ## load data
  full_file_path <- file.path(owler_dir, file)
  df <- read.csv(full_file_path, stringsAsFactors = F, na.strings = na.strings)
  
  ## append verices
  vt <- rbind(vt, df)
  
  ## select competitor column names of the form: "competitor_<number>"
  cols <- names(df)
  compcols <- cols[grep('competitor_\\d{1,}',x = cols)]  ## \\d{1,} is integer of 1+ digits 
  
  ## if no competitor data columns or missing company_name_unique column, 
  ## then skip to next data file
  if (length(compcols)==0 | !('company_name_unique' %in% names(df)))
    next
  
  ## loop over firms in data file
  for (i in 1:nrow(df)) {
    
    ## firm i 
    firm_i <- df[i, 'company_name_unique']

    ## select competitors of firm i 
    firm_i_comps <- unlist(df[i, compcols]) ## unlist from data.frame to vector
    
    ## skip if firm i has no company_name_unique
    if (is.na(firm_i))
      next
    ## skip rows with no competitors included
    if (all(sapply(firm_i_comps, is.na)))
      next
    
    ## loop over each  competitor j of firm i
    for (j in 1:length(firm_i_comps)) {
      
      comp_j <- unname(firm_i_comps[j])
      
      if (!is.na(comp_j)) {
        tmp_el <- data.frame(source=firm_i, target=comp_j, rank=j, weight=1)
        ## append competitor relation
        el <- rbind(el, tmp_el)          
      }
      
    }
    
    if (i %% 50 == 0) cat(sprintf('firm %s %s\n', i, firm_i))  ## echo progress
    
  }
  
}
```

```
## 
## 
## data file Ford.csv
## firm 100 dodgeofftworth
## firm 150 physioroom
## firm 200 alldata
## 
## 
## data file NYTimes.csv
## firm 50 lex-machina
## firm 100 linkedin
## firm 150 marketaxess
## firm 200 electra-information-systems
## firm 250 iri
## firm 300 cable-one
## firm 350 salem-media-group
```

The `target` column is not of the format like `company_name_unique` which must be fixed or else the firm names cannot be matched against the vertex dataframe to create the graph data frame.


```r
## check vertices
dim(vt)
```

```
## [1] 724  28
```

```r
head(vt)
```

```
##            name company_name_unique founded_year founded_date closed_date
## 1          Ford                ford         1963    1963/6/16        <NA>
## 2        Toyota       toyota-global         1903    1903/6/17        <NA>
## 3         Honda               honda         1948    1948/9/24        <NA>
## 4     Chevrolet           chevrolet         1911    1911/11/1        <NA>
## 5        Nissan       nissan-global         1933   1933/12/26        <NA>
## 6 Hyundai Motor             hyundai         1967   1967/12/29        <NA>
##   acquired_date acquired_by_company_name_unique employees
## 1          <NA>                            <NA>   202,000
## 2          <NA>                            <NA>   369,124
## 3          <NA>                            <NA>   215,638
## 4          <NA>                            <NA>    20,000
## 5          <NA>                            <NA>   152,421
## 6          <NA>                            <NA>    63,099
##              domain total_funding_usd annual_revenue
## 1          ford.com             $756M        $158.7B
## 2 toyota-global.com               $ -        $470.2B
## 3         honda.com               $ -        $138.9B
## 4     chevrolet.com               $ -         $11.5B
## 5 nissan-global.com               $ -        $106.1B
## 6       hyundai.com               $ -         $73.1B
##                         hq_region  status   ipo_date independence
## 1                DearbornMichigan  Public 1956-01-01            Y
## 2     Toyota CityAichi Prefecture  Public 1978-01-13            Y
## 3     Minato-kuTōkyō Prefecture  Public 1978-01-13            Y
## 4                  Oshawa,Ontario Private       <NA>            Y
## 5 Yokohama-shiKanagawa Prefecture Private       <NA>            Y
## 6          SeoulSeoul Teugbyeolsi  Public 2003-01-10            Y
##   stock_symbol_1 stock_symbol_2  competitor_1  competitor_2 competitor_3
## 1           NYSE           <NA>        Toyota         Honda    Chevrolet
## 2           NYSE           <NA>        Nissan         Honda   Ford Motor
## 3            BMV           <NA>        Toyota        Nissan   Ford Motor
## 4           <NA>           <NA> Hyundai Motor    Ford Motor          Kia
## 5           <NA>           <NA> Hyundai Motor         Acura        Honda
## 6 Korea Exchange           <NA>        Nissan Mercedes-Benz  Mazda Motor
##   competitor_4       competitor_5   competitor_6 competitor_7
## 1       Nissan      Hyundai Motor     Volkswagen   Volkswagen
## 2   Volkswagen     General Motors  Hyundai Motor    BMW Group
## 3   Volkswagen      Hyundai Motor General Motors     Chrysler
## 4       Nissan             Toyota          Honda   Volkswagen
## 5      Peugeot Toyota Motor Sales     Volkswagen   SAIC Motor
## 6        Honda         Volkswagen           Audi       Toyota
##     competitor_8 competitor_9 competitor_10 notes
## 1 General Motors           NA            NA  <NA>
## 2    Tata Motors           NA            NA  <NA>
## 3           <NA>           NA            NA  <NA>
## 4       Chrysler           NA            NA  <NA>
## 5         Toyota           NA            NA  <NA>
## 6 General Motors           NA            NA  <NA>
```

```r
## check edge list
dim(el)
```

```
## [1] 4122    4
```

```r
head(el, 20)
```

```
##           source         target rank weight
## 1           ford         Toyota    1      1
## 2           ford          Honda    2      1
## 3           ford      Chevrolet    3      1
## 4           ford         Nissan    4      1
## 5           ford  Hyundai Motor    5      1
## 6           ford     Volkswagen    6      1
## 7           ford     Volkswagen    7      1
## 8           ford General Motors    8      1
## 9  toyota-global         Nissan    1      1
## 10 toyota-global          Honda    2      1
## 11 toyota-global     Ford Motor    3      1
## 12 toyota-global     Volkswagen    4      1
## 13 toyota-global General Motors    5      1
## 14 toyota-global  Hyundai Motor    6      1
## 15 toyota-global      BMW Group    7      1
## 16 toyota-global    Tata Motors    8      1
## 17         honda         Toyota    1      1
## 18         honda         Nissan    2      1
## 19         honda     Ford Motor    3      1
## 20         honda     Volkswagen    4      1
```


We need to update `target` column by using the mapping `name`-->`company_name_unique` that we already have in the vertex dataframe.


```r
# company name to company_name_unique mapping
mapping <- vt[,c('name','company_name_unique')]
names(mapping) <- c('target','target_name_unique')

## merge the original edge list and company_name_unique mapping
el2 <- merge(el, mapping, by.x='target', by.y='target', all.x=T, all.y=F)

## replace original target column with new mapped target_name_unique
el2$target <- el2$target_name_unique
## finally remove the temporary target_name_unique column
el2$target_name_unique <- NULL

head(el2)
```

```
##          target        source rank weight
## 1 toyota-global      bmwgroup    2      1
## 2 toyota-global nissan-global    8      1
## 3 toyota-global       hyundai    7      1
## 4 toyota-global mercedes-benz    6      1
## 5 toyota-global  volkswagenag    1      1
## 6 toyota-global    tatamotors    4      1
```

```r
## write edge list to csv file
el_file <- file.path(data_dir, 'owler_edge_list.csv')
write.csv(el, file = el_file, row.names = F)

## write vertex list to csv file
el_file <- file.path(data_dir, 'owler_vertex_list.csv')
write.csv(vt, file = el_file, row.names = F)
```






















