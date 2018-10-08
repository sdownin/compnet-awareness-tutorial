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

## new data directory name
cb_data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"

## owler data directory name
owler_data_dir_name <- 'owler_data'

## SET FOCAL FIRM
focal_firm <- 'ford'


##================================
## Relative paths based on above directories
##--------------------------------
## data_dir <- '/set/your/data/directory/here'
data_dir <- file.path(work_dir, 'data')

## new data directory name
owler_data_dir <- file.path(data_dir, owler_data_dir_name)

##==================================================
## Run data loading and prep scripts
##--------------------------------------------------
source(file.path(work_dir,'R','amj_awareness_functions.R'))    ## aaf: compnet awareness functions
source(file.path(work_dir,'R','amj_tutorial_cb_data_prep.R'))  ## cb:  CrunchBase dataframes object

print(summary(aaf))
print(summary(cb))

##==================================================
##
##  Make Full Graph
##
##--------------------------------------------------

cat('\nmaking full graph...')

max.year <- 2016

## delete edges at or later than this date (the year after max.year)
exclude.date <- sprintf('%d-01-01', max.year+1)

## make graph
g.full <- aaf$makeGraph(comp = cb$co_comp, vertdf = cb$co)

## cut out confirmed dates >= 2016
g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= max.year
                                                                | is.na(V(g.full)$founded_year)
                                                                | V(g.full)$founded_year=='' ) ] )
g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= exclude.date)])

## SIMPLIFY
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
                           edge.attr.comb = list(weight='sum',
                                                 relation_began_on='max',
                                                 relation_ended_on='min'))

## save graph file
igraph::write.graph(graph = g.full, file=file.path(data_dir, "g_full.graphml"), format = 'graphml')

cat('done.\n')



##==================================================
##
##  Create Focal Firm Networks per time period
##     and compute covariate arrays lists 
##     from network in each period 
##
##--------------------------------------------------



## -- settings --
d <- 3                   ## distance threshold for cohort selection
yrpd <- 1                ## length of period in years
startYr <- 2005          ## starting year (including 1 previous year for lag)
endYr <- 2017            ## dropping first for memory term; actual dates 2007-2016
lg.cutoff <- 1100        ## large network size cutoff to save periods seprately 
force.overwrite <- FALSE ## if network files in directory should be overwritten
## --------------  


##====================================
## run main network period creation loop
##-------------------------------------
# for (i in 1:length(firms.todo)) {

name_i <- focal_firm
cat(sprintf('\n\n------------ %s -------------\n\n',name_i))
periods <- seq(startYr,endYr,yrpd)
company.name <- 'company_name_unique'
g.base <- g.full  

## focal firm ego network sample
g.d.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.base)[V(g.base)$name==name_i], order = d, mode = 'all')[[1]]

## convert to network object
net.d.sub <- asNetwork(g.d.sub)
net <- net.d.sub
net %n% 'ego' <- name_i

##-------process pre-start-year acquisitions----------
acqs.pd <- cb$co_acq[cb$co_acq$acquired_on <= sprintf('%d-12-31',startYr-1), ]
g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, remove.isolates=T, verbose = T)
net.d.sub <- asNetwork(g.d.sub)
cat(sprintf('v = %d, e = %d\n',vcount(g.d.sub),ecount(g.d.sub)))

##------------Network Time Period List--------------------
nl <- list()

for (t in 2:length(periods)) 
{
  ## period dates
  cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
  t1 <- sprintf('%d-01-01',periods[t-1]) ## inclusive start date 'YYYY-MM-DD'
  t2 <- sprintf('%d-12-31',periods[t-1]) ## inclusive end date 'YYYY-MM-DD'
  
  ## check if period network file exists (skip if not force overwrite)
  file.rds <- sprintf('firm_nets_rnr/%s_d%d_y%s.rds',name_i,d,periods[t-1])
  if (!force.overwrite & file.exists(file.rds)) {
    cat(sprintf('file exists: %s\nskipping.\n', file.rds))
    next
  }
  
  ## 1. Node Collapse acquisitions within period
  acqs.pd <- cb$co_acq[cb$co_acq$acquired_on >= t1 & cb$co_acq$acquired_on <= t2, ]
  g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, verbose = T)
  
  ## 2. Subset Period Network
  nl[[t]] <- aaf$makePdNetwork(asNetwork(g.d.sub), periods[t-1], periods[t], isolates.remove = F) 
  
  ## 3. Set Covariates for updated Period Network
  covlist <- c('age','mmc','dist','ipo_status','constraint','similarity','centrality','generalist')
  nl[[t]] <- aaf$setCovariates(nl[[t]], periods[t-1], periods[t], covlist=covlist,
                               acq=cb$co_acq,br=cb$co_br,rou=cb$co_rou,ipo=cb$co_ipo)
  
}


## ----drop null and skipped periods----
nl.bak <- nl
nl <- nl[which(sapply(nl, length)>0)]

if (length(nl) > 1) {
  names(nl) <- periods[2:length(periods)]
}

##--------------- GET TERGM NETS LIST -----------
## only nets with edges > 0
if (length(nl) > 1) {
  nets.all <- nl[2:length(nl)]
} else {
  nets.all <- nl
}
nets <- nets.all[ which(sapply(nets.all, aaf$getNetEcount) > 0) ]
## record network sizes
write.csv(sapply(nets,function(x)length(x$val)), file = file.path(data_dir,sprintf('%s_d%s.csv',name_i,d)))

#-------------------------------------------------

## CAREFUL TO OVERWRITE 
file.rds <- file.path(data_dir,sprintf('%s_d%d.rds',name_i,d))
saveRDS(nets, file = file.rds)

