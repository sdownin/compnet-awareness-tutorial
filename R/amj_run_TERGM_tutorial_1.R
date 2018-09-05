####################################################
##
## Competition Network Analysis Tutorial
##
## Part 1. Analyzing Existing Network Data Sample
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
data_dir <- '/set/your/data/directory/here'



## analysis parameters
firm_i <- 'qualtrics'  ## focal firm
d <- 2                 ## ego network theshold (order)

## load RDS data file into memory as a list of networks
data_file <- file.path(data_dir,sprintf('%s_d%s.rds',firm_i,d))
nets.all <- readRDS(data_file)
len <- length(nets.all)

## set number of time periods
nPeriods <- 8  ## any number from 3 to 11 is OK, but use 8 to compare results with example

## subset network periods
nets <- nets.all[(len-nPeriods+1):len]


######### DEFINE MODELS ###############

m0 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  memory(type = "stability", lag = 1)

m1 <-   nets ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed=T) + 
  nodematch("ipo_status", diff = F) + 
  nodematch("state_code", diff = F) + 
  nodecov("age") + absdiff("age") + 
  nodecov("cent_deg") +
  memory(type = "stability", lag = 1) + 
  nodecov("genidx_multilevel") + 
  nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + 
  cycle(3) + cycle(4) 

######### end models ###############


## SET RESAMPLES
R <- 200  ## enough for a rough estimate


##==================
## RUN TERGM MODEL 0
##------------------

## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit0 <- btergm(get('m0'), R=R, parallel = "multicore", ncpus = detectCores())

## SAVE SERIALIZED DATA
fit0_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm0'))
saveRDS(fit0, file=fit0_file)


##==================
## RUN TERGM MODEL 1
##------------------

## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit1 <- btergm(get('m1'), R=R, parallel = "multicore", ncpus = detectCores())  

## SAVE SERIALIZED DATA
fit1_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm1'))
saveRDS(fit1, file=fit1_file)


##========================
## OUTPUT AND VIEW RESULTS
##------------------------

## Cache model fits list
fits <- list(Model_0=m0,Model_1=m1)

## Echo model comparison table to screen
texreg::screenreg(fits, digits = 3)

## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0-m1'))
texreg::htmlreg(fits, digits = 3, file=compare_file)


cat('finished successfully.')

