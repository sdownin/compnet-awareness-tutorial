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
# data_dir <- '/set/your/data/directory/here'
data_dir <- 'C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet-awareness-tutorial\\data'


# ## analysis parameters
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
fit0 <- btergm(m0, R=R, parallel = "multicore", ncpus = detectCores())

## SAVE SERIALIZED DATA
fit0_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm0'))
saveRDS(fit0, file=fit0_file)


##==================
## RUN TERGM MODEL 1
##------------------

## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit1 <- btergm(m1, R=R, parallel = "multicore", ncpus = detectCores())  

## SAVE SERIALIZED DATA
fit1_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm1'))
saveRDS(fit1, file=fit1_file)


##========================
## OUTPUT AND VIEW RESULTS
##------------------------

## Cache model fits list
fits <- list(Model_0=fit0,Model_1=fit1)

## Echo model comparison table to screen
texreg::screenreg(fits, digits = 3)

## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0-m1'))
texreg::htmlreg(fits, digits = 3, file=compare_file)


cat('\nEnd Part 1.\n')


##=========================================
##
## Part 2. Goodness of Fit, Degeneracy, Estimation Algorithm
##
##-----------------------------------------

## Model 0 Goodness of Fit
gof0 <- gof(fit0, statistics=c(dsp, esp, deg, geodesic), nsim=30)
print(gof0)


## Plot GOF statistics for `m0`
plot(gof0)


##Now compare the GOF for `m1`
gof1 <- gof(fit1, statistics=c(dsp, esp, deg, geodesic), nsim=30)
print(gof1)


##Plot GOF statistics for `m1`
plot(gof1)

##Check degeneracy of `m0` with another sample of random networks based on model parameters (instead of the diagnostic statistics used for GOF above).
degen0 <- checkdegeneracy(fit0, nsim=30)
print(degen0)

## Plot degeneracy check of `m0` model parameters
par(mfrow=c(3,3))
plot(degen0)
par(mfrow=c(1,1))


## check degeneracy for `m1`
degen1 <- checkdegeneracy(fit1, nsim=30)
print(degen1)


##And plot degeneracy check for `m1`
par(mfrow=c(3,3))
plot(degen1)
par(mfrow=c(1,1))



### Compare Estimation Algorithms: PMLE vs MCMCMLE


## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit0m <- mtergm(m0, ctrl=control.ergm(seed = 1111))

## SAVE SERIALIZED DATA
fit0m_file <- file.path(data_dir,sprintf('fit_%s_pd%s_%s.rds', firm_i, nPeriods, 'm0m'))
saveRDS(fit0m, file=fit0m_file)


##Compare PMLE and MCMCMLE results
## Cache model fits list
fits <- list(PMLE=fit0, MCMCMLE=fit0m)

## Echo model comparison table to screen
screenreg(fits, digits = 3)

## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0PMLE-m0MCMCMLE'))
htmlreg(fits, digits = 3, file=compare_file)



##compare the PMLE and MCMCMLE confidence intervals directly
## Cache model fits list
fits <- list(PMLE=fit0, MCMCMLE=fit0m)

## Echo model comparison table to screen
screenreg(fits, digits = 3, ci.force = T, ci.force.level = .95)

## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0PMLE-m0MCMCMLE_ci'))
htmlreg(fits, digits = 3, file=compare_file, ci.force = T, ci.force.level = .05)


##Finally, check the MCMCMLE diagnostics
mcmc.diagnostics(fit0m@ergm)

cat('\nEnd Part 2.\n')



