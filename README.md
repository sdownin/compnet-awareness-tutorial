Competition Network Analysis
=======

Tutorial for computations to replicate the results in Downing, Kang, & Markman (Under Review).

Acknowledgement:
This research was supported in part by MOST-105-2420-H-009-012-DR and  MOST-106-2922-I-009-127.

## Info Links
- [Introduction to Exponential Random Graph Models (ERGM)](http://ranger.uta.edu/~chqding/cse5301/classPapers/ExponentialRandomGraph.pdf  "")    
- [Computing ERGMs in R](https://www.jstatsoft.org/index.php/jss/article/view/v024i03/v24i03.pdf  "")
- [Bootstrapped ERGMs for Big Networks in R](https://arxiv.org/pdf/1708.02598.pdf  "")


## Part 1: Analyzing Existing Network Data Sample

In this repository's `R` directory, download the R script `amj_run_TERGM_tutorial_1.R`. 

Save the RDS data file [tutorial_d2_competition_network_sample.rds](https://drive.google.com/file/d/1UqNcj4ci7_V2VRYL0hH9j6Y8bmNrDZfh/view?usp=sharing "Example Competition Network Sample") in the same directory that you save the above script. You can run the script in its entirety simply to get the results, but an explanation of each part is provided below in case you want to change the analysis.

Set the name of the directory where you saved the data file:
```R
##===============================
## SET YOUR DATA DIRECTORY:
##   This is the path to the folder where you saved the data file.
##   If you are using a Windows PC, use double backslash path separators "..\\dir\\subdir\\.."
##-------------------------------
data_dir <- '/set/your/data/directory/here'
```

Set parameters for the analysis and load the data into memory:
```R
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
```

Set the model formuas for the Part 1 tutorial:
```R
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
```

Set the number of bootstrap replications. Roughly 100 is enough for an approximate estimate; on the order of 1000 or more for reporting results:
```R
R <- 200  ## enough for a rough estimate
```

Compute the first model `m0` and save to disk as an RDS (serialized) file:
```R
## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit0 <- btergm(get('m0'), R=R, parallel = "multicore", ncpus = detectCores())

## SAVE SERIALIZED DATA
fit0_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm0'))
saveRDS(fit0, file=fit0_file)
```

Compute the second model `m1` and save to disk as an RDS (serialized) file:
```R
## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit1 <- btergm(get('m1'), R=R, parallel = "multicore", ncpus = detectCores())  

## SAVE SERIALIZED DATA
fit1_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm1'))
saveRDS(fit1, file=fit1_file)
```

Create a list of model fits. Print the regression table to screen and save it as a formatted HTML file.
```R
## Cache model fits list
fits <- list(Model_0=fit0,Model_1=fit1)

## Echo model comparison table to screen
texreg::screenreg(fits, digits = 3)

## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0-m1'))
texreg::htmlreg(fits, digits = 3, file=compare_file)
```

Compute and the first model and save to disk as an RDS (serialized) file:
```R

```

## Part 2: Introducing and Cleaning New Network Data

Coming soon...


## Part 3: Creating Competition Networks and Covariate Arrays from Updated Data 

Coming soon...


## Part 4: Analyzing Updated Network Data Sample 

Coming soon...
