Competition Network Analysis
=======

Tutorial for computations to replicate the results in Downing, Kang, & Markman (Under Review).

Acknowledgement:
This research was supported in part by MOST-105-2420-H-009-012-DR and  MOST-106-2922-I-009-127.

### Info 
- [Introduction to Exponential Random Graph Models (ERGM)](http://ranger.uta.edu/~chqding/cse5301/classPapers/ExponentialRandomGraph.pdf  "link")    
- [Computing ERGMs in R](https://www.jstatsoft.org/index.php/jss/article/view/v024i03/v24i03.pdf  "download")
- [Bootstrapped ERGMs for Big Networks in R](https://arxiv.org/pdf/1708.02598.pdf  "link")
- [Temporal ERGMs (TERGM) in R](https://www.jstatsoft.org/index.php/jss/article/view/v083i06/v83i06.pdf "download")

### Parts
- [Part 1: Analyzing Existing Network Data Sample](#part-1-analyzing-existing-network-data-sample  "Part 1")
- [Part 2: Adding and Cleaning New data](#part-2-adding-and-cleaning-new-data  "Part 2")
- [Part 3: Computing Period Networks and Covariate Lists](#part-3-computing-period-networks-and-covariate-lists  "Part 3")
- [Part 4: Analyzing Updated Network Data Sample](#part-4-analyzing-updated-network-data-sample  "Part 4")
- [Part 5: Estimation Algorithm, Goodness of Fit, Degeneracy](#part-5-estimation-algorithm-goodness-of-fit-degeneracy  "Part 5")


# Part 1: Analyzing Existing Network Data Sample

In this repository's `R` directory, download the R script `amj_run_TERGM_tutorial_1.R`. 

Download and save the following RDS (serialized) data file     
- [tutorial_d2_competition_network_sample.rds](https://drive.google.com/file/d/1DcpV0tomKyeY4BUsWcBZ1WSOIYOxPYMG/view?usp=sharing "Example Competition Network Sample")

in the same directory that you save the above script. You can run the script in its entirety simply to get the results, but an explanation of each part is provided below in case you want to change the analysis. 

Set the name of the directory where you saved the data file:

```r
##===============================
## SET YOUR DATA DIRECTORY:
##   This is the path to the folder where you saved the data file.
##   If you are using a Windows PC, use double backslash path separators "..\\dir\\subdir\\.."
##-------------------------------
data_dir <- '/set/your/data/directory/here'
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

Set the model formulas for the Part 1 tutorial:

```r
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

Set the number of bootstrap replications. According to [Leifeld, Cranmer, & Desmarais (2018)](https://www.jstatsoft.org/article/view/v083i06 "Temporal Exponential Random Graph Models with btergm") :
- Roughly 100 is enough for an approximate estimate
- On the order of 1000 or more for reporting results


```r
R <- 100  ## enough for a rough estimate
```

Compute the first model `m0` and save to disk as an RDS (serialized) file:

```r
## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit0 <- btergm(get('m0'), R=R, parallel = "multicore", ncpus = detectCores())
```

```
## 
## Initial dimensions of the network and covariates:
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
## 
## All networks are conformable.
## 
## Dimensions of the network and covariates after adjustment:
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
## 
## Starting pseudolikelihood estimation with 100 bootstrapping replications using multicore forking on 4 cores...
## Done.
```

```r
## SAVE SERIALIZED DATA
fit0_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm0'))
saveRDS(fit0, file=fit0_file)
```

Compute the second model `m1` and save to disk as an RDS (serialized) file:

```r
## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit1 <- btergm(get('m1'), R=R, parallel = "multicore", ncpus = detectCores())  
```

```
## 
## Initial dimensions of the network and covariates:
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
## 
## All networks are conformable.
## 
## Dimensions of the network and covariates after adjustment:
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
## 
## Starting pseudolikelihood estimation with 100 bootstrapping replications using multicore forking on 4 cores...
## Done.
```

```r
## SAVE SERIALIZED DATA
fit1_file <- file.path(data_dir,sprintf('fit_%s_pd%s_R%s_%s.rds', firm_i, nPeriods, R, 'm1'))
saveRDS(fit1, file=fit1_file)
```

Create a list of model fits. Print the regression table to screen and save it as a formatted HTML file.
You should see results like these:


```r
## Cache model fits list
fits <- list(Model_0=fit0,Model_1=fit1)

## Echo model comparison table to screen
screenreg(fits, digits = 3)
```

```
## 
## =============================================================
##                            Model_0           Model_1         
## -------------------------------------------------------------
## edges                         -0.180            -1.916 *     
##                            [-1.749;  1.067]  [-3.590; -0.606]
## gwesp.fixed.0                  0.813 *           0.449 *     
##                            [ 0.658;  0.908]  [ 0.152;  0.625]
## gwdegree                      -1.145 *          -0.504       
##                            [-1.910; -0.527]  [-1.454;  0.337]
## nodematch.ipo_status           0.132            -0.176       
##                            [-0.259;  1.070]  [-0.659;  0.777]
## nodematch.state_code          -0.593 *          -0.413 *     
##                            [-0.770; -0.387]  [-0.710; -0.048]
## nodecov.age                   -0.139 *          -0.119 *     
##                            [-0.199; -0.065]  [-0.159; -0.051]
## absdiff.age                    0.157 *           0.142 *     
##                            [ 0.085;  0.216]  [ 0.075;  0.177]
## edgecov.memory[[i]]            5.229 *           4.907 *     
##                            [ 4.908;  5.769]  [ 4.605;  5.482]
## nodecov.cent_deg                                 0.009       
##                                              [-0.071;  0.067]
## nodecov.genidx_multilevel                        1.526 *     
##                                              [ 0.401;  2.503]
## nodecov.cent_pow_n0_4                            0.008       
##                                              [-0.117;  0.176]
## absdiff.cent_pow_n0_4                            0.203 *     
##                                              [ 0.098;  0.450]
## cycle3                                           0.349 *     
##                                              [ 0.121;  0.615]
## cycle4                                          -0.010       
##                                              [-0.054;  0.035]
## -------------------------------------------------------------
## Num. obs.                  13787             67288           
## =============================================================
## * 0 outside the confidence interval
```

```r
## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0-m1'))
htmlreg(fits, digits = 3, file=compare_file)

## 
## finished successfully.
```


# Part 2: Adding and Cleaning New data

Coming soon...


# Part 3: Computing Period Networks and Covariate Lists

Coming soon...


# Part 4: Analyzing Updated Network Data Sample

Coming soon...


# Part 5: Estimation Algorithm, Goodness of Fit, Degeneracy

Coming soon...

