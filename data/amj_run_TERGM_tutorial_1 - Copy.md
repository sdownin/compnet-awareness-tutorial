---
title: "amj_run_TERGM_tutorial_1.R"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---



## Tutorial Parts
- Part 1 (You are here.)
- [Part 2](https://www.jstatsoft.org/index.php/jss/article/view/v024i03/v24i03.pdf  "download")
- [Part 3](https://arxiv.org/pdf/1708.02598.pdf  "link")
- [Part 4](https://www.jstatsoft.org/index.php/jss/article/view/v083i06/v83i06.pdf "download")


# Part 1: Analyzing Existing Network Data Sample



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
fit0 <- btergm(m0, R=R, parallel = "multicore", ncpus = detectCores())
```

```
## 
## Initial dimensions of the network and covariates:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## All networks are conformable.
```

```
## 
## Dimensions of the network and covariates after adjustment:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## Starting pseudolikelihood estimation with 100 bootstrapping replications using multicore forking on 4 cores...
```

```
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
fit1 <- btergm(m1, R=R, parallel = "multicore", ncpus = detectCores())  
```

```
## 
## Initial dimensions of the network and covariates:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## All networks are conformable.
```

```
## 
## Dimensions of the network and covariates after adjustment:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## Starting pseudolikelihood estimation with 100 bootstrapping replications using multicore forking on 4 cores...
```

```
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
```

```
## The table was written to the file 'C:\Users\T430\Google Drive\PhD\Dissertation\competition networks\compnet-awareness-tutorial\data/qualtrics_tergm_results_pd8_R100_m0-m1.html'.
```

```r
##cat('\nfinished successfully.\n')
```

### Check Goodness of Fit (GOF) and Degeneracy Checks

Check `m0` goodness of fit for the following diagnostic statistics by simulating `nsim` number of random networks from model `m0`:
- `dsp` dyad-wise shared partners
- `esp` edge-wise shared partners
- `degree` degree distribution
- `geodesic` shortest path distribution
On the order of 1000 to 10,000 networks should be smapled for reporting goodness of fit results, but instruction we briefly simulate 30 per period.

```r
gof0 <- gof(fit0, statistics=c(dsp, esp, deg, geodesic), nsim=30)
```

```
## 
## Starting GOF assessment on a single computing core....
```

```
## 
## Initial dimensions of the network and covariates:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## All networks are conformable.
```

```
## 
## Dimensions of the network and covariates after adjustment:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## No 'target' network(s) provided. Using networks on the left-hand side of the model formula as observed networks.
```

```
## Simulating 30 networks from the following formula:
##  nets[[1]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[1]])
```

```
## Simulating 30 networks from the following formula:
##  nets[[2]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[2]])
```

```
## Simulating 30 networks from the following formula:
##  nets[[3]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[3]])
```

```
## Simulating 30 networks from the following formula:
##  nets[[4]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[4]])
```

```
## Simulating 30 networks from the following formula:
##  nets[[5]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[5]])
```

```
## Simulating 30 networks from the following formula:
##  nets[[6]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[6]])
```

```
## Simulating 30 networks from the following formula:
##  nets[[7]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + edgecov(memory[[7]])
```

```
## 7 networks from which simulations are drawn were provided.
```

```
## Processing statistic: Dyad-wise shared partners
```

```
## Processing statistic: Edge-wise shared partners
```

```
## Processing statistic: Degree
```

```
## Processing statistic: Geodesic distances
```

```r
print(gof0)
```

```
## Dyad-wise shared partners
```

```
##     obs: mean median   min   max  sim: mean median   min   max    Pr(>z)
## 0  2.6815e+04  26890 23734 30776 2.7695e+04  27529 23360 31484 0.4762126
## 1  4.1563e+03   4294   972  6534 3.4573e+03   3760   556  6896 0.4697068
## 2  9.1171e+02    698   366  1456 7.8336e+02    661   130  1766 0.5246196
## 3  1.9686e+02    198    72   298 1.7090e+02    141    28   374 0.5320781
## 4  8.2857e+01     90    26   130 6.5771e+01     73     4   156 0.3239972
## 5  3.1143e+01     28     4    58 2.5724e+01     26     0    78 0.5430098
## 6  9.1429e+00      8     0    16 8.3524e+00      8     0    28 0.7849971
## 7  6.2857e+00      6     0    14 4.2381e+00      2     0    16 0.3638171
## 8  2.5714e+00      2     0     6 1.8952e+00      0     0    10 0.5446789
## 9  5.7143e-01      0     0     4 1.0571e+00      0     0     6 0.4332566
## 10 3.1429e+00      2     0     8 2.2381e+00      0     0    10 0.5161993
## 11 8.5714e-01      0     0     2 8.1905e-01      0     0     6 0.9291793
## 12 2.8571e-01      0     0     2 2.5714e-01      0     0     4 0.9246306
## 13 0.0000e+00      0     0     0 1.9048e-02      0     0     2 0.1577941
## 14 8.5714e-01      0     0     2 4.1905e-01      0     0     2 0.3226778
## 15 0.0000e+00      0     0     0 1.5238e-01      0     0     2 4.804e-05
## 16 0.0000e+00      0     0     0 0.0000e+00      0     0     0 1.0000000
## 17 0.0000e+00      0     0     0 2.3810e-01      0     0     2 2.735e-07
## 18 2.8571e-01      0     0     2 4.7619e-02      0     0     2 0.4374178
## 19 2.8571e-01      0     0     2 2.5714e-01      0     0     2 0.9244243
## 20 0.0000e+00      0     0     0 3.1429e-01      0     0     2 2.366e-09
## 21 2.8571e-01      0     0     2 1.7143e-01      0     0     2 0.7050653
## 22 5.7143e-01      0     0     2 8.5714e-02      0     0     2 0.2366290
## 23 2.8571e-01      0     0     2 2.7619e-01      0     0     2 0.9747830
## 24 2.8571e-01      0     0     2 4.6667e-01      0     0     2 0.5560331
## 25 8.5714e-01      0     0     2 2.8571e-01      0     0     4 0.2088791
## 26 0.0000e+00      0     0     0 1.1429e-01      0     0     2 0.0004607
## 27 0.0000e+00      0     0     0 2.8571e-02      0     0     2 0.0832610
## 28 0.0000e+00      0     0     0 0.0000e+00      0     0     0 1.0000000
##       
## 0     
## 1     
## 2     
## 3     
## 4     
## 5     
## 6     
## 7     
## 8     
## 9     
## 10    
## 11    
## 12    
## 13    
## 14    
## 15 ***
## 16    
## 17 ***
## 18    
## 19    
## 20 ***
## 21    
## 22    
## 23    
## 24    
## 25    
## 26 ***
## 27 .  
## 28
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

```
## Edge-wise shared partners
```

```
##    obs: mean median min max  sim: mean median min max    Pr(>z)    
## 0  150.85714    166  92 194 150.190476    157  72 206 0.9648816    
## 1  167.14286    156 102 234 175.876190    178  64 300 0.7020008    
## 2   84.57143     78  42 126  78.219048     72  18 146 0.6350366    
## 3   62.85714     74  30  92  55.647619     49  18 120 0.4986991    
## 4   32.85714     36  14  54  28.095238     30   2  70 0.4291294    
## 5   18.57143     18   2  34  14.647619     13   0  48 0.4844099    
## 6    7.14286      8   0  12   6.047619      6   0  24 0.6165301    
## 7    4.57143      4   0  10   3.228571      2   0  12 0.4255438    
## 8    1.71429      2   0   4   1.247619      0   0   6 0.5221666    
## 9    0.57143      0   0   4   1.019048      0   0   6 0.4683639    
## 10   3.14286      2   0   8   2.238095      0   0  10 0.5161993    
## 11   0.57143      0   0   2   0.438095      0   0   6 0.7341107    
## 12   0.00000      0   0   0   0.085714      0   0   4 0.0063706 ** 
## 13   0.00000      0   0   0   0.000000      0   0   0 1.0000000    
## 14   0.00000      0   0   0   0.000000      0   0   0 1.0000000    
## 15   0.00000      0   0   0   0.019048      0   0   2 0.1577941    
## 16   0.00000      0   0   0   0.000000      0   0   0 1.0000000    
## 17   0.00000      0   0   0   0.238095      0   0   2 2.735e-07 ***
## 18   0.28571      0   0   2   0.047619      0   0   2 0.4374178    
## 19   0.28571      0   0   2   0.257143      0   0   2 0.9244243    
## 20   0.00000      0   0   0   0.314286      0   0   2 2.366e-09 ***
## 21   0.28571      0   0   2   0.171429      0   0   2 0.7050653    
## 22   0.57143      0   0   2   0.085714      0   0   2 0.2366290    
## 23   0.28571      0   0   2   0.276190      0   0   2 0.9747830    
## 24   0.28571      0   0   2   0.466667      0   0   2 0.5560331    
## 25   0.85714      0   0   2   0.285714      0   0   4 0.2088791    
## 26   0.00000      0   0   0   0.114286      0   0   2 0.0004607 ***
## 27   0.00000      0   0   0   0.028571      0   0   2 0.0832610 .  
## 28   0.00000      0   0   0   0.000000      0   0   0 1.0000000
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

```
## Degree
```

```
##    obs: mean median min max  sim: mean median min max    Pr(>z)    
## 0   50.71429     46  12 105 55.8238095     53  11 110 0.7432710    
## 1   45.00000     45  22  61 32.3761905     32  19  48 0.0810802 .  
## 2   21.28571     24  14  26 26.9380952     28  10  44 0.0316864 *  
## 3   18.28571     16  11  25 19.0714286     19   5  36 0.7410391    
## 4    9.42857      9   7  12 11.1190476     11   1  22 0.0706291 .  
## 5   11.42857     12   6  17 10.3476190     10   1  20 0.5105855    
## 6    8.57143     10   4  12  7.8333333      7   0  17 0.5384777    
## 7    2.00000      2   1   3  4.1047619      4   0  13 0.0001864 ***
## 8    3.71429      4   3   5  3.0380952      3   0   7 0.0572705 .  
## 9    1.42857      2   0   2  1.9142857      2   0   6 0.1594537    
## 10   1.00000      1   0   2  1.4095238      1   0   5 0.2407204    
## 11   1.42857      1   1   2  1.1380952      1   0   6 0.2142620    
## 12   0.42857      0   0   1  0.4190476      0   0   2 0.9644844    
## 13   0.42857      0   0   1  0.3000000      0   0   2 0.5526279    
## 14   0.28571      0   0   1  0.3285714      0   0   2 0.8268817    
## 15   0.28571      0   0   1  0.3952381      0   0   2 0.5798620    
## 16   0.00000      0   0   0  0.1523810      0   0   1 4.326e-09 ***
## 17   0.00000      0   0   0  0.1142857      0   0   2 1.426e-06 ***
## 18   0.28571      0   0   1  0.1095238      0   0   1 0.3783896    
## 19   0.28571      0   0   1  0.0666667      0   0   1 0.2809840    
## 20   0.14286      0   0   1  0.1285714      0   0   1 0.9244243    
## 21   0.14286      0   0   1  0.1428571      0   0   2 1.0000000    
## 22   0.57143      1   0   1  0.2904762      0   0   2 0.2166996    
## 23   0.00000      0   0   0  0.1333333      0   0   1 4.698e-08 ***
## 24   0.00000      0   0   0  0.0714286      0   0   1 8.464e-05 ***
## 25   0.14286      0   0   1  0.0714286      0   0   1 0.6369134    
## 26   0.14286      0   0   1  0.0952381      0   0   1 0.7521784    
## 27   0.00000      0   0   0  0.1095238      0   0   1 8.745e-07 ***
## 28   0.28571      0   0   1  0.2476190      0   0   1 0.8448380    
## 29   0.28571      0   0   1  0.1142857      0   0   1 0.3906779    
## 30   0.00000      0   0   0  0.0142857      0   0   1 0.0832610 .  
## 31   0.00000      0   0   0  0.0476190      0   0   1 0.0014252 ** 
## 32   0.14286      0   0   1  0.0666667      0   0   1 0.6149535    
## 33   0.00000      0   0   0  0.0333333      0   0   1 0.0078449 ** 
## 34   0.00000      0   0   0  0.0047619      0   0   1 0.3184669    
## 35   0.00000      0   0   0  0.0000000      0   0   0 1.0000000    
## 36   0.00000      0   0   0  0.0000000      0   0   0 1.0000000    
## 37   0.00000      0   0   0  0.0000000      0   0   0 1.0000000    
## 38   0.00000      0   0   0  0.0047619      0   0   1 0.3184669    
## 39   0.00000      0   0   0  0.1047619      0   0   1 1.559e-06 ***
## 40   0.14286      0   0   1  0.0285714      0   0   1 0.4552264    
## 41   0.00000      0   0   0  0.0047619      0   0   1 0.3184669    
## 42   0.00000      0   0   0  0.0000000      0   0   0 1.0000000    
## 43   0.00000      0   0   0  0.0000000      0   0   0 1.0000000    
## 44   0.28571      0   0   1  0.0809524      0   0   1 0.3108619    
## 45   0.28571      0   0   1  0.1571429      0   0   1 0.5146501    
## 46   0.00000      0   0   0  0.1238095      0   0   1 1.524e-07 ***
## 47   0.14286      0   0   1  0.1047619      0   0   1 0.8004085    
## 48   0.00000      0   0   0  0.0428571      0   0   1 0.0025105 ** 
## 49   0.14286      0   0   1  0.0714286      0   0   1 0.6369134    
## 50   0.14286      0   0   1  0.1714286      0   0   1 0.8501175    
## 51   0.14286      0   0   1  0.1904762      0   0   1 0.7536980    
## 52   0.28571      0   0   2  0.1047619      0   0   2 0.5508579    
## 53   0.00000      0   0   0  0.0809524      0   0   1 2.724e-05 ***
## 54   0.00000      0   0   0  0.0095238      0   0   1 0.1577941    
## 55   0.00000      0   0   0  0.0857143      0   0   1 1.542e-05 ***
## 56   0.14286      0   0   1  0.0571429      0   0   1 0.5722807    
## 57   0.14286      0   0   1  0.0047619      0   0   1 0.3712110    
## 58   0.00000      0   0   0  0.0000000      0   0   0 1.0000000
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

```
## Geodesic distances
```

```
##     obs: mean median  min   max  sim: mean median  min   max   Pr(>z)    
## 1      537.43    548  284   728 5.1929e+02    522  216   808 0.812902    
## 2     5018.86   4948 1252  7936 4.1557e+03   4342  608  8254 0.462454    
## 3     6750.86   5620 1542 11706 5.8890e+03   5059  980 12830 0.640328    
## 4     4814.00   5046 1550  7518 3.9995e+03   3968 1032  7100 0.423672    
## 5      471.71    536  116   812 8.7863e+02    974   68  1972 0.011941 *  
## 6       82.00     14    0   278 2.5990e+02    255    0   836 0.005932 ** 
## 7       10.00      0    0    38 8.0190e+01     30    0   472 6.11e-08 ***
## 8        0.00      0    0     0 1.9952e+01      2    0   272 2.33e-10 ***
## 9        0.00      0    0     0 5.2286e+00      0    0   152 2.81e-05 ***
## 10       0.00      0    0     0 1.3619e+00      0    0    88 0.009096 ** 
## 11       0.00      0    0     0 2.9524e-01      0    0    32 0.074865 .  
## 12       0.00      0    0     0 6.6667e-02      0    0     8 0.126931    
## 13       0.00      0    0     0 9.5238e-03      0    0     2 0.318467    
## 14       0.00      0    0     0 0.0000e+00      0    0     0 1.000000    
## Inf  14535.14  15184 4164 26670 1.6411e+04  17087 3828 28052 0.631090
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

Plot GOF statistics for `m0`

```r
plot(gof0)
```

![](amj_run_TERGM_tutorial_1_files/figure-html/gof_0_plot-1.png)<!-- -->

Now compare the GOF for `m1`

```r
gof1 <- gof(fit1, statistics=c(dsp, esp, deg, geodesic), nsim=30)
```

```
## 
## Starting GOF assessment on a single computing core....
```

```
## 
## Initial dimensions of the network and covariates:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## All networks are conformable.
```

```
## 
## Dimensions of the network and covariates after adjustment:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## No 'target' network(s) provided. Using networks on the left-hand side of the model formula as observed networks.
```

```
## Simulating 30 networks from the following formula:
##  nets[[1]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[1]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## Simulating 30 networks from the following formula:
##  nets[[2]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[2]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## Simulating 30 networks from the following formula:
##  nets[[3]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[3]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## Simulating 30 networks from the following formula:
##  nets[[4]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[4]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## Simulating 30 networks from the following formula:
##  nets[[5]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[5]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## Simulating 30 networks from the following formula:
##  nets[[6]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[6]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## Simulating 30 networks from the following formula:
##  nets[[7]] ~ edges + gwesp(0, fixed = T) + gwdegree(0, fixed = T) + nodematch("ipo_status", diff = F) + nodematch("state_code", diff = F) + nodecov("age") + absdiff("age") + nodecov("cent_deg") + edgecov(memory[[7]]) + nodecov("genidx_multilevel") + nodecov("cent_pow_n0_4") + absdiff("cent_pow_n0_4") + cycle(3) + cycle(4)
```

```
## 7 networks from which simulations are drawn were provided.
```

```
## Processing statistic: Dyad-wise shared partners
```

```
## Processing statistic: Edge-wise shared partners
```

```
## Processing statistic: Degree
```

```
## Processing statistic: Geodesic distances
```

```r
print(gof1)
```

```
## Dyad-wise shared partners
```

```
##     obs: mean median   min   max  sim: mean median   min   max    Pr(>z)
## 0  2.6815e+04  26890 23734 30776 2.6475e+04  25227 21022 31432 0.7812126
## 1  4.1563e+03   4294   972  6534 4.3773e+03   5625   600  8458 0.8167212
## 2  9.1171e+02    698   366  1456 9.8446e+02    991   130  2014 0.7169791
## 3  1.9686e+02    198    72   298 2.2584e+02    230    32   492 0.4910735
## 4  8.2857e+01     90    26   130 8.6505e+01     84     4   200 0.8287811
## 5  3.1143e+01     28     4    58 3.4676e+01     42     0    82 0.6901652
## 6  9.1429e+00      8     0    16 1.3457e+01     12     0    38 0.1716471
## 7  6.2857e+00      6     0    14 8.0476e+00      8     0    28 0.4362546
## 8  2.5714e+00      2     0     6 3.0857e+00      2     0    16 0.6471140
## 9  5.7143e-01      0     0     4 2.5429e+00      2     0    12 0.0125747
## 10 3.1429e+00      2     0     8 2.1810e+00      2     0    10 0.4900297
## 11 8.5714e-01      0     0     2 1.2381e+00      0     0    10 0.4002964
## 12 2.8571e-01      0     0     2 6.2857e-01      0     0     8 0.2891486
## 13 0.0000e+00      0     0     0 4.1905e-01      0     0     4 4.167e-10
## 14 8.5714e-01      0     0     2 3.5238e-01      0     0     6 0.2613795
## 15 0.0000e+00      0     0     0 3.1429e-01      0     0     4 2.010e-08
## 16 0.0000e+00      0     0     0 2.8571e-01      0     0     4 4.364e-08
## 17 0.0000e+00      0     0     0 2.6667e-01      0     0     4 1.409e-07
## 18 2.8571e-01      0     0     2 2.6667e-01      0     0     2 0.9495807
## 19 2.8571e-01      0     0     2 2.9524e-01      0     0     2 0.9747995
## 20 0.0000e+00      0     0     0 3.2381e-01      0     0     2 1.291e-09
## 21 2.8571e-01      0     0     2 2.7619e-01      0     0     2 0.9747830
## 22 5.7143e-01      0     0     2 1.6190e-01      0     0     2 0.3108619
## 23 2.8571e-01      0     0     2 1.9048e-01      0     0     4 0.7523925
## 24 2.8571e-01      0     0     2 2.5714e-01      0     0     4 0.9246306
## 25 8.5714e-01      0     0     2 4.0000e-01      0     0     4 0.3039967
## 26 0.0000e+00      0     0     0 3.6190e-01      0     0     4 1.364e-08
## 27 0.0000e+00      0     0     0 3.3333e-01      0     0     4 3.519e-08
## 28 0.0000e+00      0     0     0 1.0476e-01      0     0     2 0.0008101
## 29 0.0000e+00      0     0     0 4.7619e-02      0     0     2 0.0249929
## 30 0.0000e+00      0     0     0 3.8095e-02      0     0     2 0.0452374
## 31 0.0000e+00      0     0     0 9.5238e-03      0     0     2 0.3184669
## 32 0.0000e+00      0     0     0 9.5238e-03      0     0     2 0.3184669
## 33 0.0000e+00      0     0     0 0.0000e+00      0     0     0 1.0000000
##       
## 0     
## 1     
## 2     
## 3     
## 4     
## 5     
## 6     
## 7     
## 8     
## 9  *  
## 10    
## 11    
## 12    
## 13 ***
## 14    
## 15 ***
## 16 ***
## 17 ***
## 18    
## 19    
## 20 ***
## 21    
## 22    
## 23    
## 24    
## 25    
## 26 ***
## 27 ***
## 28 ***
## 29 *  
## 30 *  
## 31    
## 32    
## 33
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

```
## Edge-wise shared partners
```

```
##    obs: mean median min max  sim: mean median min max    Pr(>z)    
## 0  150.85714    166  92 194 1.3926e+02    138  70 194 0.4542258    
## 1  167.14286    156 102 234 1.6317e+02    169  60 284 0.8604042    
## 2   84.57143     78  42 126 9.1562e+01     97  14 176 0.6043855    
## 3   62.85714     74  30  92 5.8486e+01     50  18 120 0.6784580    
## 4   32.85714     36  14  54 3.5238e+01     35   0  78 0.6899464    
## 5   18.57143     18   2  34 1.9067e+01     20   0  48 0.9283142    
## 6    7.14286      8   0  12 9.8095e+00     10   0  28 0.2493395    
## 7    4.57143      4   0  10 6.5714e+00      6   0  24 0.2557805    
## 8    1.71429      2   0   4 2.2571e+00      2   0  10 0.4681195    
## 9    0.57143      0   0   4 2.2000e+00      2   0  10 0.0288333 *  
## 10   3.14286      2   0   8 2.0667e+00      1   0  10 0.4420056    
## 11   0.57143      0   0   2 1.2381e+00      0   0  10 0.1300864    
## 12   0.00000      0   0   0 6.2857e-01      0   0   8 6.361e-11 ***
## 13   0.00000      0   0   0 4.1905e-01      0   0   4 4.167e-10 ***
## 14   0.00000      0   0   0 3.4286e-01      0   0   6 2.860e-07 ***
## 15   0.00000      0   0   0 2.6667e-01      0   0   4 3.660e-07 ***
## 16   0.00000      0   0   0 1.8095e-01      0   0   4 2.445e-05 ***
## 17   0.00000      0   0   0 1.3333e-01      0   0   2 0.0001490 ***
## 18   0.28571      0   0   2 1.1429e-01      0   0   2 0.5722807    
## 19   0.28571      0   0   2 1.4286e-01      0   0   2 0.6369134    
## 20   0.00000      0   0   0 2.3810e-01      0   0   2 2.735e-07 ***
## 21   0.28571      0   0   2 2.1905e-01      0   0   2 0.8248759    
## 22   0.57143      0   0   2 1.4286e-01      0   0   2 0.2906634    
## 23   0.28571      0   0   2 1.5238e-01      0   0   4 0.6595427    
## 24   0.28571      0   0   2 2.3810e-01      0   0   4 0.8746005    
## 25   0.85714      0   0   2 3.9048e-01      0   0   4 0.2948466    
## 26   0.00000      0   0   0 3.6190e-01      0   0   4 1.364e-08 ***
## 27   0.00000      0   0   0 3.3333e-01      0   0   4 3.519e-08 ***
## 28   0.00000      0   0   0 1.0476e-01      0   0   2 0.0008101 ***
## 29   0.00000      0   0   0 4.7619e-02      0   0   2 0.0249929 *  
## 30   0.00000      0   0   0 3.8095e-02      0   0   2 0.0452374 *  
## 31   0.00000      0   0   0 9.5238e-03      0   0   2 0.3184669    
## 32   0.00000      0   0   0 9.5238e-03      0   0   2 0.3184669    
## 33   0.00000      0   0   0 0.0000e+00      0   0   0 1.0000000
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

```
## Degree
```

```
##    obs: mean median min max  sim: mean median min max    Pr(>z)    
## 0   50.71429     46  12 105 55.2714286   48.0   9 118 0.7702925    
## 1   45.00000     45  22  61 38.9476190   38.0  22  58 0.3550056    
## 2   21.28571     24  14  26 24.7238095   26.0   9  41 0.1469390    
## 3   18.28571     16  11  25 16.6809524   17.0   2  34 0.5047365    
## 4    9.42857      9   7  12 10.0619048   10.5   1  21 0.4557195    
## 5   11.42857     12   6  17  9.5666667    9.0   1  21 0.2719355    
## 6    8.57143     10   4  12  7.3523810    7.0   1  16 0.3205023    
## 7    2.00000      2   1   3  4.3761905    4.0   0  10 6.348e-05 ***
## 8    3.71429      4   3   5  2.7523810    3.0   0   8 0.0140353 *  
## 9    1.42857      2   0   2  1.8952381    2.0   0   4 0.1741906    
## 10   1.00000      1   0   2  1.3380952    1.0   0   5 0.3231522    
## 11   1.42857      1   1   2  0.9714286    1.0   0   5 0.0665010 .  
## 12   0.42857      0   0   1  0.7238095    1.0   0   3 0.2010782    
## 13   0.42857      0   0   1  0.5285714    0.0   0   4 0.6467909    
## 14   0.28571      0   0   1  0.2809524    0.0   0   2 0.9805549    
## 15   0.28571      0   0   1  0.3476190    0.0   0   2 0.7529167    
## 16   0.00000      0   0   0  0.3714286    0.0   0   2 < 2.2e-16 ***
## 17   0.00000      0   0   0  0.1809524    0.0   0   1 1.108e-10 ***
## 18   0.28571      0   0   1  0.1428571    0.0   0   2 0.4710908    
## 19   0.28571      0   0   1  0.1809524    0.0   0   2 0.5937300    
## 20   0.14286      0   0   1  0.1190476    0.0   0   2 0.8744872    
## 21   0.14286      0   0   1  0.2000000    0.0   0   2 0.7076634    
## 22   0.57143      1   0   1  0.1476190    0.0   0   2 0.0811424 .  
## 23   0.00000      0   0   0  0.1857143    0.0   0   2 5.591e-10 ***
## 24   0.00000      0   0   0  0.1285714    0.0   0   1 8.473e-08 ***
## 25   0.14286      0   0   1  0.1047619    0.0   0   1 0.8004085    
## 26   0.14286      0   0   1  0.0619048    0.0   0   1 0.5934037    
## 27   0.00000      0   0   0  0.1047619    0.0   0   1 1.559e-06 ***
## 28   0.28571      0   0   1  0.1714286    0.0   0   2 0.5613693    
## 29   0.28571      0   0   1  0.1285714    0.0   0   1 0.4293359    
## 30   0.00000      0   0   0  0.1619048    0.0   0   1 1.291e-09 ***
## 31   0.00000      0   0   0  0.0952381    0.0   0   1 4.921e-06 ***
## 32   0.14286      0   0   1  0.0666667    0.0   0   1 0.6149535    
## 33   0.00000      0   0   0  0.0285714    0.0   0   1 0.0139540 *  
## 34   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 35   0.00000      0   0   0  0.0142857    0.0   0   1 0.0832610 .  
## 36   0.00000      0   0   0  0.0333333    0.0   0   1 0.0078449 ** 
## 37   0.00000      0   0   0  0.0428571    0.0   0   1 0.0025105 ** 
## 38   0.00000      0   0   0  0.0190476    0.0   0   1 0.0452374 *  
## 39   0.00000      0   0   0  0.0380952    0.0   0   1 0.0044309 ** 
## 40   0.14286      0   0   1  0.0000000    0.0   0   0 0.3559177    
## 41   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 42   0.00000      0   0   0  0.0047619    0.0   0   1 0.3184669    
## 43   0.00000      0   0   0  0.0000000    0.0   0   0 1.0000000    
## 44   0.28571      0   0   1  0.0047619    0.0   0   1 0.1785568    
## 45   0.28571      0   0   1  0.0238095    0.0   0   1 0.2057271    
## 46   0.00000      0   0   0  0.0428571    0.0   0   1 0.0025105 ** 
## 47   0.14286      0   0   1  0.0333333    0.0   0   1 0.4735323    
## 48   0.00000      0   0   0  0.0380952    0.0   0   1 0.0044309 ** 
## 49   0.14286      0   0   1  0.0333333    0.0   0   1 0.4735323    
## 50   0.14286      0   0   1  0.0428571    0.0   0   1 0.5116132    
## 51   0.14286      0   0   1  0.0809524    0.0   0   1 0.6819897    
## 52   0.28571      0   0   2  0.0714286    0.0   0   1 0.4822211    
## 53   0.00000      0   0   0  0.1190476    0.0   0   2 2.018e-06 ***
## 54   0.00000      0   0   0  0.1047619    0.0   0   2 4.473e-06 ***
## 55   0.00000      0   0   0  0.1142857    0.0   0   2 7.778e-06 ***
## 56   0.14286      0   0   1  0.0904762    0.0   0   1 0.7284695    
## 57   0.14286      0   0   1  0.0619048    0.0   0   2 0.5937159    
## 58   0.00000      0   0   0  0.0523810    0.0   0   1 0.0008101 ***
## 59   0.00000      0   0   0  0.0380952    0.0   0   1 0.0044309 ** 
## 60   0.00000      0   0   0  0.0238095    0.0   0   1 0.0249929 *  
## 61   0.00000      0   0   0  0.0761905    0.0   0   1 4.804e-05 ***
## 62   0.00000      0   0   0  0.0714286    0.0   0   2 0.0002294 ***
## 63   0.00000      0   0   0  0.0666667    0.0   0   1 0.0001490 ***
## 64   0.00000      0   0   0  0.0761905    0.0   0   2 0.0001314 ***
## 65   0.00000      0   0   0  0.0380952    0.0   0   1 0.0044309 ** 
## 66   0.00000      0   0   0  0.0333333    0.0   0   1 0.0078449 ** 
## 67   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 68   0.00000      0   0   0  0.0047619    0.0   0   1 0.3184669    
## 69   0.00000      0   0   0  0.0190476    0.0   0   1 0.0452374 *  
## 70   0.00000      0   0   0  0.0000000    0.0   0   0 1.0000000    
## 71   0.00000      0   0   0  0.0047619    0.0   0   1 0.3184669    
## 72   0.00000      0   0   0  0.0000000    0.0   0   0 1.0000000    
## 73   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 74   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 75   0.00000      0   0   0  0.0047619    0.0   0   1 0.3184669    
## 76   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 77   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 78   0.00000      0   0   0  0.0095238    0.0   0   1 0.1577941    
## 79   0.00000      0   0   0  0.0000000    0.0   0   0 1.0000000
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

```
## Geodesic distances
```

```
##     obs: mean median  min   max  sim: mean median  min   max   Pr(>z)   
## 1      537.43    548  284   728 5.3544e+02    573  204   808 0.979255   
## 2     5018.86   4948 1252  7936 5.3485e+03   6567  658 10568 0.776198   
## 3     6750.86   5620 1542 11706 6.4890e+03   6130  952 13492 0.886231   
## 4     4814.00   5046 1550  7518 3.2026e+03   3027  944  6400 0.138854   
## 5      471.71    536  116   812 5.4467e+02    558   20  1294 0.559407   
## 6       82.00     14    0   278 1.1897e+02     77    0   450 0.442898   
## 7       10.00      0    0    38 1.9933e+01      2    0   256 0.191117   
## 8        0.00      0    0     0 2.4381e+00      0    0   120 0.001334 **
## 9        0.00      0    0     0 2.3810e-01      0    0     8 0.005793 **
## 10       0.00      0    0     0 1.9048e-02      0    0     2 0.157794   
## 11       0.00      0    0     0 0.0000e+00      0    0     0 1.000000   
## Inf  14535.14  15184 4164 26670 1.5958e+04  15448 3150 28678 0.714688
```

```
## 
## Note: Small p-values indicate a significant difference 
##       between simulations and observed network(s).
```

Plot GOF statistics for `m1`

```r
plot(gof1)
```

![](amj_run_TERGM_tutorial_1_files/figure-html/gof_1_plot-1.png)<!-- -->

Check degeneracy of `m0` with another sample of random networks based on model parameters (instead of the diagnostic statistics used for GOF above).

```r
degen0 <- checkdegeneracy(fit0, nsim=30)

print(degen0)
```

```
## 
## Degeneracy check for network 1:
```

```
##                           obs     2.5%      25%      50%      75%    97.5%
## edges                 142.000  109.725  113.250  121.000  124.750  133.475
## gwesp.fixed.0          96.000   58.000   62.000   66.500   69.000   75.275
## gwdegree               75.000   70.450   75.500   78.000   82.000   85.275
## nodematch.ipo_status  142.000  108.725  113.250  121.000  124.000  133.475
## nodematch.state_code   19.000   11.000   13.000   14.000   16.000   17.275
## nodecov.age          2271.000 1842.175 1942.250 1984.500 2098.250 2369.850
## absdiff.age          1109.000  827.325  902.250  958.500 1056.000 1281.150
## edgecov.memory[[i]]    42.000   46.525   56.000   60.500   66.750   70.825
```

```
## 
## Degeneracy check for network 2:
```

```
##                           obs     2.5%      25%      50%      75%    97.5%
## edges                 158.000  164.175  170.250  174.500  179.750  185.275
## gwesp.fixed.0         104.000   99.450  104.750  109.000  112.750  119.000
## gwdegree               85.000   89.175   94.000   95.000   97.000  104.000
## nodematch.ipo_status  158.000  164.175  169.250  174.000  179.000  185.275
## nodematch.state_code   22.000   19.450   21.000   22.000   24.000   26.275
## nodecov.age          2735.000 2659.775 2779.000 2839.500 2933.250 3089.775
## absdiff.age          1277.000 1232.675 1325.250 1399.500 1460.500 1636.525
## edgecov.memory[[i]]   122.000   96.725  101.250  106.500  111.750  117.100
```

```
## 
## Degeneracy check for network 3:
```

```
##                           obs     2.5%      25%      50%      75%    97.5%
## edges                 228.000  175.725  182.250  186.000  191.750  200.825
## gwesp.fixed.0         156.000  103.350  111.000  117.000  121.000  127.375
## gwdegree              114.000   98.725  100.000  104.000  108.000  112.650
## nodematch.ipo_status  228.000  174.725  182.250  186.000  191.750  200.825
## nodematch.state_code   33.000   23.000   24.000   25.000   26.000   29.275
## nodecov.age          3905.000 3091.000 3177.750 3249.000 3358.500 3543.500
## absdiff.age          2049.000 1395.625 1447.750 1519.500 1584.250 1805.150
## edgecov.memory[[i]]    78.000  105.175  114.000  119.000  122.000  128.100
```

```
## 
## Degeneracy check for network 4:
```

```
##                          obs    2.5%     25%     50%     75%   97.5%
## edges                 274.00  249.80  256.25  260.00  266.75  274.55
## gwesp.fixed.0         191.00  171.45  177.50  181.50  185.75  196.28
## gwdegree              134.00  119.72  123.25  125.50  128.50  134.82
## nodematch.ipo_status  271.00  247.07  254.00  258.00  263.75  271.55
## nodematch.state_code   37.00   32.45   34.25   35.00   36.00   39.55
## nodecov.age          4792.00 4541.60 4646.75 4726.50 4849.25 4966.07
## absdiff.age          2428.00 2246.55 2332.50 2389.50 2472.25 2644.32
## edgecov.memory[[i]]   168.00  167.18  175.00  178.00  186.00  191.28
```

```
## 
## Degeneracy check for network 5:
```

```
##                          obs    2.5%     25%     50%     75%    97.5%
## edges                 357.00  293.45  301.25  305.00  308.00  314.550
## gwesp.fixed.0         270.00  207.72  213.00  218.00  222.00  228.000
## gwdegree              163.00  133.72  137.00  139.00  140.75  144.825
## nodematch.ipo_status  351.00  290.18  298.00  301.00  303.75  310.275
## nodematch.state_code   48.00   36.00   38.25   40.50   42.00   45.275
## nodecov.age          6197.00 5465.27 5629.75 5667.00 5806.75 6092.700
## absdiff.age          3187.00 2618.60 2696.50 2790.00 2874.75 3063.750
## edgecov.memory[[i]]   177.00  213.45  217.75  222.00  225.75  233.100
```

```
## 
## Degeneracy check for network 6:
```

```
##                           obs     2.5%      25%      50%      75%   97.5%
## edges                 364.000  378.900  388.250  392.000  399.000  403.55
## gwesp.fixed.0         275.000  288.350  296.250  303.000  306.750  317.55
## gwdegree              168.000  163.000  164.000  165.000  166.000  168.55
## nodematch.ipo_status  358.000  372.625  381.000  386.000  391.000  396.38
## nodematch.state_code   47.000   47.725   50.000   51.500   55.000   56.55
## nodecov.age          6847.000 7117.350 7255.500 7380.000 7460.500 7658.00
## absdiff.age          3201.000 3346.700 3448.500 3513.000 3579.000 3724.95
## edgecov.memory[[i]]   336.000  295.900  301.000  307.000  311.250  320.20
```

```
## 
## Degeneracy check for network 7:
```

```
##                          obs    2.5%     25%     50%     75%   97.5%
## edges                 358.00  370.73  375.00  378.00  383.00  387.27
## gwesp.fixed.0         261.00  275.73  280.25  283.50  288.50  293.27
## gwdegree              166.00  166.00  167.00  168.00  168.00  170.28
## nodematch.ipo_status  352.00  364.00  367.25  371.50  375.75  379.82
## nodematch.state_code   44.00   41.00   43.00   44.00   46.00   49.55
## nodecov.age          7670.00 7349.45 7530.75 7638.50 7756.25 7869.20
## absdiff.age          3272.00 3183.70 3320.25 3381.00 3415.50 3523.85
## edgecov.memory[[i]]   334.00  301.45  307.00  311.00  314.75  319.38
```

Plot degeneracy check of `m0` model parameters

```r
plot(degen0)
```

![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-1.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-2.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-3.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-4.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-5.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-6.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-7.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-8.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-9.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-10.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-11.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-12.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-13.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-14.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-15.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-16.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-17.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-18.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-19.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-20.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-21.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-22.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-23.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-24.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-25.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-26.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-27.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-28.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-29.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-30.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-31.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-32.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-33.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-34.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-35.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-36.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-37.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-38.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-39.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-40.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-41.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-42.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-43.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-44.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-45.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-46.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-47.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-48.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-49.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-50.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-51.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-52.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-53.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-54.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-55.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_0_plot-56.png)<!-- -->

And check degeneracy for `m1`

```r
degen1 <- checkdegeneracy(fit1, nsim=30)

print(degen1)
```

```
## 
## Degeneracy check for network 1:
```

```
##                                obs     2.5%      25%      50%      75%
## edges                      142.000  102.900  108.250  111.500  114.750
## gwesp.fixed.0               96.000   54.450   60.250   63.000   64.750
## gwdegree                    75.000   65.900   70.250   72.000   74.750
## nodematch.ipo_status       142.000  102.900  108.250  111.000  114.750
## nodematch.state_code        19.000   11.725   12.250   14.000   15.000
## nodecov.age               2271.000 1825.325 1900.250 1965.500 2033.500
## absdiff.age               1109.000  808.775  869.500  929.000 1001.750
## nodecov.cent_deg          2372.000 1558.650 1605.500 1659.000 1705.750
## edgecov.memory[[i]]         42.000   59.000   66.500   69.000   73.750
## nodecov.genidx_multilevel  159.620  110.008  114.144  117.525  121.104
## nodecov.cent_pow_n0_4       88.882   56.392   73.299   79.469   86.708
## absdiff.cent_pow_n0_4      356.320  286.640  298.067  306.808  313.882
## cycle3                      63.000   32.725   36.000   37.000   39.000
## cycle4                     274.000   82.725   87.000   92.000   96.750
##                              97.5%
## edges                      123.000
## gwesp.fixed.0               68.550
## gwdegree                    81.825
## nodematch.ipo_status       123.000
## nodematch.state_code        17.000
## nodecov.age               2226.700
## absdiff.age               1159.450
## nodecov.cent_deg          1763.725
## edgecov.memory[[i]]         78.550
## nodecov.genidx_multilevel  123.946
## nodecov.cent_pow_n0_4       97.518
## absdiff.cent_pow_n0_4      320.285
## cycle3                      40.825
## cycle4                     110.850
```

```
## 
## Degeneracy check for network 2:
```

```
##                                obs     2.5%      25%      50%      75%
## edges                      158.000  155.725  160.000  164.000  168.750
## gwesp.fixed.0              104.000   97.000  103.000  106.000  110.000
## gwdegree                    85.000   80.450   85.500   89.000   92.500
## nodematch.ipo_status       158.000  155.000  159.250  164.000  168.000
## nodematch.state_code        22.000   19.450   21.000   22.000   23.000
## nodecov.age               2735.000 2535.900 2687.000 2732.000 2851.000
## absdiff.age               1277.000 1152.450 1264.250 1312.000 1397.000
## nodecov.cent_deg          2816.000 2696.225 2734.250 2826.000 2861.000
## edgecov.memory[[i]]        122.000  103.625  108.250  112.000  115.750
## nodecov.genidx_multilevel  194.747  187.465  189.545  195.926  198.959
## nodecov.cent_pow_n0_4       42.578   31.397   42.274   49.193   56.291
## absdiff.cent_pow_n0_4      379.337  368.653  383.171  388.047  395.046
## cycle3                      69.000   68.000   70.250   73.500   75.000
## cycle4                     304.000  283.625  297.250  305.500  313.500
##                              97.5%
## edges                      174.375
## gwesp.fixed.0              117.100
## gwdegree                    97.825
## nodematch.ipo_status       174.100
## nodematch.state_code        26.275
## nodecov.age               2954.075
## absdiff.age               1464.850
## nodecov.cent_deg          3020.325
## edgecov.memory[[i]]        120.275
## nodecov.genidx_multilevel  206.506
## nodecov.cent_pow_n0_4       64.600
## absdiff.cent_pow_n0_4      408.175
## cycle3                      78.650
## cycle4                     352.750
```

```
## 
## Degeneracy check for network 3:
```

```
##                                obs     2.5%      25%      50%      75%
## edges                      228.000  183.725  191.000  197.000  201.750
## gwesp.fixed.0              156.000  113.450  120.000  130.500  134.000
## gwdegree                   114.000   95.725  101.000  102.500  105.750
## nodematch.ipo_status       228.000  183.725  190.250  196.500  201.750
## nodematch.state_code        33.000   23.725   25.000   26.000   28.000
## nodecov.age               3905.000 3073.300 3209.250 3303.000 3391.500
## absdiff.age               2049.000 1374.125 1474.750 1549.000 1620.250
## nodecov.cent_deg          6212.000 4078.075 4208.500 4424.500 4622.750
## edgecov.memory[[i]]         78.000   95.350  105.250  110.000  115.750
## nodecov.genidx_multilevel  314.776  218.917  224.719  236.135  247.667
## nodecov.cent_pow_n0_4      134.805   82.601   92.178  100.926  106.013
## absdiff.cent_pow_n0_4      391.104  346.240  355.427  367.708  377.220
## cycle3                     123.000   74.725   79.000   88.500   99.500
## cycle4                     665.000  332.900  362.000  401.000  454.750
##                             97.5%
## edges                      211.55
## gwesp.fixed.0              146.00
## gwdegree                   113.83
## nodematch.ipo_status       211.55
## nodematch.state_code        31.10
## nodecov.age               3561.70
## absdiff.age               1774.55
## nodecov.cent_deg          4866.77
## edgecov.memory[[i]]        121.55
## nodecov.genidx_multilevel  258.72
## nodecov.cent_pow_n0_4      120.80
## absdiff.cent_pow_n0_4      398.05
## cycle3                     112.28
## cycle4                     528.23
```

```
## 
## Degeneracy check for network 4:
```

```
##                                obs     2.5%      25%      50%      75%
## edges                      274.000  267.725  277.000  281.000  287.000
## gwesp.fixed.0              191.000  201.700  213.750  222.000  230.500
## gwdegree                   134.000  121.000  125.250  128.000  129.750
## nodematch.ipo_status       271.000  265.725  275.000  278.500  285.000
## nodematch.state_code        37.000   31.725   35.000   38.000   41.000
## nodecov.age               4792.000 4742.175 4836.500 4942.000 5020.750
## absdiff.age               2428.000 2391.925 2454.750 2521.500 2582.500
## nodecov.cent_deg          7528.000 8157.775 8383.000 8752.500 8814.500
## edgecov.memory[[i]]        168.000  140.900  153.000  157.000  161.750
## nodecov.genidx_multilevel  446.700  477.076  489.607  508.751  512.812
## nodecov.cent_pow_n0_4      195.460  166.019  173.892  183.491  188.665
## absdiff.cent_pow_n0_4      450.615  434.535  446.862  455.521  461.400
## cycle3                     156.000  170.975  193.000  197.500  204.750
## cycle4                     783.000  941.825 1125.500 1175.000 1291.750
##                             97.5%
## edges                      294.38
## gwesp.fixed.0              240.28
## gwdegree                   133.28
## nodematch.ipo_status       291.38
## nodematch.state_code        43.00
## nodecov.age               5179.75
## absdiff.age               2692.43
## nodecov.cent_deg          9064.67
## edgecov.memory[[i]]        172.28
## nodecov.genidx_multilevel  527.74
## nodecov.cent_pow_n0_4      202.47
## absdiff.cent_pow_n0_4      481.42
## cycle3                     223.55
## cycle4                    1446.00
```

```
## 
## Degeneracy check for network 5:
```

```
##                                 obs      2.5%       25%       50%
## edges                       357.000   311.725   318.250   321.500
## gwesp.fixed.0               270.000   230.450   236.250   241.000
## gwdegree                    163.000   137.725   141.000   142.500
## nodematch.ipo_status        351.000   307.725   314.000   317.500
## nodematch.state_code         48.000    39.725    41.250    43.000
## nodecov.age                6197.000  5606.250  5757.250  5837.000
## absdiff.age                3187.000  2708.700  2821.750  2887.000
## nodecov.cent_deg          11774.000  9915.425 10204.500 10328.500
## edgecov.memory[[i]]         177.000   197.525   205.250   210.500
## nodecov.genidx_multilevel   495.843   405.967   417.851   422.795
## nodecov.cent_pow_n0_4        83.339    74.304    89.251    93.840
## absdiff.cent_pow_n0_4       543.128   497.596   509.890   514.917
## cycle3                      238.000   198.725   205.500   211.000
## cycle4                     1492.000  1125.525  1188.750  1244.000
##                                 75%     97.5%
## edges                       326.000   335.375
## gwesp.fixed.0               246.000   256.275
## gwdegree                    144.750   146.000
## nodematch.ipo_status        321.750   332.550
## nodematch.state_code         44.000    46.275
## nodecov.age                5932.750  6103.400
## absdiff.age                2973.750  3040.750
## nodecov.cent_deg          10437.500 10693.575
## edgecov.memory[[i]]         214.000   220.275
## nodecov.genidx_multilevel   428.405   439.206
## nodecov.cent_pow_n0_4       103.869   117.172
## absdiff.cent_pow_n0_4       521.315   537.593
## cycle3                      218.000   227.575
## cycle4                     1287.000  1374.700
```

```
## 
## Degeneracy check for network 6:
```

```
##                                 obs      2.5%       25%       50%
## edges                       364.000   381.350   389.250   396.500
## gwesp.fixed.0               275.000   297.075   313.000   322.500
## gwdegree                    168.000   161.725   163.000   165.000
## nodematch.ipo_status        358.000   373.900   381.250   389.500
## nodematch.state_code         47.000    48.450    51.000    52.500
## nodecov.age                6847.000  7010.400  7133.500  7334.000
## absdiff.age                3201.000  3252.075  3349.500  3510.000
## nodecov.cent_deg          12180.000 12728.250 13118.000 13229.000
## edgecov.memory[[i]]         336.000   295.000   300.000   304.000
## nodecov.genidx_multilevel   491.650   515.034   530.976   536.390
## nodecov.cent_pow_n0_4        93.621    76.275    83.629    89.201
## absdiff.cent_pow_n0_4       542.199   553.226   562.489   575.944
## cycle3                      232.000   260.075   290.250   296.000
## cycle4                     1450.000  1736.300  1986.750  2055.500
##                                 75%     97.5%
## edges                       400.000   403.550
## gwesp.fixed.0               326.000   338.550
## gwdegree                    165.000   166.000
## nodematch.ipo_status        392.750   396.825
## nodematch.state_code         55.000    56.275
## nodecov.age                7467.750  7562.600
## absdiff.age                3598.250  3688.550
## nodecov.cent_deg          13394.750 13582.975
## edgecov.memory[[i]]         309.750   316.650
## nodecov.genidx_multilevel   544.364   550.716
## nodecov.cent_pow_n0_4        93.974   103.418
## absdiff.cent_pow_n0_4       579.311   595.088
## cycle3                      301.750   307.825
## cycle4                     2099.500  2215.625
```

```
## 
## Degeneracy check for network 7:
```

```
##                                 obs      2.5%       25%       50%
## edges                       358.000   383.000   389.000   393.000
## gwesp.fixed.0               261.000   285.725   300.250   307.000
## gwdegree                    166.000   166.000   167.000   167.500
## nodematch.ipo_status        352.000   376.725   381.250   386.000
## nodematch.state_code         44.000    44.725    48.000    49.000
## nodecov.age                7670.000  7716.000  7870.500  7934.500
## absdiff.age                3272.000  3372.500  3452.500  3500.500
## nodecov.cent_deg          11996.000 12756.825 12906.500 13156.500
## edgecov.memory[[i]]         334.000   284.800   300.500   306.500
## nodecov.genidx_multilevel   571.974   613.911   624.292   632.626
## nodecov.cent_pow_n0_4        97.888   105.524   122.135   132.324
## absdiff.cent_pow_n0_4       492.419   511.541   528.729   540.695
## cycle3                      221.000   259.700   271.000   282.500
## cycle4                     1421.000  1820.675  1909.750  1985.000
##                                 75%    97.5%
## edges                       399.250   413.20
## gwesp.fixed.0               313.750   326.00
## gwdegree                    168.000   169.00
## nodematch.ipo_status        391.500   403.93
## nodematch.state_code         50.750    54.00
## nodecov.age                8053.500  8290.95
## absdiff.age                3578.750  3710.57
## nodecov.cent_deg          13275.500 13688.10
## edgecov.memory[[i]]         310.750   314.10
## nodecov.genidx_multilevel   641.961   661.15
## nodecov.cent_pow_n0_4       143.451   163.15
## absdiff.cent_pow_n0_4       549.927   565.85
## cycle3                      290.000   304.27
## cycle4                     2100.500  2312.55
```

And plot degeneracy check for `m1`

```r
plot(degen1)
```

![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-1.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-2.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-3.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-4.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-5.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-6.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-7.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-8.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-9.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-10.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-11.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-12.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-13.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-14.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-15.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-16.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-17.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-18.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-19.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-20.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-21.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-22.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-23.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-24.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-25.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-26.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-27.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-28.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-29.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-30.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-31.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-32.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-33.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-34.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-35.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-36.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-37.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-38.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-39.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-40.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-41.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-42.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-43.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-44.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-45.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-46.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-47.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-48.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-49.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-50.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-51.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-52.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-53.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-54.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-55.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-56.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-57.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-58.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-59.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-60.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-61.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-62.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-63.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-64.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-65.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-66.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-67.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-68.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-69.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-70.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-71.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-72.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-73.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-74.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-75.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-76.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-77.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-78.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-79.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-80.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-81.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-82.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-83.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-84.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-85.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-86.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-87.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-88.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-89.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-90.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-91.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-92.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-93.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-94.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-95.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-96.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-97.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/degen_1_plot-98.png)<!-- -->


### Compare Estimation Algorithms: PMLE vs MCMCMLE

For instructional purposes, we will only compare the estamation algofirthm for the first model `m0`.  You may repeat the same steps as needed to compare other models.

Compute the first model `m0` again using MCMCMLE (instead of bootstrapped PMLE) and save to disk as an RDS (serialized) file:

```r
## set pseudorandom number generator seed for reproducibility
set.seed(1111)
## estimate the TERGM with bootstrapped PMLE
fit0m <- mtergm(m0, ctrl=control.ergm(seed = 1111))
```

```
## 
## Initial dimensions of the network and covariates:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## 
## All networks are conformable.
```

```
## 
## Dimensions of the network and covariates after adjustment:
```

```
##              t=2 t=3 t=4 t=5 t=6 t=7 t=8
## nets (row)   180 180 180 180 180 180 180
## nets (col)   180 180 180 180 180 180 180
## memory (row) 180 180 180 180 180 180 180
## memory (col) 180 180 180 180 180 180 180
```

```
## Estimating...
```

```
## Starting maximum likelihood estimation via MCMLE:
```

```
## Iteration 1 of at most 20:
```

```
## Optimizing with step length 0.380849430581003.
```

```
## The log-likelihood improved by 2.465.
```

```
## Iteration 2 of at most 20:
```

```
## Optimizing with step length 0.659003375233084.
```

```
## The log-likelihood improved by 2.668.
```

```
## Iteration 3 of at most 20:
```

```
## Optimizing with step length 1.
```

```
## The log-likelihood improved by 1.182.
```

```
## Step length converged once. Increasing MCMC sample size.
```

```
## Iteration 4 of at most 20:
```

```
## Optimizing with step length 1.
```

```
## The log-likelihood improved by 1.42.
```

```
## Step length converged twice. Stopping.
```

```
## Evaluating log-likelihood at the estimate. Using 20 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 .
## This model was fit using MCMC.  To examine model diagnostics and check for degeneracy, use the mcmc.diagnostics() function.
## Done.
```

```r
## SAVE SERIALIZED DATA
fit0m_file <- file.path(data_dir,sprintf('fit_%s_pd%s_%s.rds', firm_i, nPeriods, 'm0m'))
saveRDS(fit0m, file=fit0m_file)
```

Compare PMLE and MCMCMLE results

```r
## Cache model fits list
fits <- list(PMLE=fit0, MCMCMLE=fit0m)

## Echo model comparison table to screen
screenreg(fits, digits = 3)
```

```
## 
## ======================================================
##                       PMLE              MCMCMLE       
## ------------------------------------------------------
## edges                    -0.180             -0.477    
##                       [-1.749;  1.067]      (0.382)   
## gwesp.fixed.0             0.813 *            0.964 ***
##                       [ 0.658;  0.908]      (0.076)   
## gwdegree                 -1.145 *           -0.741 ***
##                       [-1.910; -0.527]      (0.144)   
## nodematch.ipo_status      0.132              0.175    
##                       [-0.259;  1.070]      (0.358)   
## nodematch.state_code     -0.593 *           -0.624 ***
##                       [-0.770; -0.387]      (0.169)   
## nodecov.age              -0.139 *           -0.128 ***
##                       [-0.199; -0.065]      (0.011)   
## absdiff.age               0.157 *            0.145 ***
##                       [ 0.085;  0.216]      (0.012)   
## edgecov.memory[[i]]       5.229 *                     
##                       [ 4.908;  5.769]                
## edgecov.memory                               5.047 ***
##                                             (0.144)   
## ------------------------------------------------------
## Num. obs.             13787             225540        
## ======================================================
## *** p < 0.001, ** p < 0.01, * p < 0.05 (or 0 outside the confidence interval).
```

```r
## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0PMLE-m0MCMCMLE'))
htmlreg(fits, digits = 3, file=compare_file)
```

```
## The table was written to the file 'C:\Users\T430\Google Drive\PhD\Dissertation\competition networks\compnet-awareness-tutorial\data/qualtrics_tergm_results_pd8_R100_m0PMLE-m0MCMCMLE.html'.
```


And compare the PMLE and MCMCMLE confidence intervals directly

```r
## Cache model fits list
fits <- list(PMLE=fit0, MCMCMLE=fit0m)

## Echo model comparison table to screen
screenreg(fits, digits = 3, ci.force = T, ci.force.level = .95)
```

```
## 
## ========================================================
##                       PMLE              MCMCMLE         
## --------------------------------------------------------
## edges                    -0.180             -0.477      
##                       [-1.749;  1.067]  [-1.225;  0.271]
## gwesp.fixed.0             0.813 *            0.964 *    
##                       [ 0.658;  0.908]  [ 0.814;  1.113]
## gwdegree                 -1.145 *           -0.741 *    
##                       [-1.910; -0.527]  [-1.023; -0.459]
## nodematch.ipo_status      0.132              0.175      
##                       [-0.259;  1.070]  [-0.527;  0.878]
## nodematch.state_code     -0.593 *           -0.624 *    
##                       [-0.770; -0.387]  [-0.956; -0.292]
## nodecov.age              -0.139 *           -0.128 *    
##                       [-0.199; -0.065]  [-0.150; -0.107]
## absdiff.age               0.157 *            0.145 *    
##                       [ 0.085;  0.216]  [ 0.122;  0.168]
## edgecov.memory[[i]]       5.229 *                       
##                       [ 4.908;  5.769]                  
## edgecov.memory                               5.047 *    
##                                         [ 4.765;  5.330]
## --------------------------------------------------------
## Num. obs.             13787             225540          
## ========================================================
## * 0 outside the confidence interval
```

```r
## SAVE FORMATTED REGRESSION TABLE
compare_file <- file.path(data_dir,sprintf('%s_tergm_results_pd%s_R%s_%s.html', firm_i, nPeriods, R, 'm0PMLE-m0MCMCMLE_ci'))
htmlreg(fits, digits = 3, file=compare_file, ci.force = T, ci.force.level = .05)
```

```
## The table was written to the file 'C:\Users\T430\Google Drive\PhD\Dissertation\competition networks\compnet-awareness-tutorial\data/qualtrics_tergm_results_pd8_R100_m0PMLE-m0MCMCMLE_ci.html'.
```

Finally, check the MCMCMLE diagnostics

```r
mcmc.diagnostics(fit0m@ergm)
```

```
## Sample statistics summary:
## 
## Iterations = 16384:4209664
## Thinning interval = 1024 
## Number of chains = 1 
## Sample size per chain = 4096 
## 
## 1. Empirical mean and standard deviation for each variable,
##    plus standard error of the mean:
## 
##                           Mean      SD Naive SE Time-series SE
## edges                   3.0723  19.700   0.3078         1.2698
## gwesp.fixed.0           3.5552  19.959   0.3119         1.8134
## gwdegree               -0.3508  11.232   0.1755         0.5734
## nodematch.ipo_status    1.2542  19.092   0.2983         1.3942
## nodematch.state_code   -3.6062   6.416   0.1002         0.3763
## nodecov.age           -35.6899 384.359   6.0056        27.4444
## absdiff.age          -134.2041 300.434   4.6943        19.9207
## edgecov.memory         10.8696  19.003   0.2969         1.1498
## 
## 2. Quantiles for each variable:
## 
##                         2.5%  25%  50% 75% 97.5%
## edges                 -36.00  -10    3  17  42.0
## gwesp.fixed.0         -39.00   -9    4  17  41.0
## gwdegree              -21.62   -8   -1   7  22.0
## nodematch.ipo_status  -36.62  -11    1  14  38.0
## nodematch.state_code  -16.00   -8   -4   1  10.0
## nodecov.age          -784.00 -307  -28 231 685.6
## absdiff.age          -702.00 -343 -132  73 453.6
## edgecov.memory        -27.00   -2   12  24  47.0
## 
## 
## Sample statistics cross-correlations:
##                           edges gwesp.fixed.0   gwdegree
## edges                 1.0000000     0.6314057  0.6581411
## gwesp.fixed.0         0.6314057     1.0000000  0.1204709
## gwdegree              0.6581411     0.1204709  1.0000000
## nodematch.ipo_status  0.9886884     0.6256350  0.6462060
## nodematch.state_code  0.3580458     0.2441033  0.2293714
## nodecov.age           0.7275301     0.4828927  0.3857011
## absdiff.age           0.5918817     0.3200693  0.3720640
## edgecov.memory       -0.8724774    -0.5287060 -0.5860218
##                      nodematch.ipo_status nodematch.state_code nodecov.age
## edges                           0.9886884            0.3580458   0.7275301
## gwesp.fixed.0                   0.6256350            0.2441033   0.4828927
## gwdegree                        0.6462060            0.2293714   0.3857011
## nodematch.ipo_status            1.0000000            0.3644899   0.7101860
## nodematch.state_code            0.3644899            1.0000000   0.2704267
## nodecov.age                     0.7101860            0.2704267   1.0000000
## absdiff.age                     0.5755261            0.2028273   0.9265124
## edgecov.memory                 -0.8753191           -0.3510019  -0.5997116
##                      absdiff.age edgecov.memory
## edges                  0.5918817     -0.8724774
## gwesp.fixed.0          0.3200693     -0.5287060
## gwdegree               0.3720640     -0.5860218
## nodematch.ipo_status   0.5755261     -0.8753191
## nodematch.state_code   0.2028273     -0.3510019
## nodecov.age            0.9265124     -0.5997116
## absdiff.age            1.0000000     -0.5667370
## edgecov.memory        -0.5667370      1.0000000
## 
## Sample statistics auto-correlation:
## Chain 1 
##              edges gwesp.fixed.0  gwdegree nodematch.ipo_status
## Lag 0    1.0000000     1.0000000 1.0000000            1.0000000
## Lag 1024 0.8838772     0.9425326 0.8096165            0.8798159
## Lag 2048 0.7864046     0.8876284 0.6656872            0.7803422
## Lag 3072 0.7030145     0.8392670 0.5566241            0.6960648
## Lag 4096 0.6326490     0.7922254 0.4692374            0.6253648
## Lag 5120 0.5733575     0.7489398 0.3961800            0.5646937
##          nodematch.state_code nodecov.age absdiff.age edgecov.memory
## Lag 0               1.0000000   1.0000000   1.0000000      1.0000000
## Lag 1024            0.8347718   0.8881187   0.8874959      0.8749346
## Lag 2048            0.7022984   0.7968271   0.7951130      0.7698871
## Lag 3072            0.5937905   0.7178424   0.7125952      0.6797544
## Lag 4096            0.4979769   0.6498921   0.6418456      0.6035623
## Lag 5120            0.4202840   0.5955045   0.5839419      0.5391573
## 
## Sample statistics burn-in diagnostic (Geweke):
## Chain 1 
## 
## Fraction in 1st window = 0.1
## Fraction in 2nd window = 0.5 
## 
##                edges        gwesp.fixed.0             gwdegree 
##              -2.4150              -1.6614              -1.7590 
## nodematch.ipo_status nodematch.state_code          nodecov.age 
##              -1.8425               1.1636              -1.2394 
##          absdiff.age       edgecov.memory 
##              -1.3510              -0.3909 
## 
## Individual P-values (lower = worse):
##                edges        gwesp.fixed.0             gwdegree 
##           0.01573314           0.09663062           0.07857317 
## nodematch.ipo_status nodematch.state_code          nodecov.age 
##           0.06539507           0.24456780           0.21520396 
##          absdiff.age       edgecov.memory 
##           0.17668597           0.69585895 
## Joint P-value (lower = worse):  5.638501e-08 .
```

```
## Warning in formals(fun): argument is not a function
```

![](amj_run_TERGM_tutorial_1_files/figure-html/mcmc_diag-1.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/mcmc_diag-2.png)<!-- -->![](amj_run_TERGM_tutorial_1_files/figure-html/mcmc_diag-3.png)<!-- -->

```
## 
## MCMC diagnostics shown here are from the last round of simulation, prior to computation of final parameter estimates. Because the final estimates are refinements of those used for this simulation run, these diagnostics may understate model performance. To directly assess the performance of the final model on in-model statistics, please use the GOF command: gof(ergmFitObject, GOF=~model).
```

This completes Part 1.

