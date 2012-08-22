> summary(model.nodemog)

Call:
glm(formula = case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + 
    HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + 
    BPs_sd + BPd_sd + BPm_sd + BPp_sd + HRT_slope + BPs_slope + 
    BPd_slope + BPm_slope + BPp_slope, family = "binomial")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0779  -0.4909  -0.2757  -0.1173   3.8645  

Coefficients:
                                               Estimate Std. Error z value Pr(>|z|)   
(Intercept)                                  -6.917e+01  6.503e+01  -1.064  0.28744   
HRT_spot                                      2.232e-02  1.238e-02   1.803  0.07140 . 
BPs_spot                                      1.293e-02  1.251e-02   1.034  0.30127   
BPd_spot                                      1.588e-02  2.193e-02   0.724  0.46893   
BPm_spot                                     -2.402e-02  2.516e-02  -0.955  0.33976   
HRT_mean                                      2.193e-01  7.527e-01   0.291  0.77080   
BPs_mean                                     -1.750e+01  2.590e+01  -0.676  0.49922   
BPd_mean                                      2.074e+01  2.598e+01   0.798  0.42476   
BPm_mean                                     -7.851e-01  1.074e+00  -0.731  0.46473   
BPp_mean                                      2.015e+01  2.601e+01   0.775  0.43858   
HRT_sd                                        1.657e-02  1.692e-02   0.980  0.32732   
BPs_sd                                       -2.223e-02  1.907e-02  -1.165  0.24391   
BPd_sd                                        2.985e-02  2.146e-02   1.391  0.16414   
BPm_sd                                        4.677e-02  1.479e-02   3.161  0.00157 **
BPp_sd                                        4.751e-02  1.981e-02   2.398  0.01648 * 
HRT_slope                                    -7.781e-02  5.948e-02  -1.308  0.19078   
BPs_slope                                    -3.308e-02  5.415e-02  -0.611  0.54132   
BPd_slope                                    -1.019e-01  7.498e-02  -1.360  0.17395   
BPm_slope                                     7.973e-02  6.413e-02   1.243  0.21376   
BPp_slope                                    -5.809e-02  5.991e-02  -0.970  0.33224   
HRT_mean:BPs_mean                             1.567e-01  2.472e-01   0.634  0.52608   
HRT_mean:BPd_mean                            -1.777e-01  2.484e-01  -0.716  0.47429   
BPs_mean:BPd_mean                            -2.719e-02  1.150e-02  -2.363  0.01811 * 
HRT_mean:BPm_mean                             1.042e-02  1.286e-02   0.811  0.41762   
BPs_mean:BPm_mean                             2.545e-01  3.698e-01   0.688  0.49118   
BPd_mean:BPm_mean                            -2.531e-01  3.715e-01  -0.681  0.49573   
HRT_mean:BPp_mean                            -1.769e-01  2.488e-01  -0.711  0.47711   
BPs_mean:BPp_mean                            -1.560e-02  6.792e-03  -2.297  0.02161 * 
BPd_mean:BPp_mean                            -3.570e-03  1.648e-02  -0.217  0.82853   
BPm_mean:BPp_mean                            -2.757e-01  3.717e-01  -0.742  0.45828   
HRT_mean:BPs_mean:BPd_mean                    2.059e-04  1.317e-04   1.564  0.11790   
HRT_mean:BPs_mean:BPm_mean                   -2.308e-03  3.622e-03  -0.637  0.52400   
HRT_mean:BPd_mean:BPm_mean                    2.227e-03  3.647e-03   0.611  0.54134   
BPs_mean:BPd_mean:BPm_mean                    8.537e-05  5.092e-05   1.677  0.09361 . 
HRT_mean:BPs_mean:BPp_mean                    1.335e-04  7.474e-05   1.787  0.07402 . 
HRT_mean:BPd_mean:BPp_mean                   -2.585e-05  1.959e-04  -0.132  0.89499   
BPs_mean:BPd_mean:BPp_mean                    1.415e-04  1.158e-04   1.223  0.22144   
HRT_mean:BPm_mean:BPp_mean                    2.518e-03  3.650e-03   0.690  0.49031   
BPs_mean:BPm_mean:BPp_mean                    1.744e-04  9.361e-05   1.863  0.06249 . 
BPd_mean:BPm_mean:BPp_mean                    1.130e-04  1.766e-04   0.640  0.52229   
HRT_mean:BPs_mean:BPd_mean:BPm_mean          -5.244e-07  5.811e-07  -0.902  0.36688   
HRT_mean:BPs_mean:BPd_mean:BPp_mean          -8.585e-07  1.341e-06  -0.640  0.52198   
HRT_mean:BPs_mean:BPm_mean:BPp_mean          -1.893e-06  1.103e-06  -1.715  0.08628 . 
HRT_mean:BPd_mean:BPm_mean:BPp_mean          -1.116e-06  2.100e-06  -0.532  0.59502   
BPs_mean:BPd_mean:BPm_mean:BPp_mean          -1.510e-06  9.429e-07  -1.601  0.10927   
HRT_mean:BPs_mean:BPd_mean:BPm_mean:BPp_mean  1.489e-08  1.060e-08   1.404  0.16026   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4293.2  on 6781  degrees of freedom
Residual deviance: 3494.8  on 6736  degrees of freedom
  (617 observations deleted due to missingness)
AIC: 3586.8

Number of Fisher Scoring iterations: 8

> pred.auc
Error: object 'pred.auc' not found
> perf.auc
An object of class "performance"
Slot "x.name":
[1] "None"

Slot "y.name":
[1] "Area under the ROC curve"

Slot "alpha.name":
[1] "none"

Slot "x.values":
list()

Slot "y.values":
[[1]]
[1] 0.7566197


Slot "alpha.values":
list()

> summary(model.mean.interaction)

Call:
glm(formula = case_label ~ Age * Sex + HRT_spot + BPs_spot + 
    BPd_spot + BPm_spot + HRT_mean * BPs_mean * BPd_mean * BPm_mean * 
    BPp_mean + HRT_sd + BPs_sd + BPd_sd + BPm_sd + BPp_sd + HRT_slope + 
    BPs_slope + BPd_slope + BPm_slope + BPp_slope, family = "binomial")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3023  -0.4821  -0.2644  -0.1200   3.6767  

Coefficients:
                                               Estimate Std. Error z value Pr(>|z|)  
(Intercept)                                  -7.461e+01  6.444e+01  -1.158   0.2469  
Age                                          -1.955e-02  7.776e-03  -2.514   0.0119 *
Sex                                           2.835e-01  3.750e-01   0.756   0.4496  
HRT_spot                                      2.092e-02  1.243e-02   1.683   0.0923 .
BPs_spot                                      1.082e-02  1.240e-02   0.872   0.3831  
BPd_spot                                      8.938e-03  2.136e-02   0.418   0.6756  
BPm_spot                                     -1.795e-02  2.458e-02  -0.730   0.4653  
HRT_mean                                      2.847e-01  7.435e-01   0.383   0.7017  
BPs_mean                                     -1.929e+01  2.725e+01  -0.708   0.4790  
BPd_mean                                      2.202e+01  2.732e+01   0.806   0.4203  
BPm_mean                                     -2.208e-01  1.092e+00  -0.202   0.8398  
BPp_mean                                      2.208e+01  2.735e+01   0.807   0.4196  
HRT_sd                                        1.259e-02  1.697e-02   0.742   0.4579  
BPs_sd                                       -2.040e-02  1.912e-02  -1.067   0.2860  
BPd_sd                                        3.924e-02  2.110e-02   1.859   0.0630 .
BPm_sd                                        3.436e-02  1.491e-02   2.305   0.0212 *
BPp_sd                                        4.540e-02  1.979e-02   2.294   0.0218 *
HRT_slope                                    -8.035e-02  6.010e-02  -1.337   0.1813  
BPs_slope                                    -2.045e-02  5.551e-02  -0.368   0.7126  
BPd_slope                                    -9.795e-02  7.499e-02  -1.306   0.1915  
BPm_slope                                     7.418e-02  5.913e-02   1.255   0.2096  
BPp_slope                                    -7.034e-02  6.171e-02  -1.140   0.2543  
Age:Sex                                       1.004e-02  7.984e-03   1.257   0.2086  
HRT_mean:BPs_mean                             1.753e-01  2.589e-01   0.677   0.4983  
HRT_mean:BPd_mean                            -1.918e-01  2.599e-01  -0.738   0.4605  
BPs_mean:BPd_mean                            -2.081e-02  1.134e-02  -1.835   0.0665 .
HRT_mean:BPm_mean                             5.231e-03  1.309e-02   0.400   0.6894  
BPs_mean:BPm_mean                             2.780e-01  3.880e-01   0.717   0.4736  
BPd_mean:BPm_mean                            -2.826e-01  3.896e-01  -0.725   0.4682  
HRT_mean:BPp_mean                            -1.963e-01  2.604e-01  -0.754   0.4510  
BPs_mean:BPp_mean                            -1.599e-02  6.870e-03  -2.328   0.0199 *
BPd_mean:BPp_mean                            -1.013e-02  1.665e-02  -0.608   0.5430  
BPm_mean:BPp_mean                            -3.045e-01  3.898e-01  -0.781   0.4348  
HRT_mean:BPs_mean:BPd_mean                    1.510e-04  1.306e-04   1.157   0.2473  
HRT_mean:BPs_mean:BPm_mean                   -2.565e-03  3.786e-03  -0.677   0.4982  
HRT_mean:BPd_mean:BPm_mean                    2.538e-03  3.810e-03   0.666   0.5054  
BPs_mean:BPd_mean:BPm_mean                    8.291e-05  5.018e-05   1.652   0.0985 .
HRT_mean:BPs_mean:BPp_mean                    1.357e-04  7.543e-05   1.799   0.0720 .
HRT_mean:BPd_mean:BPp_mean                    1.464e-05  1.969e-04   0.074   0.9407  
BPs_mean:BPd_mean:BPp_mean                    1.459e-04  1.145e-04   1.274   0.2026  
HRT_mean:BPm_mean:BPp_mean                    2.817e-03  3.813e-03   0.739   0.4600  
BPs_mean:BPm_mean:BPp_mean                    1.880e-04  9.502e-05   1.979   0.0478 *
BPd_mean:BPm_mean:BPp_mean                    1.642e-04  1.775e-04   0.925   0.3548  
HRT_mean:BPs_mean:BPd_mean:BPm_mean          -5.045e-07  5.685e-07  -0.887   0.3748  
HRT_mean:BPs_mean:BPd_mean:BPp_mean          -8.736e-07  1.325e-06  -0.659   0.5097  
HRT_mean:BPs_mean:BPm_mean:BPp_mean          -1.982e-06  1.118e-06  -1.773   0.0762 .
HRT_mean:BPd_mean:BPm_mean:BPp_mean          -1.396e-06  2.091e-06  -0.668   0.5043  
BPs_mean:BPd_mean:BPm_mean:BPp_mean          -1.665e-06  9.565e-07  -1.740   0.0818 .
HRT_mean:BPs_mean:BPd_mean:BPm_mean:BPp_mean  1.565e-08  1.066e-08   1.468   0.1422  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4293.2  on 6781  degrees of freedom
Residual deviance: 3455.2  on 6733  degrees of freedom
  (617 observations deleted due to missingness)
AIC: 3553.2

Number of Fisher Scoring iterations: 8

> summary(model.nodemog)

Call:
glm(formula = case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + 
    HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + 
    BPs_sd + BPd_sd + BPm_sd + BPp_sd + HRT_slope + BPs_slope + 
    BPd_slope + BPm_slope + BPp_slope, family = "binomial")

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0779  -0.4909  -0.2757  -0.1173   3.8645  

Coefficients:
                                               Estimate Std. Error z value Pr(>|z|)   
(Intercept)                                  -6.917e+01  6.503e+01  -1.064  0.28744   
HRT_spot                                      2.232e-02  1.238e-02   1.803  0.07140 . 
BPs_spot                                      1.293e-02  1.251e-02   1.034  0.30127   
BPd_spot                                      1.588e-02  2.193e-02   0.724  0.46893   
BPm_spot                                     -2.402e-02  2.516e-02  -0.955  0.33976   
HRT_mean                                      2.193e-01  7.527e-01   0.291  0.77080   
BPs_mean                                     -1.750e+01  2.590e+01  -0.676  0.49922   
BPd_mean                                      2.074e+01  2.598e+01   0.798  0.42476   
BPm_mean                                     -7.851e-01  1.074e+00  -0.731  0.46473   
BPp_mean                                      2.015e+01  2.601e+01   0.775  0.43858   
HRT_sd                                        1.657e-02  1.692e-02   0.980  0.32732   
BPs_sd                                       -2.223e-02  1.907e-02  -1.165  0.24391   
BPd_sd                                        2.985e-02  2.146e-02   1.391  0.16414   
BPm_sd                                        4.677e-02  1.479e-02   3.161  0.00157 **
BPp_sd                                        4.751e-02  1.981e-02   2.398  0.01648 * 
HRT_slope                                    -7.781e-02  5.948e-02  -1.308  0.19078   
BPs_slope                                    -3.308e-02  5.415e-02  -0.611  0.54132   
BPd_slope                                    -1.019e-01  7.498e-02  -1.360  0.17395   
BPm_slope                                     7.973e-02  6.413e-02   1.243  0.21376   
BPp_slope                                    -5.809e-02  5.991e-02  -0.970  0.33224   
HRT_mean:BPs_mean                             1.567e-01  2.472e-01   0.634  0.52608   
HRT_mean:BPd_mean                            -1.777e-01  2.484e-01  -0.716  0.47429   
BPs_mean:BPd_mean                            -2.719e-02  1.150e-02  -2.363  0.01811 * 
HRT_mean:BPm_mean                             1.042e-02  1.286e-02   0.811  0.41762   
BPs_mean:BPm_mean                             2.545e-01  3.698e-01   0.688  0.49118   
BPd_mean:BPm_mean                            -2.531e-01  3.715e-01  -0.681  0.49573   
HRT_mean:BPp_mean                            -1.769e-01  2.488e-01  -0.711  0.47711   
BPs_mean:BPp_mean                            -1.560e-02  6.792e-03  -2.297  0.02161 * 
BPd_mean:BPp_mean                            -3.570e-03  1.648e-02  -0.217  0.82853   
BPm_mean:BPp_mean                            -2.757e-01  3.717e-01  -0.742  0.45828   
HRT_mean:BPs_mean:BPd_mean                    2.059e-04  1.317e-04   1.564  0.11790   
HRT_mean:BPs_mean:BPm_mean                   -2.308e-03  3.622e-03  -0.637  0.52400   
HRT_mean:BPd_mean:BPm_mean                    2.227e-03  3.647e-03   0.611  0.54134   
BPs_mean:BPd_mean:BPm_mean                    8.537e-05  5.092e-05   1.677  0.09361 . 
HRT_mean:BPs_mean:BPp_mean                    1.335e-04  7.474e-05   1.787  0.07402 . 
HRT_mean:BPd_mean:BPp_mean                   -2.585e-05  1.959e-04  -0.132  0.89499   
BPs_mean:BPd_mean:BPp_mean                    1.415e-04  1.158e-04   1.223  0.22144   
HRT_mean:BPm_mean:BPp_mean                    2.518e-03  3.650e-03   0.690  0.49031   
BPs_mean:BPm_mean:BPp_mean                    1.744e-04  9.361e-05   1.863  0.06249 . 
BPd_mean:BPm_mean:BPp_mean                    1.130e-04  1.766e-04   0.640  0.52229   
HRT_mean:BPs_mean:BPd_mean:BPm_mean          -5.244e-07  5.811e-07  -0.902  0.36688   
HRT_mean:BPs_mean:BPd_mean:BPp_mean          -8.585e-07  1.341e-06  -0.640  0.52198   
HRT_mean:BPs_mean:BPm_mean:BPp_mean          -1.893e-06  1.103e-06  -1.715  0.08628 . 
HRT_mean:BPd_mean:BPm_mean:BPp_mean          -1.116e-06  2.100e-06  -0.532  0.59502   
BPs_mean:BPd_mean:BPm_mean:BPp_mean          -1.510e-06  9.429e-07  -1.601  0.10927   
HRT_mean:BPs_mean:BPd_mean:BPm_mean:BPp_mean  1.489e-08  1.060e-08   1.404  0.16026   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4293.2  on 6781  degrees of freedom
Residual deviance: 3494.8  on 6736  degrees of freedom
  (617 observations deleted due to missingness)
AIC: 3586.8

Number of Fisher Scoring iterations: 8

> str(model.nodemog)
List of 31
 $ coefficients     : Named num [1:46] -69.1706 0.0223 0.0129 0.0159 -0.024 ...
  ..- attr(*, "names")= chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ residuals        : Named num [1:6782] 1749.51 3.37 3.96 3.07 13.32 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ fitted.values    : Named num [1:6782] 0.000572 0.29663 0.252837 0.325647 0.075053 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ effects          : Named num [1:6782] 38.534 0.316 -10.636 -8.786 3.249 ...
  ..- attr(*, "names")= chr [1:6782] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ R                : num [1:46, 1:46] -22.7 0 0 0 0 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ rank             : int 46
 $ qr               :List of 5
  ..$ qr   : num [1:6782, 1:46] -22.6595 0.0202 0.0192 0.0207 0.0116 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:6782] "1" "2" "3" "4" ...
  .. .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
  ..$ rank : int 46
  ..$ qraux: num [1:46] 1 1.03 1.01 1 1.01 ...
  ..$ pivot: int [1:46] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ tol  : num 1e-11
  ..- attr(*, "class")= chr "qr"
 $ family           :List of 12
  ..$ family    : chr "binomial"
  ..$ link      : chr "logit"
  ..$ linkfun   :function (mu)  
  ..$ linkinv   :function (eta)  
  ..$ variance  :function (mu)  
  ..$ dev.resids:function (y, mu, wt)  
  ..$ aic       :function (y, n, mu, wt, dev)  
  ..$ mu.eta    :function (eta)  
  ..$ initialize:  expression({     if (NCOL(y) == 1) {         if (is.factor(y))              y <- y != levels(y)[1L]         n <- rep.int(1, nobs)         y[weights == 0] <- 0         if (any(y < 0 | y > 1))              stop("y values must be 0 <= y <= 1")         mustart <- (weights * y + 0.5)/(weights + 1)         m <- weights * y         if (any(abs(m - round(m)) > 0.001))              warning("non-integer #successes in a binomial glm!")     }     else if (NCOL(y) == 2) {         if (any(abs(y - round(y)) > 0.001))              warning("non-integer counts in a binomial glm!")         n <- y[, 1] + y[, 2]         y <- ifelse(n == 0, 0, y[, 1]/n)         weights <- weights * n         mustart <- (n * y + 0.5)/(n + 1)     }     else stop("for the binomial family, y must be a vector of 0 and 1's\n",          "or a 2 column matrix where col 1 is no. successes and col 2 is no. failures") })
  ..$ validmu   :function (mu)  
  ..$ valideta  :function (eta)  
  ..$ simulate  :function (object, nsim)  
  ..- attr(*, "class")= chr "family"
 $ linear.predictors: Named num [1:6782] -7.467 -0.863 -1.084 -0.728 -2.512 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ deviance         : num 3495
 $ aic              : num 3587
 $ null.deviance    : num 4293
 $ iter             : int 8
 $ weights          : Named num [1:6782] 0.000571 0.208641 0.18891 0.219601 0.06942 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ prior.weights    : Named num [1:6782] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ df.residual      : int 6736
 $ df.null          : int 6781
 $ y                : Named num [1:6782] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ converged        : logi TRUE
 $ boundary         : logi FALSE
 $ model            :'data.frame':	6782 obs. of  20 variables:
  ..$ case_label: int [1:6782] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ HRT_spot  : int [1:6782] 51 57 55 55 52 134 137 137 138 129 ...
  ..$ BPs_spot  : int [1:6782] 152 116 117 117 121 168 184 122 114 120 ...
  ..$ BPd_spot  : int [1:6782] 86 54 53 52 60 81 89 54 47 51 ...
  ..$ BPm_spot  : int [1:6782] 112 73 73 71 81 107 114 81 74 77 ...
  ..$ HRT_mean  : num [1:6782] 50.8 56.4 55 55 51.6 ...
  ..$ BPs_mean  : num [1:6782] 153 116 118 117 123 ...
  ..$ BPd_mean  : num [1:6782] 87.2 53.4 53.6 52 61.4 82 89 50.6 45.8 47.8 ...
  ..$ BPm_mean  : num [1:6782] 112.6 72.2 73.2 71 82.8 ...
  ..$ BPp_mean  : num [1:6782] 65.4 62.6 64.4 65.4 61.8 89 96.4 66.8 64.4 66.6 ...
  ..$ HRT_sd    : num [1:6782] 0.447 0.548 0 0 0.548 ...
  ..$ BPs_sd    : num [1:6782] 0.894 0.707 0.707 0.548 1.789 ...
  ..$ BPd_sd    : num [1:6782] 1.304 0.548 0.548 0 1.342 ...
  ..$ BPm_sd    : num [1:6782] 0.894 0.447 0.447 0 1.789 ...
  ..$ BPp_sd    : num [1:6782] 0.548 0.894 0.548 0.548 0.837 ...
  ..$ HRT_slope : num [1:6782] 0.229 0 0 0 0.257 ...
  ..$ BPs_slope : num [1:6782] -0.486 0 0 0 -0.743 ...
  ..$ BPd_slope : num [1:6782] -0.857 0 0 0 -0.543 ...
  ..$ BPm_slope : num [1:6782] -0.629 0 0 0 -0.8 ...
  ..$ BPp_slope : num [1:6782] 0.371 0 0 0 0 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + HRT_mean *      BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + BPs_sd +  ...
  .. .. ..- attr(*, "variables")= language list(case_label, HRT_spot, BPs_spot, BPd_spot, BPm_spot, HRT_mean,      BPs_mean, BPd_mean, BPm_mean, BPp_mean, HRT_sd, BPs_sd, BPd_sd,  ...
  .. .. ..- attr(*, "factors")= int [1:20, 1:45] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:20] "case_label" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. .. .. .. ..$ : chr [1:45] "HRT_spot" "BPs_spot" "BPd_spot" "BPm_spot" ...
  .. .. ..- attr(*, "term.labels")= chr [1:45] "HRT_spot" "BPs_spot" "BPd_spot" "BPm_spot" ...
  .. .. ..- attr(*, "order")= int [1:45] 1 1 1 1 1 1 1 1 1 1 ...
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(case_label, HRT_spot, BPs_spot, BPd_spot, BPm_spot, HRT_mean,      BPs_mean, BPd_mean, BPm_mean, BPp_mean, HRT_sd, BPs_sd, BPd_sd,  ...
  .. .. ..- attr(*, "dataClasses")= Named chr [1:20] "numeric" "numeric" "numeric" "numeric" ...
  .. .. .. ..- attr(*, "names")= chr [1:20] "case_label" "HRT_spot" "BPs_spot" "BPd_spot" ...
  ..- attr(*, "na.action")=Class 'omit'  Named int [1:617] 19 37 39 84 85 86 87 88 89 90 ...
  .. .. ..- attr(*, "names")= chr [1:617] "19" "37" "39" "84" ...
 $ na.action        :Class 'omit'  Named int [1:617] 19 37 39 84 85 86 87 88 89 90 ...
  .. ..- attr(*, "names")= chr [1:617] "19" "37" "39" "84" ...
 $ call             : language glm(formula = case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot +      HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd +  ...
 $ formula          :Class 'formula' length 3 case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + HRT_mean *      BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + BPs_sd +  ...
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
 $ terms            :Classes 'terms', 'formula' length 3 case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + HRT_mean *      BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + BPs_sd +  ...
  .. ..- attr(*, "variables")= language list(case_label, HRT_spot, BPs_spot, BPd_spot, BPm_spot, HRT_mean,      BPs_mean, BPd_mean, BPm_mean, BPp_mean, HRT_sd, BPs_sd, BPd_sd,  ...
  .. ..- attr(*, "factors")= int [1:20, 1:45] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:20] "case_label" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. .. .. ..$ : chr [1:45] "HRT_spot" "BPs_spot" "BPd_spot" "BPm_spot" ...
  .. ..- attr(*, "term.labels")= chr [1:45] "HRT_spot" "BPs_spot" "BPd_spot" "BPm_spot" ...
  .. ..- attr(*, "order")= int [1:45] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(case_label, HRT_spot, BPs_spot, BPd_spot, BPm_spot, HRT_mean,      BPs_mean, BPd_mean, BPm_mean, BPp_mean, HRT_sd, BPs_sd, BPd_sd,  ...
  .. ..- attr(*, "dataClasses")= Named chr [1:20] "numeric" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:20] "case_label" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ data             :<environment: R_GlobalEnv> 
 $ offset           : NULL
 $ control          :List of 3
  ..$ epsilon: num 1e-08
  ..$ maxit  : num 25
  ..$ trace  : logi FALSE
 $ method           : chr "glm.fit"
 $ contrasts        : NULL
 $ xlevels          : Named list()
 - attr(*, "class")= chr [1:2] "glm" "lm"
> model.nodemog$family

Family: binomial 
Link function: logit 

> model.nodemog$family$aic
function (y, n, mu, wt, dev) 
{
    m <- if (any(n > 1)) 
        n
    else wt
    -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * y), 
        round(m), mu, log = TRUE))
}
<bytecode: 0x1164a5430>
<environment: 0x116715158>
> model.nodemog

Call:  glm(formula = case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + 
    HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + 
    BPs_sd + BPd_sd + BPm_sd + BPp_sd + HRT_slope + BPs_slope + 
    BPd_slope + BPm_slope + BPp_slope, family = "binomial")

Coefficients:
                                 (Intercept)                                      HRT_spot  
                                  -6.917e+01                                     2.232e-02  
                                    BPs_spot                                      BPd_spot  
                                   1.293e-02                                     1.588e-02  
                                    BPm_spot                                      HRT_mean  
                                  -2.402e-02                                     2.193e-01  
                                    BPs_mean                                      BPd_mean  
                                  -1.750e+01                                     2.074e+01  
                                    BPm_mean                                      BPp_mean  
                                  -7.851e-01                                     2.015e+01  
                                      HRT_sd                                        BPs_sd  
                                   1.657e-02                                    -2.223e-02  
                                      BPd_sd                                        BPm_sd  
                                   2.985e-02                                     4.677e-02  
                                      BPp_sd                                     HRT_slope  
                                   4.751e-02                                    -7.781e-02  
                                   BPs_slope                                     BPd_slope  
                                  -3.308e-02                                    -1.019e-01  
                                   BPm_slope                                     BPp_slope  
                                   7.973e-02                                    -5.809e-02  
                           HRT_mean:BPs_mean                             HRT_mean:BPd_mean  
                                   1.567e-01                                    -1.777e-01  
                           BPs_mean:BPd_mean                             HRT_mean:BPm_mean  
                                  -2.719e-02                                     1.042e-02  
                           BPs_mean:BPm_mean                             BPd_mean:BPm_mean  
                                   2.545e-01                                    -2.531e-01  
                           HRT_mean:BPp_mean                             BPs_mean:BPp_mean  
                                  -1.769e-01                                    -1.560e-02  
                           BPd_mean:BPp_mean                             BPm_mean:BPp_mean  
                                  -3.570e-03                                    -2.757e-01  
                  HRT_mean:BPs_mean:BPd_mean                    HRT_mean:BPs_mean:BPm_mean  
                                   2.059e-04                                    -2.308e-03  
                  HRT_mean:BPd_mean:BPm_mean                    BPs_mean:BPd_mean:BPm_mean  
                                   2.227e-03                                     8.537e-05  
                  HRT_mean:BPs_mean:BPp_mean                    HRT_mean:BPd_mean:BPp_mean  
                                   1.335e-04                                    -2.585e-05  
                  BPs_mean:BPd_mean:BPp_mean                    HRT_mean:BPm_mean:BPp_mean  
                                   1.415e-04                                     2.518e-03  
                  BPs_mean:BPm_mean:BPp_mean                    BPd_mean:BPm_mean:BPp_mean  
                                   1.744e-04                                     1.130e-04  
         HRT_mean:BPs_mean:BPd_mean:BPm_mean           HRT_mean:BPs_mean:BPd_mean:BPp_mean  
                                  -5.243e-07                                    -8.585e-07  
         HRT_mean:BPs_mean:BPm_mean:BPp_mean           HRT_mean:BPd_mean:BPm_mean:BPp_mean  
                                  -1.893e-06                                    -1.116e-06  
         BPs_mean:BPd_mean:BPm_mean:BPp_mean  HRT_mean:BPs_mean:BPd_mean:BPm_mean:BPp_mean  
                                  -1.510e-06                                     1.489e-08  

Degrees of Freedom: 6781 Total (i.e. Null);  6736 Residual
  (617 observations deleted due to missingness)
Null Deviance:	    4293 
Residual Deviance: 3495 	AIC: 3587 
> str(summary(model.nodemog))
List of 18
 $ call          : language glm(formula = case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot +      HRT_mean * BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd +  ...
 $ terms         :Classes 'terms', 'formula' length 3 case_label ~ HRT_spot + BPs_spot + BPd_spot + BPm_spot + HRT_mean *      BPs_mean * BPd_mean * BPm_mean * BPp_mean + HRT_sd + BPs_sd +  ...
  .. ..- attr(*, "variables")= language list(case_label, HRT_spot, BPs_spot, BPd_spot, BPm_spot, HRT_mean,      BPs_mean, BPd_mean, BPm_mean, BPp_mean, HRT_sd, BPs_sd, BPd_sd,  ...
  .. ..- attr(*, "factors")= int [1:20, 1:45] 0 1 0 0 0 0 0 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:20] "case_label" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. .. .. ..$ : chr [1:45] "HRT_spot" "BPs_spot" "BPd_spot" "BPm_spot" ...
  .. ..- attr(*, "term.labels")= chr [1:45] "HRT_spot" "BPs_spot" "BPd_spot" "BPm_spot" ...
  .. ..- attr(*, "order")= int [1:45] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(case_label, HRT_spot, BPs_spot, BPd_spot, BPm_spot, HRT_mean,      BPs_mean, BPd_mean, BPm_mean, BPp_mean, HRT_sd, BPs_sd, BPd_sd,  ...
  .. ..- attr(*, "dataClasses")= Named chr [1:20] "numeric" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:20] "case_label" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ family        :List of 12
  ..$ family    : chr "binomial"
  ..$ link      : chr "logit"
  ..$ linkfun   :function (mu)  
  ..$ linkinv   :function (eta)  
  ..$ variance  :function (mu)  
  ..$ dev.resids:function (y, mu, wt)  
  ..$ aic       :function (y, n, mu, wt, dev)  
  ..$ mu.eta    :function (eta)  
  ..$ initialize:  expression({     if (NCOL(y) == 1) {         if (is.factor(y))              y <- y != levels(y)[1L]         n <- rep.int(1, nobs)         y[weights == 0] <- 0         if (any(y < 0 | y > 1))              stop("y values must be 0 <= y <= 1")         mustart <- (weights * y + 0.5)/(weights + 1)         m <- weights * y         if (any(abs(m - round(m)) > 0.001))              warning("non-integer #successes in a binomial glm!")     }     else if (NCOL(y) == 2) {         if (any(abs(y - round(y)) > 0.001))              warning("non-integer counts in a binomial glm!")         n <- y[, 1] + y[, 2]         y <- ifelse(n == 0, 0, y[, 1]/n)         weights <- weights * n         mustart <- (n * y + 0.5)/(n + 1)     }     else stop("for the binomial family, y must be a vector of 0 and 1's\n",          "or a 2 column matrix where col 1 is no. successes and col 2 is no. failures") })
  ..$ validmu   :function (mu)  
  ..$ valideta  :function (eta)  
  ..$ simulate  :function (object, nsim)  
  ..- attr(*, "class")= chr "family"
 $ deviance      : num 3495
 $ aic           : num 3587
 $ contrasts     : NULL
 $ df.residual   : int 6736
 $ null.deviance : num 4293
 $ df.null       : int 6781
 $ iter          : int 8
 $ na.action     :Class 'omit'  Named int [1:617] 19 37 39 84 85 86 87 88 89 90 ...
  .. ..- attr(*, "names")= chr [1:617] "19" "37" "39" "84" ...
 $ deviance.resid: Named num [1:6782] 3.86 1.56 1.66 1.5 2.28 ...
  ..- attr(*, "names")= chr [1:6782] "1" "2" "3" "4" ...
 $ coefficients  : num [1:46, 1:4] -69.1706 0.0223 0.0129 0.0159 -0.024 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. ..$ : chr [1:4] "Estimate" "Std. Error" "z value" "Pr(>|z|)"
 $ aliased       : Named logi [1:46] FALSE FALSE FALSE FALSE FALSE FALSE ...
  ..- attr(*, "names")= chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ dispersion    : num 1
 $ df            : int [1:3] 46 6736 46
 $ cov.unscaled  : num [1:46, 1:46] 4.23e+03 4.29e-03 -5.20e-02 -4.50e-03 7.59e-02 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
 $ cov.scaled    : num [1:46, 1:46] 4.23e+03 4.29e-03 -5.20e-02 -4.50e-03 7.59e-02 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
  .. ..$ : chr [1:46] "(Intercept)" "HRT_spot" "BPs_spot" "BPd_spot" ...
 - attr(*, "class")= chr "summary.glm"
> summary(model.nodemog)$aic
[1] 3586.768
> summary(model.mean.interaction)$aic
[1] 3553.207
> fred<-summary(model.mean.interaction)$aic
> fred
[1] 3553.207
> cat(sep='','AIC Value = ', fred , '\n')
AIC Value = 3553.207