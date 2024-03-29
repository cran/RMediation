RMediation 1.2.2 (5/11/2023)
==============
* Fixed an issue where the type="all" in medci function would generate an error "object 'MeekerCI' not found".

* changed the dependency version for grDevices to >= 3.5 in the description file.

RMediation 1.2.1 (4/28/2023)
==============
* Fixed issues to meet CRAN submission requirements.

RMediation 1.2.0 (6/28/2022)
==============
*New parametric and non-parametric MBCO test, using `mbco` function.
* Fixed an error with printing medci, '(1-alpha)% CI'

RMediation 1.1.5 (6/7/2020)
==============
* Fixed the issue with 'medic' function that printed '(1-alpha/2)% CI' instead of '(1-alpha)% CI'

RMediation 1.1.4 (3/12/2016)
=============
* 'medci' function output structure is changed. For each 'type', it produces a 'list' of '(1-alpha/2)% CI', 'Estimate', and 'SE'. See help(ci) for more information.

* Two functions 'pMC' and 'qMC' are added.

* for medci(), type=‘prodclin’ is replaced with ‘dop’.

RMediation 1.1.3
=============
* New 'ci' function produces CIs for any well-defined function of parameter estimates in an SEM and MSEM using the Monte Carlo or asymptotic normal theory with the multivariate delta standard error method.
