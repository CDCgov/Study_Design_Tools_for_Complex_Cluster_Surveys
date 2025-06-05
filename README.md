# Cluster Survey Sample Size

The web app version can be found here: https://cdcgov.github.io/Study_Design_Tools_for_Complex_Cluster_Surveys/
 
 Code to compute sample size and related parameters following the formulas of the WHO Vaccination Coverage Cluster Surveys Reference Manual.

 Originally done in R. To be done also in python. 

- [x] Initial R code from project(s)
- [x] Create initial documentation in Readme file, with math
- [x] Generalize R code to not be project-specific
- [x] R
  - [x] Create standard output format
  - [x] Create an initial R Shiny App
- [ ] Python
  - [x] Initial Python code, converted from R code
  - [x] Create standard output format
  - [ ] Create an initial Shiny for Phython App


 Throughout, page numbers refer to the first reference below. 

 ## Versions
 * 0.01
   * Estimation only, i.e. no code for classification or comparison.
   * Initial R code with project-specific code removed.
   * Python code for the computation functions. 
* 0.02
  * Estimation only
  * R & python code that both have:
    * Computation functions
    * Validation of computation functions
    * Output functions for sample size and CI
    * Example usage, including a CI figure plot
    * Modularized code

 ## References
* Vaccination Coverage Cluster Surveys: Reference Manual. Geneva: World Health Organization; 2018 (WHO/IVB/18.09). Licence: CC BY-NC-SA 3.0 IGO. Ordering code: WHO/IVB/18.09. https://www.who.int/publications/i/item/WHO-IVB-18.09


## Project Features
* R & python code to calculate sample size, confidence intervals, and potentially other parameters related to the design of "Estimation", "Classification", and "Comparison" cluster surveys as described in the WHO manual. 
* Shiny web-apps to implement project functionality; to be published on CDC's public GitHub page.


## Branch Structure

![branch structure image](img/Git_Branches.png "Branch Structure"){width=500 height=500px}


# Estimation 

## Math Formulas
* Effective Sample Size, $`n_{ESS} (p, d, \alpha)`$, p.129
* Design Effect, $`F_{DE}(m,icc,cv)`$, p.110
* Inflation Factor, $`F_{Inflation}(r)`$, p.112, 128
* Confidence Interval, $`CI(n,m,icc,cv,r,p,\alpha)`$
* Sample Size, $`n(n_{ESS}, F_{DE}, F_{Inflation})`$
* Number of Clusters, $`n_{Clust}(n_{ESS}, m)`$

where:  
* $`p`$ is expected coverage proportion
* $`d`$ is desired half-width of the confidence interval (CI)
* $`\alpha`$ is the probability value used to define the precision for estimated CIs; usually 0.05 for 95% CI
* $`m`$ is target number of respondents per cluster, p.109
* $`icc`$ is intracluster correlation coefficient, p.109
* $`cv`$ is the coefficient of variation of the survey weights, p.109
* $`r`$ anticipated non-response rate, from 0 to 1, p.112, 128

### Formulas
Effective Sample Size (ESS) $`n_{ESS}`$:
```math
n_{ESS} \ge \frac{k z^2_{1-\alpha/2}}{4d^2} + \frac{1}{d} - 
2z^2_{1-\alpha/2} + \frac{z^2_{1-\alpha/2}+2}{k}
```
where  
* $`z^2_{1-x}`$ is the standard normal distribution evaluated at $`1-x`$

and $`k`$ is found from the following table:

| If $`p`$ satisfies | Then use |
| ---                | ---      |
| $`0 \le p \lt d`$     | $`k = 8d(1-2d)`$ |
| $`d \le p \lt 0.3`$   | $`k = 4(p+d)(1-p-d)`$ |
| $`0.3 \le p \le 0.7`$ | $`k = 1`$ |
| $`0.7 \lt p \le 1-d`$ | $`k = 4(p-d)(1-p+d)`$ |
| $`1-d \lt p \le 1`$   | $`k = 8d(1-2d)`$ |

Design Effect, $`F_{DE}`$:
```math
F_{DE} = [(1+icc(m-1)][1+cv^2]
```

Inflation factor due to non-response $`F_{Inflation}`$:
```math
F_{Inflation} = \frac{r}{1-r}
```

Confidence Interval (half-width):
```math
d = \min_{d\in(0,1)}\begin{vmatrix}   n_{ESS} - \frac{k z^2_{1-\alpha/2}}{4d^2} - \frac{1}{d} + 2z^2_{1-\alpha/2} - \frac{z^2_{1-\alpha/2}+2}{k} \\\end{vmatrix}
```  

Sample Size, $`n`$:
```math
n = Ceiling\lparen n_{ESS} \times F_{DE} \times F_{Inflation} \rparen
```

Number of clusters, $`n_{Clust}`$:
```math
n_{Clust} = Ceiling\begin{pmatrix}   \frac{n}{m} \\\end{pmatrix}
```


