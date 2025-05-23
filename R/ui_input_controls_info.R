



.DE.meta = list(
  m = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "m",
      label = "Target number of respondents per cluster",
      value = "5, 15"
    )
  )
  ,
  icc = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "icc",
      label = "Intracluster correlation coefficient",
      value = "1/22, 1/6"
    )
  )
  ,
  cv = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "cv",
      label = "Coefficient of variation of sample weights",
      value = "0.50"
    )
  )
)









.INF.meta = list(
  r = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "r",
      label = "Anticipated non-response rate, from 0 to 1",
      value = "0.15"
    )
  )
)









.nOutTab.meta = list(
  d = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "d",
      label = "Desired half-width CI",
      value = "0.05, 0.10"
    )
  )
  ,
  p = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "p",
      label = "Expected coverage proportion",
      value = "0.10, 0.25"
    )
  )
  ,
  alpha = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "alpha",
      label = "Type I error rate",
      choices = c("0.01","0.025","0.05","0.10"),
      selected = "0.05",
      multiple = TRUE
    )
  )
)
.nOutTab.meta = c(.nOutTab.meta,.DE.meta,.INF.meta)








.dOutput.meta = list(
  n = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "n",
      label = "Sample size",
      value = "300, 900"
    )
  )
  ,
  p = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "p",
      label = "Expected coverage proportion",
      value = "0.10, 0.25"
    )
  )
  ,
  alpha = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "alpha",
      label = "Type I error rate",
      choices = c("0.01","0.025","0.05","0.10"),
      selected = "0.05",
      multiple = TRUE
    )
  )
)
.dOutput.meta = c(.dOutput.meta,.DE.meta,.INF.meta)







.nclOutTab.meta = list(
  P0 = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "P0",
      label = "Programmatic threshold",
      value = "0.7, 0.8"
    )
  )
  ,
  delta = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "delta",
      label = "A coverage percent defining a distance from P0", 
      value = "0.01, 0.05"
    )
  )
  ,
  direction = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "direction",
      label = "Direction",
      choices = c("below","above"),
      selected = "below",
      multiple = FALSE
    )
  )
  ,
  alpha = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "alpha",
      label = "Type I error rate",
      choices = c("0.01","0.025","0.05","0.10"),
      selected = c("0.01","0.05"),
      multiple = TRUE
    )
  )
  ,
  beta = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "beta",
      label = "Type II error rate",
      choices = c("0.10","0.20"),
      selected = c("0.10","0.20"),
      multiple = TRUE
    )
  )
)
.nclOutTab.meta = c(.nclOutTab.meta,.DE.meta,.INF.meta)








.ESS_2Grp_2Sided.meta = list(
  P1 = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "P1",
      label = "Estimated coverage level from one of the two surveys",
      value = "0.5"
    )
  )
  ,
  Delta = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "Delta",
      label = "difference above P1 form which the survey should be well powered to reject the null hypothesis", 
      value = "0.10"
    )
  )
  ,
  SS_ratio = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "SS_ratio",
      label = "Sample size ratio between two groups n2:n1",
      value = "1.1"
    )
  )
  ,
  Alpha = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "Alpha",
      label = "Type I error rate",
      choices = c("0.01","0.025","0.05","0.10"),
      selected = c("0.10"),
      multiple = TRUE
    )
  )
  ,
  Beta = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "Beta",
      label = "Type II error rate",
      choices = c("0.10","0.20"),
      selected = c("0.20"),
      multiple = TRUE
    )
  )
)
.ESS_2Grp_2Sided.meta = c(.ESS_2Grp_2Sided.meta,.DE.meta,.INF.meta)









.ESS_1Grp_1Sided.meta = list(
  P1 = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "P1",
      label = "Coverage for the previously conducted survey",
      value = "0.7"
    )
  )
  ,
  P2 = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "P2",
      label = "Coverage for the planned survey",
      value = "0.8"
    )
  )
  ,
  ESS1 = list(
    input_control_type = "textInput",
    input_control_args = list(
      inputId = "ESS1",
      label = "Effective sample size from early conducted survey",
      value = "174"
    )
  )
  ,
  Alpha = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "Alpha",
      label = "Type I error rate",
      choices = c("0.01","0.025","0.05","0.10"),
      selected = c("0.05"),
      multiple = TRUE
    )
  )
  ,
  Beta = list(
    input_control_type = "selectInput",
    input_control_args = list(
      inputId = "Beta",
      label = "Type II error rate",
      choices = c("0.10","0.20"),
      selected = c("0.20"),
      multiple = TRUE
    )
  )
)
#.ESS_1Grp_1Sided.meta = c(.ESS_1Grp_1Sided.meta,.DE.meta,.INF.meta)








