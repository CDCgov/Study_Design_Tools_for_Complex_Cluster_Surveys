



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