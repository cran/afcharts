## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----standalone-svg, eval = FALSE---------------------------------------------
# ggsave(filename = "example-plot.svg", plot = myplot,
#        width = 159, height = 100, units = "mm")

## ----standalong-png, eval = FALSE---------------------------------------------
# ggsave(filename = "example-plot.png", plot = myplot,
#        width = 159, height = 100, units = "mm", dpi = "retina")

## ----savegovuk, eval = FALSE--------------------------------------------------
# save_govuk(filename = "example-plot.png", plot = myplot, device = "svg")

