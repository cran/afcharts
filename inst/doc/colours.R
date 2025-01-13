## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = TRUE,
  echo = FALSE
)

table_note <- paste(
  "Note: the cells in the ‘Example of colour’ column may appear blank",
  "to screen reader software. They contain a colour fill, but no data."
)

## -----------------------------------------------------------------------------
afcharts:::colour_table(afcharts::af_colour_palettes$main)

## -----------------------------------------------------------------------------
afcharts:::colour_table(afcharts::af_colour_palettes$main2)

## -----------------------------------------------------------------------------
afcharts:::colour_table(afcharts::af_colour_palettes$main6)

## -----------------------------------------------------------------------------
afcharts:::colour_table(afcharts::af_colour_palettes$sequential)

## -----------------------------------------------------------------------------
afcharts:::colour_table(afcharts::af_colour_palettes$focus)

## ----view-colours, eval = FALSE, echo = TRUE----------------------------------
# # View names and hex codes for all colours
# afcharts::af_colour_values
# 
# # View names and hex codes for all colour palettes
# afcharts::af_colour_palettes
# 
# # View names and hex codes for `main` colour palette
# afcharts::af_colour_palettes$main

