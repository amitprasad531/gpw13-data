library(shiny)
library(dplyr)
library(DT)
library(markdown)
library(shinyWidgets)

gpw <- read.csv("data/gpw.csv", header = TRUE)
source("www/gpwvectors.R")
source("www/metadata_data.R")

for (i in 1:length(gpw_ind)) {
  names(gpw)[names(gpw) == gpw_ind[i]] <- gpw_colnames[i]
}

map_latest <- function(a, b, yr = "None", value = NA) {
  for (i in 1:length(a)) {
    ifelse(is.na(b[i]), value, value <- b[i])
    if (!is.na(value)) {
      yr <- a[i]
      return(c(yr, value))
    }
  }
  return(c(yr, value))
}