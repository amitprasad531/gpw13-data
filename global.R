library(shiny)
library(dplyr)
library(DT)
library(markdown)

gpw <- read.csv("data/gpw.csv", header = TRUE)
source("www/gpwvectors.R")
source("www/metadata_data.R")

for (i in 1:length(gpw_ind)) {
  names(gpw)[names(gpw) == gpw_ind[i]] <- gpw_colnames[i]
}
