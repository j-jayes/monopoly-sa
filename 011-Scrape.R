#' ---
#' title: "011-Scrape"
#' author: "JJayes"
#' date: "26/01/2022"
#' output: html_document
#' ---
#' 
## --------------------------------------------------------------------------------------------------------------
# knitr::purl("code/011-Scrape.Rmd", documentation = 2)

library(tidyverse)
library(rvest)

source("code/fns/get_property_info.R")

tbl_of_links <- read_rds("data/links/list_of_links_complete.rds")

results <- tbl_of_links %>% 
  head(20) %>% 
  mutate(property_link = paste0("https://www.privateproperty.co.za", property_link)) %>%
  mutate(result = map(property_link, possibly(get_property_info, "failed")))

st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")

results %>% 
  write_rds(paste0("data/results/results_test", st, ".rds"), compress = "gz")

#' 
#' Chang the head(20) and filename_test
