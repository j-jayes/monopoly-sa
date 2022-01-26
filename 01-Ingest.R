#' ---
#' title: "Ingest"
#' author: "JJayes"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output: html_document
#' ---
#' To get code out:
## ------------------------------------------------------------------------------------------------------------------------------
# knitr::purl("code/01-Ingest.Rmd", documentation = 2)

#' 
#' # Purpose
#' 
#' Scrape Private Property for property adverts that are from the 22 places on the monopoly board.
#' 
## ------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rvest)

source("code/fns/get_n_pages.R")
source("code/fns/stick.R")
source("code/fns/get_property_link.R")
source("code/fns/get_property_info.R")

#' 
#' ## Process
#' 
#' 5 steps:
#' 
#' 1. Get the links to the search pages on Private Property by place.
#' 
#' 2. Get number of pages of results per place
#' 
#' 3. Create a list of links to get property advert URLs from
#' 
#' 4. Get the property ad URLs
#' 
#' 5. Then scrape the pages to get the information we want.
#' 
#' Get links to base searches for each monopoly board region - in the file below.
#' 
#' ## 1. Get the links to the search pages on Private Property by place.
#' 
#' Done manually: saved as rds file.
#' 
## ------------------------------------------------------------------------------------------------------------------------------
df <- read_rds("data/board-stats.rds")

#' 
#' ## 2. Get number of pages of results per place
#' 
#' Make list of links from source file:
#' 
## ------------------------------------------------------------------------------------------------------------------------------
tbl_of_pages <- df %>% 
  # filters here exclude land and farms
  select(Property, base_url, filters) %>% 
  mutate(base_url = str_c(base_url, filters)) %>% 
  select(-filters)

#' 
#' `get_n_pages` is a function written to find number of pages by looking at number of ads, dividing by per page, and rounding up.
#' 
## ------------------------------------------------------------------------------------------------------------------------------
tbl_of_pages <- tbl_of_pages %>% 
  mutate(n_pages = map(base_url, possibly(get_n_pages, 0)))

tbl_of_pages <- tbl_of_pages %>%
  unnest(n_pages)

#' 
#' ## 3. Create a list of links to get property advert URLs from
#' 
#' `stick` is a function that makes pages 1 through number of pages in a tibble and then we unnest it.
#' 
## ------------------------------------------------------------------------------------------------------------------------------
tbl_of_pages <- tbl_of_pages %>%
  # pmap gang!
  mutate(pages = pmap(list(base_url, n_pages), stick)) %>%
  unnest(pages)

#' 
#' 
#' ## 4. Get the property ad URLs
#' 
#' `get_property_link` is a function that scrapes the search results page for each url for the property advert.
#' 
## ------------------------------------------------------------------------------------------------------------------------------
tbl_of_links <- tbl_of_pages %>%
  head(5) %>%
  mutate(propery_links = map(pages, possibly(get_property_link, "failed")))

st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")

tbl_of_links %>% 
  unnest(propery_links) %>% 
  write_rds(paste0("data/links/list_of_links", st, ".rds"), compress = "gz")

#' 
#' ## 5. Then scrape the pages to get the information we want.
#' 
#' Get details from property listing using `get_property_info`
#' 
#' Info:
#' 
#' * Price
#' 
#' * Address
#' 
#' * Floor area and rates and levy - can try to get from text
#' 
#' * Text description
#' 
#' * Key property features
#' 
#' * Agency
#' 
#' Using function through list
#' 
## ------------------------------------------------------------------------------------------------------------------------------
tbl_of_links <- tbl_of_links %>% 
  unnest(propery_links)

results <- tbl_of_links %>% 
  mutate(property_link = str_c("https://www.privateproperty.co.za", property_link)) %>% 
  mutate(result = map(property_link, possibly(get_property_info, "failed")))

st <- format(Sys.time(), "%Y-%m-%d-%I-%M-%p")

results %>% 
  write_rds(paste0("data/results/results", st, ".rds"), compress = "gz")


#' 
