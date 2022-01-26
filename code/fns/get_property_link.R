# get property links
get_property_link <- function(result_page) {
  message(paste0("Getting links from ", result_page))

  html <- read_html(result_page)

  html %>%
    html_nodes(".listingResult") %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(property_link = value)
}
