# function to get number of pages
get_n_pages <- function(b_url) {
  message(paste0("Getting n_pages from ", b_url))
  html <- read_html(b_url)

  n_pages <- html %>%
    # get properties and divide by number of results per page (24)
    html_nodes(".listingCount") %>%
    html_text(trim = T) %>%
    str_extract(., "of.*") %>%
    parse_number() %>%
    as_tibble() %>%
    mutate(value = ceiling(value / 24)) %>%
    pull(value)
}
