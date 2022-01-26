# get property info
get_property_info <- function(url) {
  message(paste0("Getting property info from ", url))
  html <- read_html(url)

  # price
  price <- html %>%
    html_nodes(".detailsPrice") %>%
    html_text() %>%
    as_tibble() %>%
    head(1) %>%
    pull() %>%
    str_remove_all(., " ") %>%
    parse_number()

  # address
  address <- html %>%
    html_nodes(".address") %>%
    html_text()

  # bed bath garage
  result_features <- html %>%
    html_nodes(".resultFeatures") %>%
    html_text() %>%
    as_tibble() %>%
    separate_rows(value, sep = "\\r") %>%
    mutate(value = str_squish(value)) %>%
    filter(nchar(value) > 0) %>%
    rename(key = value) %>%
    mutate(
      value = parse_number(key),
      key = str_remove(key, "[0-9].")
    ) %>%
    nest(result_feature = everything())

  # mainFeatures
  main_features <- html %>%
    html_nodes(".mainFeatures") %>%
    html_text() %>%
    as_tibble() %>%
    separate_rows(value, sep = "\\r") %>%
    mutate(value = str_squish(value)) %>%
    filter(nchar(value) > 0) %>%
    rename(key = value) %>%
    mutate(
      value = parse_number(key),
      key = str_squish(str_remove(key, "[0-9].*"))
    ) %>%
    nest(main_feature = everything())

  attributes <- html %>%
    html_nodes(".attributeLabel") %>%
    html_text() %>%
    as_tibble() %>%
    rename(key = value) %>%
    bind_cols(html %>%
                html_nodes(".propAttrValue") %>%
                html_text() %>%
                as_tibble()) %>%
    nest(attribute = everything())

  # still to work on this.
  description <- html %>%
    html_nodes(".description") %>%
    html_text() %>%
    str_squish()

  # agency
  agency <- html %>%
    html_nodes(".agencyName") %>%
    html_text() %>%
    str_squish()

  tibble(price, address, result_features, main_features, attributes, description, agency)
}
