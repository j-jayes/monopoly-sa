# stick for list of pages
stick <- function(b_url, n_pages){

  str_c(b_url,
        "&page=",
        1:n_pages)

}
