# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
#library(rvest)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)
#library(janitor)

read_hraun <- function(year = 1995:2018) {

  res <- list()
  for(y in 1:length(year)) {
    res[[y]] <-
      paste0("http://hraun.vedur.is/ja/viku/", year[y], "/") %>%
      xml2::read_html() %>%
      rvest::html_node("table") %>%
      rvest::html_table() %>%
      janitor::clean_names() %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_sub(name, 1, 4) %in% "vika",
                    stringr::str_sub(name, 8, 8) == "/") %>%
      dplyr::select(name, last_modified) %>%
      dplyr::mutate(last_modified = lubridate::dmy_hm(last_modified),
                    url = paste0("http://hraun.vedur.is/ja/viku/", year[y], "/", name, "listi"))
  }


  names(res) <- year

  fls <-
    dplyr::bind_rows(res, .id = "year") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::select(year, url)

  res <- list()
  for(i in 1:nrow(fls)) {
    res[[i]] <-
      read.table(fls$url[i],
                 header=TRUE,colClasses=c("integer",rep("character",2),rep("numeric",5))) %>%
      tibble::as_tibble()
  }

  res <-
    res %>%
    dplyr::bind_rows() %>%
    dplyr::rename(id = Nr,
                  date = Dags.,
                  time = Timi,
                  lat = Breidd,
                  lon = Lengd,
                  z = Dypi,
                  size = M,
                  size2 = ML) %>%
    dplyr::mutate(time = lubridate::ymd_hms(paste(date, time, "UTC")),
                  date = lubridate::ymd(date),
                  year = lubridate::year(date))

  url <- "https://www.vedur.is/skjalftar-og-eldgos/jardskjalftar#view=table"
  txt <-
    url %>%
    httr::GET() %>%
    httr::content("text")

  i1 <- stringr::str_locate(txt, "quakeInfo = \\[")[[2]]
  i2 <- stringr::str_locate(txt, "\\}\\]")[[2]]
  txt %>%
    stringr::str_sub(i1, i1 + 200)

  tb <-
    txt %>%
    stringr::str_sub(i1, i2) %>%
    stringr::str_replace_all("'", "\"") %>%
    stringr::str_replace_all("new Date", "\"DATE") %>%
    stringr::str_replace_all("\\)", "\"") %>%
    #str_replace_all(",", ".") %>%
    jsonlite::fromJSON() %>%
    tidyr::as_tibble() %>%
    dplyr::mutate_all(dplyr::funs(stringr::str_replace(., ",", "\\."))) %>%
    #mutate_all(str_replace(., ",", "\\.")) %>%
    dplyr::mutate_at(c("a", "lat", "lon", "dep", "s", "q", "dL"), as.numeric) %>%
    dplyr::rename(size = s) %>%
    dplyr::mutate(dD = stringr::str_trim(dD)) %>%
    tidyr::separate(t, into = c("ym", "d", "h", "m", "s"), sep = ",") %>%
    dplyr::mutate(ym = stringr::str_sub(ym, 6)) %>%
    dplyr::mutate(year = stringr::str_sub(ym, 1, 4),
                  month = stringr::str_sub(ym, 6)) %>%
    tidyr::separate(month, c("month", "dummy")) %>%
    dplyr::mutate(date = paste0(year, "-", month, "-", d),
                  time = lubridate::ymd_hms(paste0(date, " ", h, ":", m, ":", s)),
                  date = lubridate::ymd(date)) %>%
    dplyr::rename(z = dep) %>%
    dplyr::mutate(year = as.integer(year),
                  date = as_date(time)) %>%
    #dplyr::filter(time > max(res$time)) %>%
    dplyr::select(date, time, lat, lon, z, size, size, year, q)
  bind_rows(res %>% mutate(yfirfarinn = TRUE),
            tb  %>% mutate(yfirfarinn = FALSE))
}
