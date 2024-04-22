rm(list=ls())
#Wikipedia explorer
library(tidyverse)
library(rvest)
library(polite)

rand_page <- function(){
  random_url <- "https://en.wikipedia.org/wiki/Special:Random"
  read_html(random_url)
}
# Getting the  title 
title <- function(page){
  page %>% html_element("title") %>% html_text %>%
    str_remove(" - Wikipedia")
}
# Unique  links 
links <- function(page){
  rel_links <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    unique()
  rel_links[str_detect(rel_links, '/wiki/')]
}
# Get names and links from a page
links_and_names <- function(page){
  rel_links <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_attr("href")
  links <- rel_links[str_detect(rel_links, '/wiki/')]
  names <- page %>%
    html_elements("#content") %>%
    html_elements("p") %>%
    html_elements("a") %>%
    html_text()
  names <- names[str_detect(rel_links, "/wiki/")]
  out <- data.frame(name = names, url = links)
  unique(out)
}
# grabbing a page from a relative link
rel_get <- function(rel_link){
  url <- paste0("https://en.wikipedia.org", rel_link)
  politely(read_html)(url)
}

get_links <- function(rel_link){
  page <- rel_get(rel_link)
  page_title <- title(page)
  links_names <- links_and_names(page)
  data.frame(title = page_title, links = links_names$url, link_name = links_names$name, link_count = nrow(links_names))
}
# multiple pages
recursive_get <- function(rel_link = "/wiki/Moth", count = 0){
  out <- get_links(rel_link)
  links_to_try <- out$links
  for(l in links_to_try){
    if (count >= 100) {
      break
    }
    out2 <- recursive_get(l, count)
    out <- full_join(out, out2)
    rm(out2)
    count <- nrow(out)
  }
  out
}

example_page <- recursive_get()

example_page <- example_page[order(-example_page$link_count), ]

write_csv(example_page, "wiki_links.csv")

