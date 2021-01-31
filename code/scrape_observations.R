# IFS Observations Analysis - 1st Attempt
#Packages---------------------------------------------------------
library(tidyverse)
library(rvest)
library(lubridate)

#Define base url -------------------------------------------------
#I have alread searched and filtered for observation articles only at this url
base_url <- "https://www.ifs.org.uk/publications?q=&partial_results=1&types%5b%5d=26&year_published%5bstart%5d=&year_published%5bend%5d=&author=&page="

#Create list of repeated publications to exclude------------------------------

ifs_exclude <- read_html(paste0(base_url, "1"))

exclude_urls <- ifs_search %>% 
  html_nodes("a") %>%
  html_attr('href') %>%
  as.data.frame() %>%
  rename(urls = 1) %>%
  mutate(urls = str_replace(urls, "/", "\\")) %>%
  filter(str_detect(urls, "publications/")) %>%
  slice(-(11:20)) 

#Save as vector for lapply
exclude_urls <- as.vector(exclude_urls$urls)

#Loop through pages to get urls of publications------------------------
#As of 31/01/21 there are 34 pages - check when running in the future.
page_numbers <- 1:34

#Loop over these urls and get list of urls of search results
url_list <- lapply(page_numbers, function(search_page){
  url <- paste0(base_url, search_page)
  ifs_search <- read_html(url)
  final_list <- ifs_search %>% 
    html_nodes("a") %>%
    html_attr('href') %>%
    as.data.frame() %>%
    rename(urls = 1) %>%
    mutate(urls = str_replace(urls, "/", "\\")) %>%
    filter(str_detect(urls, "publications/")) %>%
    filter(!(urls %in% exclude_urls))
})
url_df <- do.call(rbind, url_list)

#Ingest article contents-----------------------------------------------------

pub_url <- "https://www.ifs.org.uk/"

#Use list of urls to get out contents of all observations articles
observations_list <- lapply(as.vector(url_df$urls), function(pub_num){
  url <- paste0(pub_url, pub_num)
  
  pub_page <- read_html(url)
  
  #Parse out information
  pub_df <- pub_page %>%
    #Author - adjusting for the fact there can be multiple authors
    html_nodes(xpath = '//meta[@name="citation_author"]') %>%
    html_attr('content') %>%
    as_tibble() %>%
    mutate(author_num = paste0("author_", row_number())) %>%
    pivot_wider(names_from = author_num, values_from = value) %>%
    #Article Text
    bind_cols(
      pub_page %>%
        html_nodes(xpath = "//div[@class='c-page__content c-page__content--img-margin js-cms-render__content']") %>%
        html_text() %>%
        as_tibble() %>%
        rename(article_text = 1)
    ) %>%
    #Date published online
    bind_cols(
      pub_page %>%
        html_nodes(xpath = '//meta[@name="citation_online_date"]') %>%
        html_attr('content') %>%
        as_tibble() %>%
        rename(article_date = 1)
    ) %>%
    #Title of article
    bind_cols(
      pub_page %>%
        html_nodes(xpath = '//meta[@name="citation_title"]') %>%
        html_attr('content') %>%
        as_tibble() %>%
        rename(article_title = 1)
    )
})

#Bind rows together and bind in urls to get unique identifier
observations_df <- do.call(bind_rows, observations_list) %>%
  bind_cols(url_df)

#Clean date and unique identifier ---------------------------------

final_df <- observations_df %>%
  mutate(article_date = dmy(article_date)) %>%
  mutate(uniq_id = str_replace(urls, "publications/", "")) %>%
  select(-urls)

#Save data--------------------------------------
save(final_df, file = "data/210131_IFS_Observations.Rdata")