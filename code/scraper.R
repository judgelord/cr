
library (rvest)
library(tidyverse)
library(magrittr)

## txt URLs look like this:
# url <- "https://www.congress.gov/congressional-record/2017/6/6/senate-section/article/S3253-6"

# a date range to scrape
dates <- seq(as.Date("2007/01/01"), 
             Sys.Date(),
             #as.Date("2020/01/01"),
             by = "day")

# sections to scrape
sections <- c("senate-section", 
              "house-section", 
              "extensions-of-remarks-section")

base <- "https://www.congress.gov"

# a function to make a data frame of of all cr text urls for a date
get_cr_df <- function(date, section){

  ## For testing
  # section <- "senate-section"
  # date <- "2020-09-15"
  
url <- str_c(base, 
             "congressional-record", 
             date %>% str_replace_all("-", "/"), 
             section, sep = "/")

pages <- read_html(url) %>%
  html_nodes("a")
  
d <- tibble(name = html_text(pages),
            path = html_attr(pages, "href")) %>% 
  # trim down to html txt pages
  filter(path %>% str_detect("article")) 

return(d)
}

d_init <-  tibble(name = "",
                  path = "",
                  file = "")


# make a data frame of urls and file paths
df <- map2_dfr(dates, sections, possibly(get_cr_df, otherwise = d_init))

# drop dates where for which there is no record
df %<>% filter(name != "")


# download raw html?
download = T

# make file path
df %<>% 
  mutate(file = here::here("data", 
                           "html", 
                           str_c("CREC-", 
                                 #path %>% ,
                                 date,
                                 "-",
                                 # the file title
                                 path %>% str_remove(".*article/"), 
                                 ".html")))

# already downloaded 
downloaded <- list.files(here::here("html"))

# a function to download html
get_cr_html <- function(path, file){
  message(path)
  if(!file %>% str_remove(".*html/") %in% downloaded){
    read_html(str_c(base, path)) %>% write_html(file = file)
  }
}

## test 
# get_cr(d$path[2], d$file[2])

if(download == T){
Sys.time()
walk2(df$path, df$file, get_cr_html)
Sys.time()
}


# Over time by type



