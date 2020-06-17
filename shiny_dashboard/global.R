library(tidyverse)
library(shinydashboard)
library(DT)
library(lubridate)
library(ggthemes)
library(googleVis)


### DEFINE CLEANING FUNCTIONS ###

collect_gender <- function(X){
  for (item in X){
    v <- c()
    m <- regexpr("wom.n'?s?|ladies", item, perl=TRUE, ignore.case = TRUE)
    n <- regexpr("m.n'?s?", item, perl=TRUE, ignore.case = TRUE)
    o <- regexpr("unisex", item, perl=TRUE, ignore.case = TRUE)
    if (m[1] != -1){
      v <- c(v, regmatches(item, m))
    }else if (n[1] != -1){
      v <- c(v, regmatches(item, n))
    } else if (o[1] != -1){
      v <- c(v, regmatches(item, o))
    } else{
      v <- c(v, "Unknown")
    }
  } 
  return(v)
}

collect_model_num <- function(X){
  for (item in X){
    v <- c()
    m <- regexpr(" \\d{4}\\d+ | \\d+$", item, perl=FALSE, fixed=FALSE)
    n <- regexpr("-?—?-?\\d{4}\\d+.?0?)", item, perl=FALSE, fixed=FALSE)
    if (m[1] != -1){
      v <- c(v, regmatches(item, m))
    } else if (n[1] != -1){
      v <- c(v, gsub(")", "", regmatches(item, n)))
    } else {
      v <- c(v, "Unknown")
    }
  } 
  return(v)
}


### LOAD DATA AND CLEAN ###

macys_df <- read.csv("data/macys_website.csv", stringsAsFactors = FALSE)  %>% 
  mutate(rating = as.numeric(gsub("%", "", rating))) %>%
  mutate(price = gsub("Orig. |Now |Sale |,", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
macys_df["gender"] <- sapply(macys_df$watch_model, collect_gender)
macys_5_rating <- nrow(macys_df[macys_df["rating"] == 100 & !is.na(macys_df["rating"]), ])

movado_df <- read.csv("data/movado_website.csv", stringsAsFactors = FALSE)  %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))

nordstrom_df <- read.csv("data/nordstrom_website.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
nordstrom_5_rating <- nrow(nordstrom_df[nordstrom_df["rating"] == 5 & !is.na(nordstrom_df["rating"]), ])

amazon_df <- read.csv("data/amazonsellerssecond copy.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = gsub(",|$", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
amazon_df["gender"] <- sapply(amazon_df$product, collect_gender)
amazon_df["product_number"] <- sapply(amazon_df$product, collect_model_num)



