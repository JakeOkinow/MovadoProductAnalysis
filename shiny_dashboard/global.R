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
    n <- regexpr(" m.n'?s?|^m.n'?s? ", item, perl=TRUE, ignore.case = TRUE)
    o <- regexpr("unisex", item, perl=TRUE, ignore.case = TRUE)
    if (m[1] != -1){
      v <- c(v, "Women's")
    }else if (n[1] != -1){
      v <- c(v, "Men's")
    } else if (o[1] != -1){
      v <- c(v, regmatches(item, o))
    } else{
      v <- c(v, "unknown")
    }
  } 
  return(v)
}


collect_model_num <- function(x, y){
    m <- regexpr("\\d+$", x, perl=FALSE, fixed=FALSE)
    n <- regexpr("\\d{4}\\d+", y, perl=FALSE, fixed=FALSE)
    if (m[1] != -1){
      return(regmatches(x, m))
    } else if (n[1] != -1){
      return(regmatches(y, n))
    } else {
      return("Unknown")
    } 
}


collect_case_diameter <- function(X){
  for (item in X){
    v <- c()
    m <- regexpr("\\d{2}mm", item, perl=FALSE, fixed=FALSE)
    if (m[1] != -1){
      v <- c(v, regmatches(item, m))
    } else {
      v <- c(v, "Unknown")
    }
  } 
  return(trimws(v))
}

collect_crystal <- function(x){
   m <- regexpr("k.?1|sapphire|gorilla glass|crystal", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
    if (m[1] != -1){ return(ifelse(regmatches(x, m) == "crystal", "K1", regmatches(x, m)))
    } else {return("Unknown")}}


collect_dial <- function(x){
  v <- c()
  m <- regexpr("mother.*pearl", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
  n <- regexpr("diamonds", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
  o <- regexpr("museum.*dot", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
  if (m[1] != -1){v <- regmatches(x, m)}
  if (n[1] != -1){v <- paste(v, regmatches(x, n))}
  if (o[1] != -1){v <- paste(v, "Museum with concave dot")}
  if (is_null(v)){v <- ""}
  v
}


collect_collection <- function(X){
  for (item in X){
    v <- c()
    item = str_to_lower(item)
    m <- regexpr("movado bold|meusem classic|connect 2.0", item, perl=FALSE, fixed=FALSE)
    p <- regexpr("movado ultra slim|modern 47|movado face", item, perl=FALSE, fixed=FALSE)
    s <- regexpr("heritage series|musem sport|series 800", item, perl=FALSE, fixed=FALSE)
    w <- regexpr("esperanze|sapphire|red label", item, perl=FALSE, fixed=FALSE)
    z <- regexpr("faceto|kora|la nouvelle", item, perl=FALSE, fixed=FALSE)
    c <- regexpr("vizio|strato|1881 auotmatic", item, perl=FALSE, fixed=FALSE)
    if (m[1] != -1){
      v <- c(v, regmatches(item, m))
    } else if (p[1] != -1){
      v <- c(v, regmatches(item, p))
    } else if (s[1] != -1){
      v <- c(v, regmatches(item, s))
    } else if (w[1] != -1){
      v <- c(v, regmatches(item, w))
    } else if (z[1] != -1){
      v <- c(v, regmatches(item, z))
    } else if (c[1] != -1){
      v <- c(v, regmatches(item, c))
    }  else {
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
macys_df["case_diameter"] <- sapply(macys_df$watch_model, collect_case_diameter)
macys_df["model_number"] <- mapply(collect_model_num, macys_df$description, y=macys_df$watch_model)
macys_5_rating <- nrow(macys_df[macys_df["rating"] == 100 & !is.na(macys_df["rating"]), ])

movado_df <- read.csv("data/movado_website.csv", stringsAsFactors = FALSE)  %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price)))) %>% 
  mutate(model_number = as.character(model_number))

nordstrom_df <- read.csv("data/nordstrom_website.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
nordstrom_5_rating <- nrow(nordstrom_df[nordstrom_df["rating"] == 5 & !is.na(nordstrom_df["rating"]), ])
nordstrom_df["case_diameter"] <- sapply(nordstrom_df$watch_model, collect_case_diameter)
nordstrom_df["crystal"] <- sapply(nordstrom_df$bullet_details, collect_crystal)
nordstrom_df["dial"] <- trimws(sapply(nordstrom_df$description, collect_dial, simplify = TRUE, USE.NAMES = FALSE))

amazon_df <- read.csv("data/amazonsellers copy.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = as.numeric(gsub(",|\\$", "", price))) %>% 
  mutate(model_number = trimws(code)) %>% 
  mutate(q_count = ifelse(q_count == "", 0, as.numeric(gsub(" answered questions| answered question", "", trimws(q_count))))) %>% 
  mutate(star = as.numeric(unlist(lapply(strsplit(star, " "), '[[', 1)))) %>% 
  mutate(rev_count = ifelse(rev_count == "", 0, as.numeric(gsub(" ratings| rating", "", rev_count))))
amazon_df["gender"] <- sapply(amazon_df$product, collect_gender)
amazon_df["collection"] <- sapply(amazon_df$product, collect_collection)


test_df <- full_join(select(movado_df, -"online_exclusive", -"in_stock"), nordstrom_df, by="model_number")
