library(tidyverse)
library(shinydashboard)
library(DT)
library(lubridate)
library(ggthemes)
library(googleVis)
library(tibble)
library(dplyr)
library(tidyr)


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
  return(trimws(v))
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
#macys_df["gender"] <- sapply(macys_df$watch_model, collect_gender)
macys_5_rating <- nrow(macys_df[macys_df["rating"] == 100 & !is.na(macys_df["rating"]), ])

movado_df <- read.csv("data/movado_website.csv", stringsAsFactors = FALSE)  %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price)))) %>% 
  mutate(model_number = as.character(model_number))

nordstrom_df <- read.csv("data/nordstrom_website.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
nordstrom_5_rating <- nrow(nordstrom_df[nordstrom_df["rating"] == 5 & !is.na(nordstrom_df["rating"]), ])



#AMAZON COPY PASTE

amazon_df <- read.csv("data/amazonsellers.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = gsub(",|$", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
amazon_df["gender"] <- sapply(amazon_df$product, collect_gender)
amazon_df["model_number"] <- sapply(amazon_df$product, collect_model_num)
amazon_df["collection"] <- sapply(amazon_df$product, collect_collection)
amazon_df <- amazon_df %>% mutate(., code = trimws(as.character(code)))
amazon_df <- amazon_df %>% mutate(., q_count = trimws(as.character(q_count)))
amazon_df <- amazon_df %>% mutate(., q_count = gsub(" answered questions", "", q_count))
amazon_df <- amazon_df %>% mutate(., rev_count = gsub(" ratings| rating", "", rev_count))
amazon_df <- amazon_df %>% mutate(., q_count = ifelse(q_count == "", 0, q_count))
amazon_df <- amazon_df %>% mutate(., rev_count = as.numeric(ifelse(rev_count == "", 0, rev_count)))
amazon_df <- amazon_df %>% mutate(., star = as.character(star))
amazon_df <- amazon_df %>% mutate(., star = ifelse(nchar(star) < 5,"0.0 out of 5 stars", ifelse(nchar(star) > 18, "0.0 out of 5 stars", star )))
amazon_df <- amazon_df %>% mutate(., star = as.numeric(gsub(" out of 5 stars", "", star)))
amazon_d_df <- amazon_df %>% distinct(., code, .keep_all = TRUE)

gs = amazon_df %>% group_by(seller) %>% mutate(., count = n())
ungroup(gs)

amazon_dt <- gs %>% 
  select(seller, count, product, everything()) %>% 
  nest(-seller, -count)

data <- amazon_dt %>% {bind_cols(data_frame(' ' = rep('&oplus;',nrow(.))),.)}

# get dynamic info and strings
nested_columns         <- which(sapply(data,class)=="list") %>% setNames(NULL)
not_nested_columns     <- which(!(seq_along(data) %in% c(1,nested_columns)))
not_nested_columns_str <- not_nested_columns %>% paste(collapse="] + '_' + d[") %>% paste0("d[",.,"]")


# The callback
# turn rows into child rows and remove from parent
callback <- paste0("
                    table.column(1).nodes().to$().css({cursor: 'pointer'});
                
                    // Format data object (the nested table) into another table
                    var format = function(d) {
                      if(d != null){ 
                        var result = ('<table id=\"child_' + ",not_nested_columns_str," + '\">').replace('.','_') + '<thead><tr>'
                        for (var col in d[",nested_columns,"]){
                          result += '<th>' + col + '</th>'
                        }
                        result += '</tr></thead></table>'
                        return result
                      }else{
                        return '';
                      }
                    }
                
                    var format_datatable = function(d) {
                      var dataset = [];
                      for (i = 0; i < + d[",nested_columns,"]['product'].length; i++) {
                        var datarow = [];
                        for (var col in d[",nested_columns,"]){
                          datarow.push(d[",nested_columns,"][col][i])
                        }
                        dataset.push(datarow)
                      }
                      var subtable = $(('table#child_' + ",not_nested_columns_str,").replace('.','_')).DataTable({
                        'data': dataset,
                        'autoWidth': true, 
                        'deferRender': true, 
                        'info': false, 
                        'lengthChange': false, 
                        'ordering': true, 
                        'paging': false, 
                        'scrollX': false, 
                        'scrollY': false, 
                        'searching': false 
                      });
                    };
                
                    table.on('click', 'td.details-control', function() {
                      var td = $(this), row = table.row(td.closest('tr'));
                      if (row.child.isShown()) {
                        row.child.hide();
                        td.html('&oplus;');
                      } else {
                        row.child(format(row.data())).show();
                        td.html('&CircleMinus;');
                        format_datatable(row.data())
                      }
                    });"
)

#AMAZON COPY PASTE



#test_df <- full_join(select(movado_df, -"online_exclusive", -"in_stock"), amazon_df, by="model_number")
