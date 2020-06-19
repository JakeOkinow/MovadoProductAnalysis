library(tidyverse)
library(shinydashboard)
library(DT)
library(lubridate)
library(ggthemes)
library(googleVis)
library(mltools)


### DEFINE CLEANING FUNCTIONS ###

collect_gender <- function(x){
  m <- regexpr("wom.n'?s?|ladies", x, perl=TRUE, ignore.case = TRUE)
  n <- regexpr(" m.n'?s?|^m.n'?s? ", x, perl=TRUE, ignore.case = TRUE)
  o <- regexpr("unisex", x, perl=TRUE, ignore.case = TRUE)
  if (m[1] != -1){
    return("Women's")
  }else if (n[1] != -1){
    return("Men's")
  } else if (o[1] != -1){
    return("Unisex")
  } else{return("Unknown")}}


collect_model_num <- function(x, y){
    m <- regexpr("\\d+$", x, perl=FALSE, fixed=FALSE)
    n <- regexpr("\\d{4}\\d+", y, perl=FALSE, fixed=FALSE)
    if (m[1] != -1){
      return(regmatches(x, m))
    } else if (n[1] != -1){
      return(regmatches(y, n))
    } else {
      return("Unknown")}}


collect_case_diameter <- function(x){
  m <- regexpr("\\d{2}mm", x, perl=FALSE, fixed=FALSE)
  if (m[1] != -1){return(as.numeric(gsub("mm", "", regmatches(x, m))))
  } else {return("Unknown")}}


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


collect_collection <- function(x){
  x = str_to_lower(x)
  m <- regexpr("movado bold|meusem classic|connect 2.0", x, perl=FALSE, fixed=FALSE)
  p <- regexpr("movado ultra slim|modern 47|movado face", x, perl=FALSE, fixed=FALSE)
  s <- regexpr("heritage series|musem sport|series 800", x, perl=FALSE, fixed=FALSE)
  w <- regexpr("esperanze|sapphire|red label", x, perl=FALSE, fixed=FALSE)
  z <- regexpr("faceto|kora|la nouvelle", x, perl=FALSE, fixed=FALSE)
  c <- regexpr("vizio|strato|1881 auotmatic", x, perl=FALSE, fixed=FALSE)
  if (m[1] != -1){
    return(regmatches(x, m))
  } else if (p[1] != -1){
   return(regmatches(x, p))
  } else if (s[1] != -1){
    return(regmatches(x, s))
  } else if (w[1] != -1){
    return(regmatches(x, w))
  } else if (z[1] != -1){
    return(regmatches(x, z))
  } else if (c[1] != -1){
    return(regmatches(x, c))
  }  else {return("Unknown")}}


collect_collection_nordstrom <- function(x){
  x = str_to_lower(x)
  m <- regexpr("heritage", x, perl=FALSE, fixed=FALSE)
  n <- regexpr("amorosa", x, perl=FALSE, fixed=FALSE)
  o <- regexpr("bold ceramic", x, perl=FALSE, fixed=FALSE)
  p <- regexpr("connect", x, perl=FALSE, fixed=FALSE)
  q <- regexpr("bold evolution", x, perl=FALSE, fixed=FALSE)
  r <- regexpr("bold fusion", x, perl=FALSE, fixed=FALSE)
  s <- regexpr("esperanza", x, perl=FALSE, fixed=FALSE)
  t <- regexpr("museum sport", x, perl=FALSE, fixed=FALSE)
  w <- regexpr("museum classic", x, perl=FALSE, fixed=FALSE)
  z <- regexpr("sapphire", x, perl=FALSE, fixed=FALSE)
  if (m[1] != -1){ return("Movado Heritage Series")
  } else if (n[1] != -1){ return("Amorosa")
  } else if (o[1] != -1){ return("Movado BOLD Ceramic")
  } else if (p[1] != -1){ return(paste(last(unlist(strsplit(x, " "))), "Movado Connect 2.0"))
  } else if (q[1] != -1){ return("Movado BOLD Evolution")
  } else if (r[1] != -1){ return("Movado BOLD Fusion")
  } else if (s[1] != -1){ return("Esperanza")
  } else if (t[1] != -1){ return("Museum Sport")
  } else if (w[1] != -1){ return("Museum Classic")
  } else if (z[1] != -1){ return("Sapphire")
  } else {return("Unknown")}
}

collect_movement <- function(x){
  m <- regexpr("connect", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
  n <- regexpr("1881 automatic|red label", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
  o <- regexpr("chronograph", x, ignore.case = TRUE, perl=FALSE, fixed=FALSE)
  if (m[1] != -1){ return("Smart module")
  } else if (n[1] != -1){ return(regmatches(x, n))
  } else if (o[1] != -1){ return("Swiss quartz chronograph movement")
  } else {return("Swiss quartz movement")}
}


### LOAD DATA AND CLEAN ###

macys_df <- read.csv("data/macys_website.csv", stringsAsFactors = FALSE)  %>% 
  mutate(rating = as.numeric(gsub("%", "", rating))) %>%
  mutate(price = gsub("Orig. |Now |Sale |,", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price))))
macys_df["gender"] <- sapply(macys_df$watch_model, collect_gender)
macys_df["case_diameter"] <- sapply(macys_df$watch_model, collect_case_diameter)
macys_df["model_number"] <- mapply(collect_model_num, macys_df$description, y=macys_df$watch_model)
macys_df <- macys_df %>% mutate(model_number = gsub("^0", "", model_number))
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
nordstrom_df["collection"] <- sapply(nordstrom_df$watch_model, collect_collection_nordstrom)
nordstrom_df["movement"] <- sapply(nordstrom_df$watch_model, collect_movement)

amazon_df <- read.csv("data/amazonsellers copy.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = as.numeric(gsub(",|\\$", "", price))) %>% 
  mutate(code = trimws(code)) %>% 
  mutate(model_number = trimws(code)) %>% 
  mutate(q_count = ifelse(q_count == "", 0, as.numeric(gsub(" answered questions| answered question", "", trimws(q_count))))) %>% 
  mutate(star = as.numeric(unlist(lapply(strsplit(star, " "), '[[', 1)))) %>% 
  mutate(rev_count = ifelse(rev_count == "", 0, as.numeric(gsub(" ratings| rating", "", rev_count))))
amazon_df["gender"] <- sapply(amazon_df$product, collect_gender)
amazon_df["collection"] <- sapply(amazon_df$product, collect_collection)
amazon_d_df <- amazon_df %>% distinct(., code, .keep_all = TRUE)

prices_df <- left_join(select(movado_df, "model_number", "watch_model", "price"), 
                     select(macys_df, "model_number", "price", "watch_model"), by= "model_number", suffix = c("_movado", "_macys")) %>% 
  left_join(select(amazon_df, "model_number", "price","product"),  by= "model_number")

prices_df$difference <- if_else(is.na(prices_df$price_macys) & is.na(prices_df$price), 0,
                                ifelse(is.na(prices_df$price_macys), (prices_df$price_movado - prices_df$price), 
                                       ifelse(is.na(prices_df$price), (prices_df$price_movado - prices_df$price_macys),
                                      ifelse(prices_df$price < prices_df$price_macys, (prices_df$price_movado - prices_df$price), 
                                (prices_df$price_movado - prices_df$price_macys)))))

max_diff <- prices_df[prices_df["difference"] == max(prices_df$difference, na.rm = TRUE), ]
sec_diff <- prices_df[prices_df["difference"] == sort(prices_df$difference, decreasing=TRUE)[2], ]
thrd_diff <- prices_df[prices_df["difference"] == sort(prices_df$difference, decreasing=TRUE)[3], ]


macys_df$group <-cut(macys_df$price, seq(100, 3000, by=100), labels = FALSE)

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


