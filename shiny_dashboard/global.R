library(tidyverse)
library(shinydashboard)
library(DT)
library(lubridate)
library(ggthemes)
library(scales)
library(networkD3)
library(googleVis)
library(wordcloud)
library(stopwords)
# library(wesanderson)


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

# this is to collect the case diameter as per the title
collect_case_diameter <- function(x){
  m <- regexpr("\\d{2}.?\\d?mm", x)
  if (m[1] != -1){return(regmatches(x, m))
  } else {return("Unknown")}}

# specifically designed for Nordstrom's bullet_details, to compare descrepancies
collect_case_diameter_from_details <- function(x){
  m <- regexpr("^\\d{2}.?\\d?mm", x)
  if (m[1] != -1){return(regmatches(x, m))
  } else {return("Unknown")}}


collect_crystal <- function(x){
   m <- regexpr("k.?1|sapphire|gorilla glass|crystal", x, ignore.case = TRUE)
    if (m[1] != -1){ return(ifelse(regmatches(x, m) == "crystal", "K1", regmatches(x, m)))
    } else {return("Unknown")}}


collect_dial <- function(x){
  v <- c()
  m <- regexpr("sophisticated", x, ignore.case = TRUE)
  n <- regexpr("sunray dial", x, ignore.case = TRUE)
  o <- regexpr("museum.*dot", x, ignore.case = TRUE)
  p <- regexpr("everyday style", x, ignore.case = TRUE)
  q <- regexpr("cocktail.*style", x, ignore.case = TRUE)
  r <- regexpr(" elegan.*", x, ignore.case = TRUE)
  if (m[1] != -1){v <- "'sophisticated'"}
  if (n[1] != -1){v <- paste(v, "'sunray dial'")}
  if (o[1] != -1){v <- paste(v, "'Museum dot'")}
  if (p[1] != -1){v <- paste(v, "'everyday style'")}
  if (q[1] != -1){v <- paste(v, "'cocktail-style'")}
  if (r[1] != -1){v <- paste(v, "'elegant'")}
  if (is_null(v)){v <- ""}
  v
}


collect_collection <- function(x){
  x = str_to_lower(x)
  m <- regexpr("movado bold|meusem classic|connect 2.0", x)
  p <- regexpr("movado ultra slim|modern 47|movado face", x)
  s <- regexpr("heritage series|musem sport|series 800", x)
  w <- regexpr("esperanze|sapphire|red label", x)
  z <- regexpr("faceto|kora|la nouvelle", x)
  c <- regexpr("vizio|strato|1881 auotmatic", x)
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
  m <- regexpr("heritage", x)
  n <- regexpr("amorosa", x)
  o <- regexpr("bold .*ceramic", x)
  p <- regexpr("connect", x)
  q <- regexpr("bold evolution", x)
  r <- regexpr("bold fusion", x)
  s <- regexpr("esperanza", x)
  t <- regexpr("museum sport", x)
  v <- regexpr("bold .* bangle", x)
  w <- regexpr("museum classic", x)
  y <- regexpr("series 800", x)
  z <- regexpr("sapphire", x)
  if (m[1] != -1){ return("Movado Heritage Series")
  } else if (n[1] != -1){ return("Amorosa")
  } else if (o[1] != -1){ return("Movado BOLD Ceramic")
  } else if (p[1] != -1){ return(paste(last(unlist(strsplit(x, " "))), "Movado Connect 2.0"))
  } else if (q[1] != -1){ return("Movado BOLD Evolution")
  } else if (r[1] != -1){ return("Movado BOLD Fusion")
  } else if (s[1] != -1){ return("Esperanza")
  } else if (t[1] != -1){ return("Museum Sport")
  } else if (v[1] != -1){ return("Movado BOLD Bangle")
  } else if (w[1] != -1){ return("Museum Classic")
  } else if (y[1] != -1){ return("Series 800")
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
  mutate(price = gsub("Orig. |Now |Sale |,", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price)))) %>% 
  mutate(price = if_else(nchar(sale) == 25, price*.75, price))
macys_df["gender"] <- sapply(macys_df$watch_model, collect_gender)
macys_df["case_diameter"] <- sapply(macys_df$watch_model, collect_case_diameter)
macys_df["model_number"] <- mapply(collect_model_num, macys_df$description, y=macys_df$watch_model)
macys_df <- macys_df %>% mutate(model_number = gsub("^0", "", model_number))
macys_5_rating <- nrow(macys_df[macys_df["rating"] == 100 & !is.na(macys_df["rating"]), ])

movado_df <- read.csv("data/movado_website.csv", stringsAsFactors = FALSE)  %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price)))) %>% 
  mutate(model_number = as.character(model_number))

nordstrom_df <- read.csv("data/nordstrom_website.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = gsub(",", "", price)) %>% mutate(price = as.numeric(substr(price, 2, nchar(price)))) %>% 
  mutate(model_number = as.character(model_number))
nordstrom_5_rating <- nrow(nordstrom_df[nordstrom_df["rating"] == 5 & !is.na(nordstrom_df["rating"]), ])
nordstrom_df["case_diameter"] <- sapply(nordstrom_df$watch_model, collect_case_diameter)
nordstrom_df["crystal"] <- sapply(nordstrom_df$bullet_details, collect_crystal)
nordstrom_df["dial"] <- trimws(sapply(nordstrom_df$description, collect_dial, simplify = TRUE, USE.NAMES = FALSE))
nordstrom_df["collection"] <- sapply(nordstrom_df$watch_model, collect_collection_nordstrom)
nordstrom_df["movement"] <- sapply(nordstrom_df$watch_model, collect_movement)
nordstrom_df["bullet_d_case_d"] <- sapply(nordstrom_df$bullet_details, collect_case_diameter_from_details)

amazon_df <- read.csv("data/amazonsellers copy.csv", stringsAsFactors = FALSE) %>% 
  mutate(price = as.numeric(gsub(",|\\$", "", price))) %>% 
  mutate(code = trimws(code)) %>% 
  mutate(model_number = trimws(code)) %>% 
  mutate(q_count = ifelse(q_count == "", 0, as.numeric(gsub(" answered questions| answered question", "", trimws(q_count))))) %>% 
  mutate(star = as.numeric(unlist(lapply(strsplit(star, " "), '[[', 1)))) %>% 
  mutate(rev_count = ifelse(rev_count == "", 0, as.numeric(gsub(" ratings| rating", "", rev_count))))
amazon_df["gender"] <- sapply(amazon_df$product, collect_gender)
amazon_df["collection"] <- sapply(amazon_df$product, collect_collection)
amazon_5_rating <- length(unique(amazon_df[amazon_df["star"] == 5 & !is.na(amazon_df["star"]), "model_number"]))

amazon_d_df <- amazon_df %>% distinct(., code, .keep_all = TRUE)
amazon_s_df <- amazon_df %>% group_by(seller) %>% mutate(., count = n())
amazon_s_df <- amazon_s_df %>% distinct(., seller, .keep_all = TRUE)
amazon_p_df <- amazon_df %>% group_by(product) %>% mutate(., count = n())
amazon_p_df <- amazon_p_df %>% distinct(., product, .keep_all = TRUE)

amazon_s_df <- amazon_s_df %>% mutate(., name =paste(as.character(seller), count, sep=" : " ))
amazon_s_df <- amazon_s_df[order(-amazon_s_df$count),]
s_list <- setNames(amazon_s_df$seller, amazon_s_df$name)

amazon_p_df <- amazon_p_df %>% mutate(., name =paste(as.character(product), count, sep=" : " ))
amazon_p_df <- amazon_p_df[order(-amazon_p_df$count),]
p_list <- setNames(amazon_p_df$product, amazon_p_df$name)


# PROVIDE MODEL NUMBERS TO NORDSTROM DATA
real_m_num <- nordstrom_df %>% 
  mutate(real_m_num = ifelse(model_number == "3259168", "3600086", ifelse(model_number == "3971613", "3600278",
           ifelse(model_number == "4388010", "3650004", ifelse(model_number == "4776696", "607153",
           ifelse(model_number == "4776703", "607180", ifelse(model_number == "4832729", "607178", 
           ifelse(model_number == "4944900", "3600521", ifelse(model_number == "4944904" & color == "Gold", "3600508",
           ifelse(model_number == "4944904" & color == "Blue", "3600510", ifelse(model_number == "4944904" & color == "Gunmetal", "3600509",
           ifelse(model_number == "4944918", "607358", ifelse(model_number == "4944938", "607220",
           ifelse(model_number == "4944940", "607219", ifelse(model_number == "4945350" & color == "White", "3600616",
           ifelse(model_number == "4945350" & color == "Blush Pink", "3600615",
           ifelse(model_number == "5030472", "3600507", ifelse(model_number == "5071045", "607226",
           ifelse(model_number == "5064190", "3600551", ifelse(model_number == "5071046", "607240",
           ifelse(model_number == "5071049" & color == "Black", "3600562", ifelse(model_number == "5071049" & color == "Gold", "3600560",
           ifelse(model_number == "5071049" & color == "Blue", "3600610", ifelse(model_number == "5071049" & color == "Grey", "3600561",
           ifelse(model_number == "5071050", "607307", ifelse(model_number == "5108250", "3650069",
           ifelse(model_number == "5247092" & color == "Silver", "3600595", ifelse(model_number == "5247092" & color == "Light Gold", "3600598",
           ifelse(model_number == "5247093", "3600602",
           ifelse(model_number == "5247097", "3650092", ifelse(model_number == "5247100" & color == "Silver", "3600589",
           ifelse(model_number == "5247100" & color == "Gold", "3600588", ifelse(model_number == "5247103", "3600599",
           ifelse(model_number == "5247104" & color == "Silver/ Blue/ Silver", "607349", ifelse(model_number == "5247104" & color == "Gold/ Black/ Gold", "607396",
           ifelse(model_number == "5257771", "3600619", ifelse(model_number == "5357327", "3600592", ifelse(model_number == "5358932", "3600593",
           ifelse(model_number == "5402100" & color == "Gold", "3600682", ifelse(model_number == "5402100" & color == "Blue", "3600683",
           ifelse(model_number == "5402102", "3660032", ifelse(model_number == "5402108", "3600629",
           ifelse(model_number == "5402112" & color == "Gold", "3600656", ifelse(model_number == "5402112" & color == "Rose Gold", "3600657",
           ifelse(model_number == "5402190" & color == "Gold/ Beige/ Gold", "3600640", ifelse(model_number == "5402190" & color == "Silver/ Blush", "3600702",
           ifelse(model_number == "5402190" & color == "Silver", "3600638", ifelse(model_number == "5402191", "3600653", "Unknown")))))))))))))))))))))))))))))))))))))))))))))))) %>% 
  select(real_m_num)
nordstrom_df <- cbind.data.frame(nordstrom_df, real_m_num)
           

# cont.
nordstrom_df <- nordstrom_df %>% mutate(real_m_num = ifelse(model_number == "5402193" & color == "Cognac/ Blue", "3600630", ifelse(model_number == "5402193" & color == "Cognac/ White/ Gunmetal", "3600631",
           ifelse(model_number == "5402203", "3600660", ifelse(model_number == "5402199" & color == "Silver/ Gold", "3600651",
           ifelse(model_number == "5402199" & color == "Gold", "3600648", ifelse(model_number == "5402199" & color == "Silver/ Rose Gold", "3600647",
           ifelse(model_number == "5402200", "3600675", ifelse(model_number == "5402204", "3660025",
           ifelse(model_number == "5402239", "3600677", ifelse(model_number == "5402240", "3660026",
           ifelse(model_number == "5412752", "3600685", ifelse(model_number == "5412755", "3600629",
           ifelse(model_number == "5415004", "3600632", ifelse(model_number == "5439361", "3600673", 
           ifelse(model_number == "5439467" & color == "Rose Gold", "3600654", ifelse(model_number == "5439467" & color == "Silver", "3600655",
           ifelse(model_number == "5444405", "3650105", ifelse(model_number == "5523030", "3650101", ifelse(model_number == "5577742", "3600699",
           ifelse(model_number == "5577744" & color == "White/ Gold", "3600710", ifelse(model_number == "5577744" & color == "Blush/ Silver", "3600709",
           ifelse(model_number == "5577745", "607472", ifelse(model_number == "5577748", "607471", ifelse(model_number == "5613740", "3600712",
           ifelse(model_number == "5613741", "607511", ifelse(model_number == "5613744" & color == "Grey/ Gold", "3600692", 
           ifelse(model_number == "5613744" & color == "Tan/ Blue", "3600691", ifelse(model_number == "5613745", "607476", 
           ifelse(model_number == "5636328", "3600596", ifelse(model_number == "5648011", "607352", ifelse(model_number == "5661973", "3600639",
           ifelse(model_number == "5661975", "3600649", ifelse(model_number == "5662017", "3600591",
           ifelse(model_number == "5678560" & color == "Black", "3600621", ifelse(model_number == "5678560" & color == "Black/ Gold", "3600623",
           ifelse(model_number == "5678560" & color == "Black/ Silver", "3600624", ifelse(model_number == "5678616" & color == "Black/ Blue/ Silver", "607270",
           ifelse(model_number == "5678616" & color == "Black/ Gold", "607271", ifelse(model_number == "5678616" & color == "Black/ Silver", "607269",
           ifelse(model_number == "5678881", "607203", ifelse(model_number == "5678851", "3600492", ifelse(model_number == "5678896", "0607491",
           ifelse(model_number == "5678898", "3600698", ifelse(model_number == "5679011", "3600708",
           ifelse(model_number == "5679026", "607202", ifelse(model_number == "5679033" & color == "Khaki/ Carnation Gold", "3600643",
           ifelse(model_number == "5679033" & color == "Khaki/ Gold", "3600642", ifelse(model_number == "5679085" & color == "Silver/ Rose Gold", "3600504", 
                      real_m_num)))))))))))))))))))))))))))))))))))))))))))))))))

# cont.
nordstrom_df <- nordstrom_df %>% 
  mutate(model_number = ifelse(model_number == "5679085" & color == "Silver", "3600579", ifelse(model_number == "5679046", "3600658",
           ifelse(model_number == "5683302", "3660028", ifelse(model_number == "5683304", "3660030", ifelse(model_number == "5683305", "3660036",
           ifelse(model_number == "5679407", "3650097", ifelse(model_number == "5679406", "3600586", 
           ifelse(model_number == "5686203" & color == "Black/ Gold", "607271", ifelse(model_number == "5686203" & color == "Black/ Silver", "607269", 
           ifelse(model_number == "5686203" & color == "Black/ Blue/ Silver", "607270", 
           ifelse(model_number == "5686219", "607491", ifelse(model_number == "3833074", "3600258",
           ifelse(model_number == "3502549", "2600142", ifelse(model_number == "4117677", "3600279",
           ifelse(model_number == "5247095", "3600590", ifelse(model_number == "3907993", "3600306",
           ifelse(model_number == "5686205" & color == "Silver/ Rose Gold", "3600504", ifelse(model_number == "5686205" & color == "Silver", "3600501",
           ifelse(model_number == "5686127", "3600086", ifelse(model_number == "5686179", "3600534",
           ifelse(model_number == "5686181", "3600658", ifelse(model_number == "5686250", "3600712",
           ifelse(model_number == "5247098", "3600586", ifelse(model_number == "5686214", "3650088",
           ifelse(model_number == "5686157" & color == "Black/ Silver", "3600624", ifelse(model_number == "5686157" & color == "Black/ Gold", "3600623",
           ifelse(model_number == "5686157" & color == "Black", "3600621", ifelse(model_number == "4497771", "3650011",
           ifelse(model_number == "5402104", "3660030", ifelse(model_number == "5678851", "3600492",
           ifelse(model_number == "5686223", "607307", ifelse(model_number == "4944900" & color == "Cognac/ Grey", "3600521",
           ifelse(model_number == "5577744" & color == "Navy/ Rose Gold", "3600708",
              real_m_num)))))))))))))))))))))))))))))))))) %>% select(-real_m_num)

# ESTABLISH DF OF MOVADO PRODUCTS WITH COMPETITOR PRICING
prices_df <- left_join(select(movado_df, "model_number", "watch_model", "price", "in_stock"), 
                       select(macys_df, "model_number", "price"), by= "model_number", suffix = c("_movado", "_macys")) %>% 
  left_join(select(amazon_df, "model_number", "price_amazon" = "price"),  by = "model_number") %>%
  left_join(select(nordstrom_df, "model_number", "price_nordstrom" = "price"), by = "model_number")

prices_df$difference <- if_else(is.na(prices_df$price_macys) & is.na(prices_df$price_amazon) & is.na(prices_df$price_nordstrom), 0, 
                                prices_df$price_movado - pmin(prices_df$price_macys, prices_df$price_amazon, prices_df$price_nordstrom, na.rm = TRUE))

full_prices_df <- full_join(select(movado_df, "model_number", "watch_model", "price", "in_stock"), 
                            select(macys_df, "model_number", "price"), by= "model_number", suffix = c("_movado", "_macys")) %>% 
  full_join(select(amazon_df, "model_number", "price_amazon" = "price"),  by = "model_number") %>%
  full_join(select(nordstrom_df, "model_number", "price_nordstrom" = "price"), by = "model_number")

full_prices_df$difference <- if_else(is.na(full_prices_df$price_movado), 0, 
                                     ifelse(is.na(full_prices_df$price_macys) & is.na(full_prices_df$price_amazon) & is.na(full_prices_df$price_nordstrom), 0, 
                                            full_prices_df$price_movado - pmin(full_prices_df$price_macys, full_prices_df$price_amazon, 
                                                                               full_prices_df$price_nordstrom, na.rm = TRUE)))
# EXTRACT 1, 2, 3 TOP DIFFERENCES IN PRICE
max_diff <- prices_df[prices_df["difference"] == max(prices_df$difference, na.rm = TRUE), ][1, ]
sec_diff <- prices_df[prices_df["difference"] == sort(prices_df$difference, decreasing=TRUE)[2], ][2, ]
thrd_diff <- prices_df[prices_df["difference"] == sort(prices_df$difference, decreasing=TRUE)[3], ]

# FIND WORD COUNTS OF REVIEWS
macys_df <- macys_df %>% mutate(clean_text = gsub("[This review was collected as part of a promotion.]", "", macys_df$review_text, fixed = TRUE)) %>% 
  mutate(clean_text = gsub(",|\n|  ", " ", clean_text)) %>% mutate(review_w_count = lengths(strsplit(clean_text, " ")), 
                                                                   avg_w_count = ifelse(review_count == 0, 0, review_w_count/review_count))

nordstrom_df <- nordstrom_df %>% mutate(clean_text = gsub("Sweepstakes entry\\n", "", nordstrom_df$review_text, fixed = TRUE)) %>%
  mutate(clean_text = gsub("\\w\\w\\w\\w? \\d\\d?, 20\\d\\d", "", clean_text, perl = TRUE)) %>% mutate(clean_text = gsub('\\n', " ", clean_text, fixed = TRUE)) %>% 
  mutate(review_w_count = lengths(strsplit(clean_text, " ")) - review_count, avg_w_count = ifelse(review_count == 0, 0, review_w_count/review_count))


# FIND POPULAR WORDS
popular_review_words <- data.frame()

macys_words <- macys_df %>% 
  mutate(clean_text = gsub("[This review was collected as part of a promotion.]", "", macys_df$review_text, fixed = TRUE)) %>% 
  mutate(clean_text = gsub("'", "", clean_text, fixed = FALSE)) %>% 
  mutate(clean_text = gsub("\\W", " ", clean_text, fixed = FALSE)) %>% 
  mutate(review_words = strsplit(str_to_lower(clean_text), " +")) %>% 
  select(review_words)
word_freq_macys <- table(unlist(macys_words))
word_freq_df <- cbind.data.frame("word" = names(word_freq_macys), "frequency" = as.integer(word_freq_macys))

nordstrom_words <- nordstrom_df %>% 
  mutate(clean_text = gsub("Sweepstakes entry\\n", "", nordstrom_df$review_text, fixed = TRUE)) %>%
  mutate(clean_text = gsub("\\w\\w\\w\\w? \\d\\d?, 20\\d\\d", "", clean_text, perl = TRUE)) %>% 
  mutate(clean_text = gsub('\\n', " ", clean_text, fixed = TRUE)) %>% 
  mutate(clean_text = gsub("\\W", " ", clean_text, fixed = FALSE)) %>% 
  mutate(review_words = strsplit(str_to_lower(clean_text), " +")) %>%
  select(review_words)
word_freq_nordstrom <- table(unlist(nordstrom_words))
word_freq_df <- rbind.data.frame(word_freq_df, cbind.data.frame("word" = names(word_freq_nordstrom), "frequency" = as.integer(word_freq_nordstrom)))

word_freq_df <- word_freq_df %>% group_by(word) %>% summarise(frequency = sum(frequency)) %>% 
  arrange(desc(frequency)) %>% filter(!(word %in% c(stopwords(source = "smart"), 
                                                    "watch", "watches", "purchase", "purchased", 
                                                    "day", "made", "worn", "movado")))







