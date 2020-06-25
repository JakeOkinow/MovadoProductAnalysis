
shinyUI(
  dashboardPage(
      dashboardHeader(title="MOVADO"),
      dashboardSidebar(
          sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Retailers", icon = icon("tags"),
              menuSubItem("Macy's", tabName = "macys"),
              menuSubItem("Nordstrom", tabName = "nordstrom"),
              menuSubItem("Amazon", tabName = "amazon")
                    ),
            menuItem("Product Comparison", tabName = "comparison", icon = icon("barcode")),
            menuItem("Insights", tabName = "insights", icon = icon("info-circle"))
          )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "home",
                  fluidPage(
                    fluidRow(column(width = 8, offset=2, align = "center",
                                    box(width = 12, background = "navy", align = "center", h1(tags$b("MOVADO INSIGHTS\n"), style = "font-size:55px;"),
                                                    h2(tags$b("A Look into Retailers and Resellers")),
                                                    h5("Research and modeling by Victoria Lowery and Jake Okinow")))),
                    fluidRow(column(width = 12, align="center", 
                                    box(width = 12, align = "center",
                                      h2("Results for Movado, Macy's, Nordstrom, and Amazon"),
                                      column(width = 4, align = "center", plotOutput("total_products", height = 360), br(), br(), br(),
                                             h4(tags$b("Total Products: ", as.character(sum(nrow(movado_df), nrow(macys_df), 
                                                                                            nrow(nordstrom_df), length(unique(amazon_df$model_number)))), br(),
                                                       "Total Unique Products: ", as.character(length(unique(c(movado_df$model_number, macys_df$model_number, 
                                                                                                               nordstrom_df$model_number, amazon_df$model_number))))))),
                                      column(width = 4, align = "center", plotOutput("total_reviews"), br(),
                                             h4(tags$b("Total Reviews Scraped: ", as.character(sum(sum(macys_df$review_count), sum(nordstrom_df$review_count), 
                                                                                            sum(unique(amazon_df$rev_count))))))),
                                      column(width = 4, align = "center", plotOutput("total_stars", height = 360), br(), br(), br(),
                                             h4(tags$b("Overall Average Rating: ", as.character(round(mean(c((macys_df$rating/20), 
                                                                                              nordstrom_df$rating, amazon_df$star), na.rm=TRUE 
                                                                                              ), 2)), " / 5")))
                                      )
                                    )
                             )
                    )
                  ),
          tabItem(tabName = "macys",
                  fluidPage(
                    h1(tags$b("Macy's Performance")),
                    h2("Price Distribution"),
                    fluidRow(box(width = 8, plotOutput("macys_price")),
                             column(width = 4, infoBox(width = 12, title = "Average Price",
                                                       value = paste0("$", round(mean(macys_df$price), 2))),
                                    infoBox(width = 12, title = "Median Price",
                                            value = paste0("$", round(median(macys_df$price), 2)))
                             )
                    ),
                    h2("Reviews: Presence and Distribution"), br(),
                    fluidRow(
                             column(width = 6, infoBox(width = 12, fill = TRUE, title = "Most Reviewed Product", value = macys_df[max(macys_df["review_count"]), "watch_model"],
                                                       subtitle = paste(max(macys_df["review_count"]), "total reviews"),
                                                       href = macys_df[max(macys_df["review_count"]), "url"]),
                                    box(width = 12, p("The number of products without reviews received is identical to the number of products without ratings,
                                                      possibly implying that Macy's does not allow customers to rate a product without leaving a review.",
                                                      br(), br(), "We can also see in the graph below on the right that", tags$b(" of the products receiving reviews, nearly half
                                                      of those only receive one review.")) )
                                    ),
                             column(width = 6, 
                                    tabBox(width = 12, 
                                      tabPanel("Reviewed vs. Unreviewed", align = "center", shiny::htmlOutput("macys_zero_reviews_pie")),
                                      tabPanel("Reviews Distribution", plotOutput("macys_review_count_sans_0"))))
                             ),
                    h2("Customer Satisfation: Ratings Distribution"), br(),
                    fluidRow(column(width = 6,
                                    box(width = 12, p("Almost half of Macy's products have received a review, and those with ratings seem heavily right skewed
                                      to being in the 90%-100% satisfaction range.", br(), br(), tags$b(paste0("In fact, ", round(100*macys_5_rating/nrow(macys_df)), "% of all Movado
                                      products listed on Macy's.com are rated 100% satisfaction.")))),
                                    infoBox(width = 9, color = "yellow", title = "Total Unrated Products", value = sum(is.na(macys_df$rating)), icon = icon("comment-slash"),
                                            subtitle = paste0(round(100*sum(is.na(macys_df$rating))/nrow(macys_df)), "% of products listed on Macy's")),
                                    infoBox(width = 9, title="Total 100%-Scoring Products", icon = icon("star"),
                                            subtitle = paste0(round(100*macys_5_rating/nrow(macys_df)), "% of products listed on Macy's"),
                                            value = macys_5_rating)),
                             column(width = 6,
                                    box(width = 12, plotOutput("macys_stars"))
                             ))
                      )
                  ),
          tabItem(tabName = "nordstrom",
                  fluidPage(
                    h1(tags$b("Nordstrom's Performance")),
                    h2("Price Distribution"),
                    fluidRow(box(width = 8, plotOutput("nordstrom_price")),
                             column(width = 4, infoBox(width = 12, title = "Average Price",
                                                       value = paste0("$", round(mean(nordstrom_df$price), 2))),
                                    infoBox(width = 12, title = "Median Price",
                                            value = paste0("$", round(median(nordstrom_df$price), 2)))
                                    )
                              ),
                    h2("Reviews: Presence and Distribution"), br(),
                    fluidRow(box(width = 7, p("Almost exactly half of all products listed on Nordstrom have received at least one review.
                                   Of the products who have received reviews, a majority have received only 1-3 reviews. One
                                   outlier, the Movado", as.character(nordstrom_df[max(nordstrom_df["review_count"]), "watch_model"]), "has over 65 reviews and greatly surpasses all
                                   other Movado products in terms of reviews received.")),
                             column(width = 5, infoBox(width = 12, fill = TRUE, title = "Most Reviewed Product", value = nordstrom_df[max(nordstrom_df["review_count"]), "watch_model"],
                                                       subtitle = paste(max(nordstrom_df["review_count"]), "total reviews")))),
                    fluidRow(box(align = "center", width = 5, htmlOutput("nordstrom_zero_reviews_pie")),
                             box(width = 7, plotOutput("nordstrom_review_count_sans_0"))
                      ),
                  h2("Customer Satisfation: Ratings Distribution"), br(),
                  fluidRow(box(width = 6, plotOutput("nordstrom_stars")),
                           column(width = 6, infoBox(width = 10, title="Total 5-Star-Rated Products", icon = icon("star"),
                                                     subtitle = paste0(round(100*nordstrom_5_rating/nrow(nordstrom_df)), "% of Movado products"),
                                                     value = nordstrom_5_rating),
                                  infoBox(width = 10, color = "yellow", title = "Total Unrated Products", value = sum(is.na(nordstrom_df$rating)), icon = icon("comment-slash"),
                                          subtitle = paste0(round(100*sum(is.na(nordstrom_df$rating))/nrow(nordstrom_df)), "% of Movado products"))
                                  )
                          )
                  )
                  ),
          tabItem(tabName = "amazon", 
                  fluidPage(
                    h1(tags$b("Amazon's Performance")),
                    h2("Customer Satisfation: Ratings Distribution"), br(),
                    fluidRow(box(width = 6, plotOutput("amazon_stars")),
                             column(width = 6, infoBox(width = 10, title="Total 5-Star-Rated Products", icon = icon("star"),
                                                       subtitle = paste0(round(100*amazon_5_rating/length(unique(amazon_d_df$model_number))), "% of products listed on Amazon's"),
                                                       value = amazon_5_rating),
                                    infoBox(width = 10, color = "yellow", title = "Total Unrated Products", value = sum(amazon_d_df$rev_count == 0), icon = icon("comment-slash"),
                                            subtitle = paste0(round(100*sum(amazon_d_df$rev_count == 0)/nrow(amazon_d_df)), "% of products listed on Amazon's"))
                             )
                    ),
                    h2("Reviews: Presence and Distribution"), br(),
                    fluidRow(infoBox(width = 8, fill = TRUE, title = "Most Reviewed Product", value = amazon_df$product[which.max(amazon_df$rev_count)],
                                                       subtitle = paste(max(amazon_d_df$rev_count), "total reviews")
                                     )
                    ),
                    fluidRow(box(align = "center", width = 5, shiny::htmlOutput("amazon_zero_reviews_pie")),
                             box(width = 7, plotOutput("amazon_review_count_sans_0"))),
                    h2("Price Distribution"),
                    fluidRow(box(width = 6, plotOutput("amazon_price")),
                             column(width = 6, infoBox(width = 10, title = "Average Price",
                                                       value = paste0("$", round(mean(amazon_d_df$price), 2))),
                                    infoBox(width = 10, title = "Median Price",
                                            value = paste0("$", round(median(amazon_d_df$price), 2)))
                             )
                    ),
                    h2("seller list and number of listings"), br(),
                    fluidRow(selectInput("seller_var", "Variable:",
                                         s_list),
                             plotOutput("seller_price"),
                             DT::dataTableOutput("seller_table")),
                    h2("Watch list and number of sellers"), br(),
                    fluidRow(selectInput("product_var", "Variable:",
                                         p_list),
                             plotOutput("watches_price"),
                             DT::dataTableOutput("watches_table")),
                    h2("Full Table"), br(),
                    fluidRow(
                      DT::dataTableOutput("full_table"))
                  )
                  ),
          tabItem(tabName = "comparison",
                  selectizeInput("select_prod_num", label = "Select Product Number:", choices = a_list),
                  fluidRow(box(width = 6, title = "Retailers' Price by Product Number", plotOutput("prod_num_graphh")),
                           dataTableOutput("selected_prod_numm"))
          ),
          tabItem(tabName = "insights",
                  fluidPage(
                    h1(tags$b("Movado Compared to Retailers")),
                    h2("Pricing"), br(),
                    fluidRow(box(width = 5, "Amazon tends to have lower priced products, while Macy's and Movado both have most
                                 frequently products priced at $695. However, Macy's average product cost is higher
                                 than Movado's. This could be that Macy's stocks more of the higher-priced items of Movado's. Macy's has also 
                                 waivered up and down greatly with each new scrape, as Macy's drops prices pretty drastically with semi-frequent sales.", br(), br(), 
                                 "The price density chart on the second tab confirms that Macy's is very close in Movado's footstep with stocking more in the 
                                 $750 - $1,750 range.", br(), br(), "The histogram on the third tab further demonstrates the different distributions of inventory, 
                                 particularly with Amazon's lower range inventory."),
                             tabBox(width = 7, tabPanel("Mean/Median Price", plotOutput("overview_price")),
                                    tabPanel("Price Densities", plotOutput("price_density")),
                                    tabPanel("Price Histogram", align = "center", checkboxGroupInput("select_retailer", "Select Retailer:",
                                                                            choices = list("Movado", "Nordstrom",
                                                                                           "Macy's", "Amazon"),
                                                                            selected = c("Movado", "Amazon"), inline = TRUE),
                                             plotOutput("price_histogram"))
                             )
                    ),
                    h3("Maximum Price Difference"),
                    fluidRow(column(width = 4,
                                    box(width = 12, p("To the left is a table combining all of Movado.com's products to those found on Nordstrom's, Macy's, and Amazon's websites.", br(), br(),
                                          "The final column is the difference between
                                          Movado.com's price and the lowest price found across retailers. During certain sales while building this dashboard, ", tags$b("the largest difference has ranged from
                                          $500 to even almost $900 cheaper than Movado's listed price."))),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"),
                              width = 12, title = "Largest Discount", value = paste0("$", max(prices_df$difference, na.rm = TRUE)),
                              subtitle = paste0(ifelse(max_diff$price_macys + max_diff$difference == max_diff$price_movado, "Macy's",
                                     ifelse(max_diff$price_nordstrom + max_diff$difference == max_diff$price_movado, "Nordstrom", "Amazon")), ", ",
                              max_diff[["watch_model"]]),
                              href = ifelse(min(max_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == max_diff$price_macys, macys_df[macys_df$model_number == max_diff$model_number, "url"],
                              ifelse(min(max_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == max_diff$price_nordstrom, nordstrom_df[nordstrom_df$model_number == max_diff$model_number, "url"],
                                     amazon_df[amazon_df$model_number == max_diff$model_number, "url"]))
                      ),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"),
                              width = 12, title = "2nd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[2]),
                              subtitle = paste0(ifelse(sec_diff$price_macys + sec_diff$difference == sec_diff$price_movado, "Macy's",
                                                      ifelse(sec_diff$price_nordstrom + sec_diff$difference == sec_diff$price_movado, "Nordstrom", "Amazon")), ", ",
                                               sec_diff[["watch_model"]]),
                              href = paste0(ifelse(min(sec_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == sec_diff$price_macys, macys_df[macys_df$model_number == sec_diff$model_number, "url"],
                                            ifelse(min(sec_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == sec_diff$price_nordstrom, nordstrom_df[nordstrom_df$model_number == sec_diff$model_number, "url"],
                                                   amazon_df[amazon_df$model_number == sec_diff$model_number, "url"])))
                      ),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"),
                              width = 12, title = "3rd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[3]),
                              subtitle = paste0(ifelse(thrd_diff$price_macys + thrd_diff$difference == thrd_diff$price_movado, "Macy's",
                                                      ifelse(thrd_diff$price_amazon + thrd_diff$difference == thrd_diff$price_movado, "Amazon", "Nordstrom")), ", ",
                                               thrd_diff[["watch_model"]]),
                              href = ifelse(min(thrd_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == thrd_diff$price_nordstrom, nordstrom_df[nordstrom_df$model_number == thrd_diff$model_number, "url"],
                                            ifelse(min(thrd_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == thrd_diff$price_macys, macys_df[macys_df$model_number == thrd_diff$model_number, "url"],
                                                   amazon_df[amazon_df$model_number == thrd_diff$model_number, "url"]))
                            )
                      ),
                      column(width = 8, box(width = 12, DT::dataTableOutput("differences_table"))),
                      fluidRow(
                        box(solidHeader = TRUE, width = 12, title = "A Note About Availability", collapsible = TRUE, collapsed = FALSE, background = "navy",
                            box(background= "navy", 
                                fluidRow(valueBox(value = length(unique(full_prices_df[full_prices_df$in_stock == "In Stock" & 
                                                                                is.na(full_prices_df["price_macys"]) & 
                                                                                is.na(full_prices_df["price_nordstrom"]) & 
                                                                                is.na(full_prices_df["price_amazon"]), "model_number"])), 
                                         subtitle = "Only from Movado.com")), br(), br(), 
                                fluidRow(valueBox(color = "yellow", value = length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | 
                                                                                 is.na(full_prices_df$in_stock)) & 
                                                                                is.na(full_prices_df["price_macys"]) & 
                                                                                is.na(full_prices_df["price_amazon"]), "model_number"])),
                                         subtitle = "Only from Nordstrom.com"),
                                valueBox(color = "yellow", value = length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | 
                                                                                 is.na(full_prices_df$in_stock)) & 
                                                                                is.na(full_prices_df["price_nordstrom"]) & 
                                                                                is.na(full_prices_df["price_amazon"]), "model_number"])),
                                         subtitle = "Only from Macys.com"), 
                                valueBox(color = "yellow", value = length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | 
                                                                                 is.na(full_prices_df$in_stock)) & 
                                                                                is.na(full_prices_df["price_macys"]) & 
                                                                                is.na(full_prices_df["price_nordstrom"]), "model_number"])),
                                         subtitle = "Only from Amazon.com")), br(), br(), 
                                fluidRow(valueBox(value = length(unique(full_prices_df[full_prices_df$in_stock == "In Stock" & 
                                                                                -(is.na(full_prices_df["price_macys"]) & 
                                                                                    is.na(full_prices_df["price_nordstrom"]) &
                                                                                    is.na(full_prices_df["price_amazon"])), "model_number"])),
                                         subtitle = "Movado and At Least 1 Retailer"), 
                                valueBox(color = "red", value = length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | 
                                                                                 is.na(full_prices_df$in_stock)) & 
                                                                                !(is.na(full_prices_df["price_macys"]) & 
                                                                                    is.na(full_prices_df["price_nordstrom"]) &
                                                                                    is.na(full_prices_df["price_amazon"])), "model_number"])),
                                         subtitle = "Only from Retailers")), 
                                ),
                            box(sankeyNetworkOutput("sankey_avail"))),
                      )
                    ),
                    h3("Discounts by Retailer"),
                    fluidRow(
                      column(width = 6, box(width = 12,
                                            p("We can also look at average savings by retailer. While sales come and go, ", tags$b("there are a total of ",
                                              as.character(length(unique(full_prices_df[full_prices_df$difference == 0 & full_prices_df$in_stock == "In Stock" & !(is.na(full_prices_df["price_macys"])
                                                                                                        & is.na(full_prices_df["price_nordstrom"]) &
                                                                                                          is.na(full_prices_df["price_amazon"])), "model_number"]))),
                                              " products that Movado.com sells that can be found only at full price on retailers' sites."),
                                              "For the products whose prices do not match Movado.com, their average savings are
                                              captured in the value boxes to the right. ", br(), br(), h4("Amazon's Discount"),
                                              "While Amazon can often be a bargain site, surprisingly Amazon's average discount has been lower than the other two retailers. 
                                              That is not the case after this most recent scrape, however, as Macy's ended a large blowout sale and moved  most products back 
                                              to full retail price. Amazon's lower discounts may be due to the fact that Macy's and Nordstrom post large sales of 20% - 30% off 
                                              merchandise and can handle lowering their profit in turn for moving items off shelves
                                              more than maybe Amazon's vendors can weather. More important, though, as seen in the above graph
                                              illustrating densities of prices (Tab #2, 'Price Densities'), ", tags$b(" Amazon
                                              lists more lower priced items, which limit the size of the discount.")))),
                      column(width = 6,
                             valueBox(color = "yellow", width = 8, subtitle = "Average Macy's Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[!is.na(prices_df$price_macys), "price_movado"] - prices_df[!is.na(prices_df$price_macys), "price_macys"]), 2),
                                              " (", round(sum(prices_df[!is.na(prices_df$price_macys), "price_movado"] - prices_df[!is.na(prices_df$price_macys), "price_macys"]) 
                                                          * 100/sum(prices_df[!is.na(prices_df$price_macys), "price_movado"]), 2), "%)")
                                    ),
                            valueBox(color = "yellow", width = 8, subtitle = "Average Amazon Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[!is.na(prices_df$price_amazon), "price_movado"] - prices_df[!is.na(prices_df$price_amazon), "price_amazon"]), 2),
                                              " (", round(sum(prices_df[!is.na(prices_df$price_amazon), "price_movado"] - prices_df[!is.na(prices_df$price_amazon), "price_amazon"])
                                                          * 100/sum(prices_df[!is.na(prices_df$price_amazon), "price_movado"]), 2), "%)")
                                    ),
                            valueBox(color = "yellow", width = 8, subtitle = "Average Nordstrom Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[!is.na(prices_df$price_nordstrom), "price_movado"] - prices_df[!is.na(prices_df$price_nordstrom), "price_nordstrom"]), 2), 
                                              " (", round(sum(prices_df[!is.na(prices_df$price_nordstrom), "price_movado"] - prices_df[!is.na(prices_df$price_nordstrom), "price_nordstrom"])
                                                          * 100/sum(prices_df[!is.na(prices_df$price_nordstrom), "price_movado"]), 2), "%)")
                                    )
                      )
                    ),
                    h2("Reviews"),
                    fluidRow(
                      tabBox(width = 7,  tabPanel("Products Left Unreviewed", plotOutput("missing_reviews")),
                             tabPanel("Word Count Densities", plotOutput("word_count")),
                             tabPanel("Average Word Count", plotOutput("avg_word_count"))),
                      box(width = 5, p(h4("Products Left Unreviewed"), "While Macy's and Nordstrom hover around 50% customer interaction for reviewing, ",
                                       tags$b("Amazon leads with over 75% customer engagement post-purchase."), br(), br(),  h4("Word Count Densities"), "In terms of time
                                       or thought spent reviewing a product, we can approximate dedicated customer engagement, or customers who are
                                        willing to spend some of their time writing well-rounded and insightful reviews,
                                       through word count. Yet surprisingly, there is a remarkably similar distribution of word counts across
                                       Nordstrom and Macy's. This might tell us that a very similar make-up of customers are likely to shop and
                                       review at both Macy's or Nordstrom, that the two shopping populations are rather similar.", br(), br(),
                                       h4("Average Word Count"), "When including all Movado products sold on respective retailer websites,
                                       both those with and without reviews, ", tags$b("Nordstrom holds the edge just slightly above Macy's with
                                                                                      about three more words on average per review written.")
                                       ))
                    ),
                    h3("Frequent Review Words"),
                    fluidRow(
                      column(width = 6, align = "center", box(background = "navy", width = 12, imageOutput("review_word_cloud"))),
                      box(width = 4, "The word cloud to the left breaks down the popular words found in reviews. 
                      Some of the more frequent words are 'beautiful' and 'love'. This is somewhat expected as we saw most of the ratings for all three 
                          retailers are very positive.")
                    ),
                    h2("Product Descriptions"),
                    fluidRow(
                      box(width = 4, h4("Incorrect Case Diameter"), p("While working with the data, we noticed that Nordstrom had some conflicting 
                                                                      measurements when it came to case diameter. The title would list one size, 
                                                                      while the product description listed a different size. On the first tab,  
                                                                      the specific products with conflicting measurements are listed."),br(), 
                          h4("Vague Product Names"), p("In addition to having incorrect information, Nordstrom's listings for Movado products 
                                                       are often extremely vague in the item's title. We were able to extract the collection each product 
                                                       is a part of for some listings, yet for many others it was impossible. 'Movado Connect 2.0' 
                                                       watches are titled as 'Connect Chronograph' watches or 'Connect 2.0 Glitz Mesh Band'. And titles like 'Bold Bracelet 
                                                       Watch, 34mm' could describe a watch in almost any of the multiple 'Movado BOLD' collections. ", 
                                                       tags$b("Almost half of all watches listed by Nordstrom have too vague a name to identify of which collection 
                                                              the product is a part.")), br(),
                          h4("Vague Descriptions"), p("Lastly, worse than listing products under vague titles,  Nordstrom does not provide adequate descriptions 
                                                      to aid in identifying which product is being listed. For example, ", tags$b("none of the watches from the 'Movado BOLD Thin' 
                                                      collection are described as being a ", tags$i("thin"), "watch"), " on Nordstrom, nor is there any distinction made 
                                                      between watches with a mesh bracelet versus a pyramid mesh. And almost all watches are described as having a 'Museum dot', 
                                                      leading some of our early classification efforts astray when rarely were the products actually a part of one of the 
                                                      Museum collections.")),
                      tabBox(width = 8, tabPanel("Incorrect Measurements", DT::dataTableOutput("incorrect_nordstrom")),
                             tabPanel("Vague Product Names", DT::dataTableOutput("vague_nordstrom")),
                             tabPanel("Vague Descriptions", DT::dataTableOutput("vague_description_nordstrom"))
                      )
                    )
          )
          )
        )
      )
  )
)