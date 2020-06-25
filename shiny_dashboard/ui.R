
shinyUI(
  dashboardPage(
      dashboardHeader(title="MOVADO"),
      dashboardSidebar(
          sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Retailers",
              menuSubItem("Macy's", tabName = "macys"),
              menuSubItem("Nordstrom", tabName = "nordstrom"),
              menuSubItem("Amazon", tabName = "amazon")
                    ),
            menuItem("Insights", tabName = "insights")
          )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "home",
                  fluidPage(
                    fluidRow(column(width = 8, offset=2, align = "center", 
                                    box(width = 12, background = "black", align = "center", h1(tags$b("MOVADO INSIGHTS\n"), style = "font-size:55px;"),
                                                    h2(tags$b("A Look into Retailers and Resellers")),
                                                    br(), #img(src = "", style="width: 100%"),
                                                    h5("Research and modeling by Victoria Lowery and Jake Okinow")))),
                    fluidRow(column(width = 10, offset=1, align="center", box(height = 220, width = 12, align = "center",  background = "black",
                                 h2("Total Number of Products Scraped"),
                                 column(width = 3, flexdashboard::gaugeOutput("gauge_movado")), 
                    column(width = 3, flexdashboard::gaugeOutput("gauge_macys")), 
                    column(width = 3, flexdashboard::gaugeOutput("gauge_nordstrom")),
                    column(width = 3, flexdashboard::gaugeOutput("gauge_amazon")))
                  )),
                  fluidRow(column(width = 10, offset=1, align="center", box(height = 220, width = 12, align = "center", background = "black",
                               h2("Total Number of Reviews Scraped"),
                               column(width = 3, flexdashboard::gaugeOutput("gauge_movado_r")), 
                               column(width = 3, flexdashboard::gaugeOutput("gauge_macys_r")), 
                               column(width = 3, flexdashboard::gaugeOutput("gauge_nordstrom_r")),
                               column(width = 3, flexdashboard::gaugeOutput("gauge_amazon_r")))
                  )), 
                  fluidRow(column(width = 10, offset=1, align="center", box(height = 220, width = 12, align = "center", background = "black",
                               h2("Average Ratings"),
                               column(width = 3, flexdashboard::gaugeOutput("gauge_movado_s")), 
                               column(width = 3, flexdashboard::gaugeOutput("gauge_macys_s")), 
                               column(width = 3, flexdashboard::gaugeOutput("gauge_nordstrom_s")),
                               column(width = 3, flexdashboard::gaugeOutput("gauge_amazon_s")))
                  ))
                  )
                  ),
          tabItem(tabName = "macys",
                  fluidPage(
                    h1(tags$b("Macy's Performance")),
                    h2("Customer Satisfation: Ratings Distribution"), br(),
                    fluidRow(
                      infoBox(width = 4, color = "yellow", title = "Total Unrated Products", value = sum(is.na(macys_df$rating)),
                              subtitle = paste0(round(100*sum(is.na(macys_df$rating))/nrow(macys_df)), "% of products listed on Macy's")),
                      infoBox(width = 4, title="Total 100%-Scoring Products", 
                              subtitle = paste0(round(100*macys_5_rating/nrow(macys_df)), "% of products listed on Macy's"), 
                              fill = TRUE, value = macys_5_rating)),
                      fluidRow(
                        box(p("Almost half of Macy's products have received a review, and those with ratings seem heavily right skewed 
                                      to being in the 90%-100% satisfaction range.", br(), br(), tags$b(paste0("In fact, ", round(100*macys_5_rating/nrow(macys_df)), "% of all Movado 
                                      products listed on Macy's.com are rated 100% satisfaction.")), style = "font-size:25px;")),
                        box(width = 6, plotOutput("macys_stars"))
                             ),
                    h2("Reviews: Presence and Distribution"), br(),
                    fluidRow(
                             column(width = 6, infoBox(width = 12, fill = TRUE, title = "Most Reviewed Product", value = macys_df[max(macys_df["review_count"]), "watch_model"],
                                                       subtitle = paste(max(macys_df["review_count"]), "total reviews")),
                                    box(width = 12, p("The number of products without reviews received is identical to the number of products without ratings, 
                                                      possibly implying that Macy's does not allow customers to rate a product without leaving a review. A  
                                                      word count of reviews written show that most do not exceed {FIND AVERAGE WORD COUNT OF MACY'S REVIEW}.", 
                                                      br(), br(), "We can also see in the graph below on the right that", tags$b(" of the products receiving reviews, nearly half 
                                                      of those only receive one review."), style = "font-size:25px;") )
                                    ), 
                             column(width = 6, box(align = "center", width = 12, htmlOutput("macys_zero_reviews_pie")), 
                             box(width = 12, plotOutput("macys_review_count_sans_0")))
                             ),
                    h2("Price Distribution"),
                    fluidRow(box(width = 8, htmlOutput("combo_macys_price"), height = 460), 
                            column(width = 4, infoBox(width = 12, title = "Average Price", 
                                                      value = paste0("$", round(mean(macys_df$price), 2))),
                                   infoBox(width = 12, title = "Median Price", 
                                           value = paste0("$", round(median(macys_df$price), 2)))
                                    )
                            )
                      )
                  ),
          tabItem(tabName = "nordstrom",
                  fluidPage(
                    h1(tags$b("Nordstrom's Performance")),
                    h2("Customer Satisfation: Ratings Distribution"), br(),
                    fluidRow(box(width = 6, plotOutput("nordstrom_stars")), 
                             column(width = 6, infoBox(width = 10, title="Total 5-Star-Rated Products", 
                                                       subtitle = paste0(round(100*nordstrom_5_rating/nrow(nordstrom_df)), "% of Movado products"), 
                                                       fill = TRUE, value = nordstrom_5_rating),
                                    infoBox(width = 10, color = "yellow", title = "Total Unrated Products", value = sum(is.na(nordstrom_df$rating)),
                                            subtitle = paste0(round(100*sum(is.na(nordstrom_df$rating))/nrow(nordstrom_df)), "% of Movado products"))
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
                             box(width = 7, plotOutput("nordstrom_review_count_sans_0"))),
                    h2("Price Distribution"),
                    fluidRow(box(width = 6, plotOutput("nordstrom_price")), 
                             column(width = 6, infoBox(width = 10, title = "Average Price", 
                                                       value = paste0("$", round(mean(nordstrom_df$price), 2))),
                                    infoBox(width = 10, title = "Median Price", 
                                            value = paste0("$", round(median(nordstrom_df$price), 2)))
                                    )
                            )
                      )
                  ),
          tabItem(tabName = "amazon", 
                  fluidPage(
                    h1(tags$b("Amazon's Performance")),
                    h2("Customer Satisfation: Ratings Distribution"), br(),
                    fluidRow(box(width = 6, plotOutput("amazon_stars")),
                             column(width = 6, infoBox(width = 10, title="Total 5-Star-Rated Products",
                                                       subtitle = paste0(round(100*sum(amazon_d_df$star == 5.0)/nrow(amazon_d_df)), "% of products listed on Amazon's"),
                                                       fill = TRUE, value = sum(amazon_d_df$star == 5.0)),
                                    infoBox(width = 10, color = "yellow", title = "Total Unrated Products", value = sum(amazon_d_df$star == 0.0),
                                            subtitle = paste0(round(100*sum(amazon_d_df$star == 0.0)/nrow(amazon_d_df)), "% of products listed on Amazon's"))
                             )
                    ),
                    h2("Reviews: Presence and Distribution"), br(),
                    fluidRow(box(width = 4, p("text text text text text text text")),
                             column(width = 8, infoBox(width = 12, fill = TRUE, title = "Most Reviewed Product", value = amazon_df$product[which.max(amazon_df$rev_count)],
                                                       subtitle = paste(max(amazon_d_df$rev_count), "total reviews"))
                             )
                    ),
                    fluidRow(box(align = "center", width = 5, htmlOutput("amazon_zero_reviews_pie")), 
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
                             dataTableOutput("seller_table")),
                    h2("Watch list and number of sellers"), br(),
                    fluidRow(selectInput("product_var", "Variable:",
                                         p_list),
                             plotOutput("watches_price"),
                             dataTableOutput("watches_table")),
                    h2("Full Table"), br(),
                    fluidRow(
                      dataTableOutput("full_table"))
                  )
          ),
          
          tabItem(tabName = "insights",
                  fluidPage(
                    h1(tags$b("Movado Compared to Retailers")),
                    h2("Pricing"), br(),
                    fluidRow(box(width = 6, "Amazon tends to have lower priced products, while Macy's and Movado both have most 
                                 frequently products priced at $695. However, Macy's average product cost is higher 
                                 than Movado's. Further investigation will determine if this is because of a tendency 
                                 to carry more of the higher priced watches of Movado's."), 
                             tabBox(tabPanel("Mean/Median Price", plotOutput("overview_price")),
                                    tabPanel("Price Densities", plotOutput("price_density"))
                             )
                    ),
                    h2("Discounts"),
                    fluidRow(column(width = 4, 
                                    box(width = 12, p("To the left is a table combining all of Movado.com's products to those found on Nordstrom's, Macy's, and Amazon's websites.
                                          The table can be organized by model number, model title, or seller's prices.", br(), br(), "The final column is the difference between 
                                          Movado.com's price and the lowest price found across retailers. When organized by differences in descending order, the 
                                          most discounted items can be found. During certain sales while building this dashboard, ", tags$b("the largest difference has ranged from 
                                          $600 to even almost $900 cheaper than Movado's listed price."))),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"), 
                              width = 12, title = "Largest Discount", value = paste0("$", max(prices_df$difference, na.rm = TRUE)),
                              subtitle = paste0(ifelse(max_diff$price_macys + max_diff$difference == max_diff$price_movado, "Macy's", 
                                                      ifelse(max_diff$price_amazon + max_diff$difference == max_diff$price_movado, "Amazon", "Nordstrom")), ", ",
                                               max_diff[["watch_model"]]), 
                              #href = ## ADD URL TO MACYS NORDSTROM AND AMAZON
                      ),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"), 
                              width = 12, title = "2nd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[2]),
                              subtitle = paste0(ifelse(sec_diff$price_macys + sec_diff$difference == sec_diff$price_movado, "Macy's", 
                                                      ifelse(sec_diff$price_amazon + sec_diff$difference == sec_diff$price_movado, "Amazon", "Nordstrom")), ", ",
                                               sec_diff[["watch_model"]]),
                              #href = ## ADD URL TO MACYS NORDSTROM AND AMAZON
                      ),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"), 
                              width = 12, title = "3rd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[3]),
                              subtitle = paste0(ifelse(thrd_diff$price_macys + thrd_diff$difference == thrd_diff$price_movado, "Macy's", 
                                                      ifelse(thrd_diff$price_nordstrom + thrd_diff$difference == thrd_diff$price_movado, "Nordstrom", "Amazon")), ", ", 
                                               thrd_diff[["watch_model"]]),
                              href = ifelse(min(thrd_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == thrd_diff$price_nordstrom, nordstrom_df[nordstrom_df$model_number == thrd_diff$model_number, "url"],
                                            ifelse(min(thrd_diff[c("price_amazon", "price_macys", "price_nordstrom")], na.rm = TRUE) == thrd_diff$price_macys, macys_df[macys_df$model_number == thrd_diff$model_number, "url"],
                                                   amazon_df[amazon_df$model_number == thrd_diff$model_number, "url"]))
                      )), 
                      column(width = 8, box(width = 12, DT::dataTableOutput("differences_table")))
                    ),
                    h3("Discounts by Retailer"),
                    fluidRow(
                      column(width = 6, box(width = 12, 
                                            p("We can also look at average savings by retailer. ", tags$b("While sales come and go, there are a total of ", 
                                              as.character(nrow(prices_df[prices_df$difference == 0 & !(is.na(prices_df["price_macys"]) 
                                                                                                        & is.na(prices_df["price_nordstrom"]) & 
                                                                                                          is.na(prices_df["price_amazon"])), ])), 
                                              " products that Movado.com sells where there is not a discounted price from Amazon, Nordstrom, or Macy's."), 
                                              "For the products whose prices do not match Movado.com, their average savings are 
                                              captured in the value boxes to the right. ", br(), br(), "On Amazon, the average discount one can find is", 
                                              paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price_amazon"], na.rm = TRUE), 2), "."), 
                                              "While Amazon can often be a bargain site, surprisingly Amazon's average discount is lower than the other two retailers.
                                              This may be that Macy's and Nordstrom post large sales of 20% - 30% off merchandise and can 
                                              handle lowering their profit in turn for moving items off shelves 
                                              more than maybe Amazon's vendors can weather. More notably, though, as seen in the above graph 
                                              illustrating densities of prices (Tab #2, 'Price Densities'), ", tags$b(" Amazon 
                                              lists more lower priced items, which limit the size of the discount.")))),
                      column(width = 6, 
                             valueBox(color = "yellow", width = 8, subtitle = "Average Macy's Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price_macys"], na.rm = TRUE), 2))
                                    ),
                            valueBox(color = "yellow", width = 8, subtitle = "Average Amazon Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price_amazon"], na.rm = TRUE), 2))
                                    ),
                            valueBox(color = "yellow", width = 8, subtitle = "Average Nordstrom Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price_nordstrom"], na.rm = TRUE), 2))
                                    )
                      )
                    ),
                    h2("Reviews"),
                    fluidRow(
                      box(width = 8, plotOutput("word_count")),
                      box(width = 4, p("Text about word count of products"))
                    ),
                    
                    # selectInput("select_model", label = "Select Model:", choices = sort(unique(prices_df$watch_model))),
                    # fluidRow(box(width = 6, height=400, title = "Mean Prices Per Retailer", plotOutput("price_graph")),
                    # ),
                    selectizeInput("select_prod_num", "Select Product Number:", a_list),
                    fluidRow(box(title = "Retailers' Price by Product Number", plotOutput("prod_num_graph")),
                             dataTableOutput("selected_prod_num"))
                  )
          )
        )
      )
  )
)