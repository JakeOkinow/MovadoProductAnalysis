
shinyUI(
  dashboardPage(
      dashboardHeader(title="MOVADO"),
      dashboardSidebar(
          sidebarMenu(
            menuItem("Movado Insight", tabName = "movado", icon = icon("info")),
            menuItem("Retailers",
              menuSubItem("Overview", tabName = "overview"),
              menuSubItem("Macy's", tabName = "macys"),
              menuSubItem("Nordstrom", tabName = "nordstrom"),
              menuSubItem("Amazon", tabName = "amazon")
                    ),
            menuItem("Competitors",
                     menuSubItem("Tag Heuer", tabName = "tag_heuer"),
                     menuSubItem("Other", tabName = "other")
                     )
          )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "movado",
                  fluidPage(
                    h2("Price Distribution"),
                    fluidRow(box(width = 6, plotOutput("movado_price")), 
                             column(width = 6, infoBox(width = 10, title = "Average Price", 
                                                       value = paste0("$", round(mean(movado_df$price), 2))),
                                    infoBox(width = 10, title = "Median Price", 
                                            value = paste0("$", round(median(movado_df$price), 2)))
                                    )
                            )
                    )
                  ),
          tabItem(tabName = "overview",
                  fluidPage(
                    h1(tags$b("Movado Compared to Retailers")),
                    h2("Overall Pricing"), br(),
                    fluidRow(box(width = 6, "Amazon tends to have lower priced products, while Macy's and Movado both have most 
                                 frequently products priced at $695. However, Macy's average product cost is higher 
                                 than Movado's. Further investigation will determine if this is because of a tendency 
                                 to carry more of the higher priced watches of Movado's."), 
                             box(plotOutput("overview_price"))
                    ),
                    h2("Individual Product Pricing"),
                    h3("Largest Price Discrepancies"),
                    fluidRow(
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"), 
                              width = 4, title = "Largest Discount", value = paste0("$", max(prices_df$difference, na.rm = TRUE)),
                              subtitle = paste(ifelse(min(max_diff[c("price", "price_macys")], na.rm = TRUE) == ifelse(is.na(max_diff[["price"]]), 0, max_diff[["price"]]), "Amazon,", "Macy's,"), max_diff[["watch_model_movado"]]), 
                              href = ## ADD URL TO MACYS NORDSTROM AND AMAZON
                                ),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"), 
                              width = 4, title = "2nd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[2]),
                              subtitle = paste(ifelse(min(sec_diff[c("price", "price_macys")], na.rm = TRUE) == ifelse(is.na(sec_diff[["price"]]), 0, sec_diff[["price"]]), "Amazon,", "Macy's,"), sec_diff[["watch_model_movado"]]), 
                              href = ## ADD URL TO MACYS NORDSTROM AND AMAZON
                              ),
                      infoBox(fill = TRUE, color = "red", icon = icon("exclamation"), 
                              width = 4, title = "3rd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[3]),
                              subtitle = paste(ifelse(min(thrd_diff[c("price", "price_macys")], na.rm = TRUE) == ifelse(is.na(thrd_diff[["price"]]), 0, thrd_diff[["price"]]), "Amazon,", "Macy's,"), thrd_diff[["watch_model_movado"]]), 
                              href = ## ADD URL TO MACYS NORDSTROM AND AMAZON
                              )
                          ),
                    h3("Discrepancies by Retailer"),
                    fluidRow(
                      valueBox(color = "yellow", width = 4, subtitle = "Average Macy's Discount", icon = icon('tags'),
                               value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price_macys"], na.rm = TRUE), 2))
                              ),
                      valueBox(color = "yellow", width = 4, subtitle = "Average Amazon Discount", icon = icon('tags'),
                             value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price"], na.rm = TRUE), 2))
                              ),
                      valueBox(color = "yellow", width = 4, subtitle = "Average Nordstrom Discount", value = 0, icon = icon('tags'),
                               #value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price"], na.rm = TRUE), 2))
                              )
                          ),
                    selectInput("select_model", label = "Select Model:", choices = sort(unique(prices_df$watch_model_movado))),
                    fluidRow(box(width = 6, height=400, title = "Mean Prices Per Retailer", htmlOutput("price_graph")),
                            box(width = 6, height=400, title = "Price Against Frequency from a Seller", htmlOutput("price_bubble"))
                    ),
                    selectizeInput("select_prod_num", label = "Select Product Number:", choices = sort(unique(prices_df$model_number))),
                    fluidRow(box(width = 6, title = "Retailers' Price by Product Number", htmlOutput("prod_num_graph")),
                             infoBoxOutput("selected_prod_num"))
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
                    fluidRow(dataTableOutput("seller_table")),
                    h2("Watch list and number of sellers"), br(),
                    fluidRow(dataTableOutput("watches_table"))
                  )
                  ),
          tabItem(tabName = "tag_heuer", "competition 1"
                  ),
          tabItem(tabName = "other", "competition 2")
        )
      )
  )
)