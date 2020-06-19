
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
                      infoBox(fill = TRUE, color = "red", icon = icon("danger"), width = 4, title = "Largest Discount", value = paste0("$", max(prices_df$difference, na.rm = TRUE)),
                                              subtitle = paste(ifelse(min(max_diff[c("price", "price_macys")]) == max_diff[["price"]], "Amazon,", "Macy's,"), max_diff[["watch_model_movado"]])),
                      infoBox(fill = TRUE, color = "red", icon = icon("danger"), width = 4, title = "2nd Largest Discount", value = paste0("$", sort(prices_df$difference, decreasing=TRUE)[2]),
                                                subtitle = paste(ifelse(min(sec_diff[c("price", "price_macys")]) == sec_diff[["price"]], "Amazon,", "Macy's,"), sec_diff[["watch_model_movado"]])),
                          ),
                    h3("Discrepancies by Retailer"),
                    fluidRow(
                      valueBox(color = "yellow", width = 4, subtitle = "Average Macy's Discount", 
                               value = paste0("$", mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price_macys"], na.rm = TRUE))
                              ),
                      valueBox(color = "yellow", width = 4, subtitle = "Average Amazon Discount", 
                             value = paste0("$", round(mean(prices_df[prices_df$difference !=0, "price_movado"] - prices_df[prices_df$difference !=0, "price"], na.rm = TRUE), 2))
                              )
                          ),
                    selectInput("select_model", label = "Select Model:", choices = sort(unique(prices_df$watch_model_movado))),
                    box(width=12, htmlOutput("price_graph"))
                  )
          ),
          tabItem(tabName = "macys",
                  fluidPage(
                    h1(tags$b("Macy's Performance")),
                    h2("Customer Satisfation: Ratings Distribution"), br(),
                    fluidRow(box(width = 6, plotOutput("macys_stars")), 
                             column(width = 6, infoBox(width = 10, title="Total 5-Star-Rated Products", 
                                                       subtitle = paste0(round(100*macys_5_rating/nrow(macys_df)), "% of products listed on Macy's"), 
                                                       fill = TRUE, value = macys_5_rating),
                                    infoBox(width = 10, color = "yellow", title = "Total Unrated Products", value = sum(is.na(macys_df$rating)),
                                            subtitle = paste0(round(100*sum(is.na(macys_df$rating))/nrow(macys_df)), "% of products listed on Macy's"))
                                    )
                             ),
                    h2("Reviews: Presence and Distribution"), br(),
                    fluidRow(box(width = 4, p("text text text text text text text")), 
                             column(width = 8, infoBox(width = 12, fill = TRUE, title = "Most Reviewed Product", value = macys_df[max(macys_df["review_count"]), "watch_model"],
                                                       subtitle = paste(max(macys_df["review_count"]), "total reviews")) 
                                    )
                             ),
                    fluidRow(box(align = "center", width = 5, htmlOutput("macys_zero_reviews_pie")), 
                             box(width = 7, plotOutput("macys_review_count_sans_0"))),
                    h2("Price Distribution"),
                    fluidRow(box(width = 6, plotOutput("macys_price")), 
                            column(width = 6, infoBox(width = 10, title = "Average Price", 
                                                      value = paste0("$", round(mean(macys_df$price), 2))),
                                   infoBox(width = 10, title = "Median Price", 
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
                    h2("Price Distribution"),
                    fluidRow(box(width = 6, plotOutput("amazon_price")), 
                             column(width = 6, infoBox(width = 10, title = "Average Price", 
                                                       value = paste0("$", round(mean(amazon_df$price), 2))),
                                    infoBox(width = 10, title = "Median Price", 
                                            value = paste0("$", round(median(amazon_df$price), 2)))
                                    )
                            ),
                    h2("Top Quatity Sellers: Sellers with > # Listings")
                  )
                  ),
          tabItem(tabName = "tag_heuer", "competition 1"
                  ),
          tabItem(tabName = "other", "competition 2")
        )
      )
  )
)