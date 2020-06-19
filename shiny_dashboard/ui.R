
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
                    h2("Pricing"), br(),
                    fluidRow(box(width = 6, "Amazon tends to have lower priced products, while Macy's and Movado both have most 
                                 frequently products priced at $695. However, Macy's average product cost is higher 
                                 than Movado's. Further investigation will determine if this is because of a tendency 
                                 to carry more of the higher priced watches of Movado's."), 
                             box(plotOutput("overview_price"))
                    ),
                    h2("Customer Satisfaction")
                    
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
          ####AMAZON COPY PASTE######
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
          ), #AMAZON COPY PASTE
          tabItem(tabName = "tag_heuer", "competition 1"
                  ),
          tabItem(tabName = "other", "competition 2")
        )
      )
  )
)