
function(input, output, session){

    # HOME
  output$gauge_movado <- flexdashboard::renderGauge(
    flexdashboard::gauge(nrow(movado_df), label = "Movado.com", min = 0, max = 400, 
          flexdashboard::gaugeSectors(success = c(250, 400), 
                                 warning = c(100, 250),
                                 danger = c(0, 100)))
  )
  
  output$gauge_macys <- flexdashboard::renderGauge(
    flexdashboard::gauge(nrow(macys_df), label = "Macys.com", min = 0, max = 400, 
                         flexdashboard::gaugeSectors(success = c(250, 400), 
                                                     warning = c(100, 250),
                                                     danger = c(0, 100)))
  )
  
  output$gauge_nordstrom <- flexdashboard::renderGauge(
    flexdashboard::gauge(nrow(nordstrom_df), label = "Nordstrom.com", min = 0, max = 400, 
                         flexdashboard::gaugeSectors(success = c(250, 400), 
                                                     warning = c(100, 250),
                                                     danger = c(0, 100)))
  )
  
  output$gauge_amazon <- flexdashboard::renderGauge(
    flexdashboard::gauge(length(unique(amazon_df$model_number)), label = "Amazon.com", min = 0, max = 400, 
                         flexdashboard::gaugeSectors(success = c(250, 400), 
                                                     warning = c(100, 250),
                                                     danger = c(0, 100)))
  )
  
  output$gauge_movado_r <- flexdashboard::renderGauge(
    flexdashboard::gauge(0, label = "Movado.com", min = 0, max = 1500, 
                         flexdashboard::gaugeSectors(success = c(1000, 1500), 
                                                     warning = c(500, 1000),
                                                     danger = c(0, 500)))
  )
  
  output$gauge_macys_r <- flexdashboard::renderGauge(
    flexdashboard::gauge(sum(macys_df$review_count), label = "Macys.com", min = 0, max = 1500, 
                         flexdashboard::gaugeSectors(success = c(1000, 1500), 
                                                     warning = c(500, 1000),
                                                     danger = c(0, 500)))
  )
  
  output$gauge_nordstrom_r <- flexdashboard::renderGauge(
    flexdashboard::gauge(sum(nordstrom_df$review_count), label = "Nordstrom.com", min = 0, max = 1500, 
                         flexdashboard::gaugeSectors(success = c(1000, 1500), 
                                                     warning = c(500, 1000),
                                                     danger = c(0, 500)))
  )
  
  output$gauge_amazon_r <- flexdashboard::renderGauge(
    flexdashboard::gauge(sum(unique(amazon_df$rev_count)), label = "Amazon.com", min = 0, max = 1500, 
                         flexdashboard::gaugeSectors(success = c(1000, 1500), 
                                                     warning = c(500, 1000),
                                                     danger = c(0, 500)))
  )
  
  output$gauge_movado_s <- flexdashboard::renderGauge(
    flexdashboard::gauge(0, label = "Movado.com", min = 0, max = 5, 
                         flexdashboard::gaugeSectors(success = c(4, 5), 
                                                     warning = c(3, 4),
                                                     danger = c(1, 3)))
  )
  
  output$gauge_macys_s <- flexdashboard::renderGauge(
    flexdashboard::gauge(round(mean(macys_df$rating, na.rm=TRUE)/20, 2), label = "Macys.com", min = 0, max = 5, 
                         flexdashboard::gaugeSectors(success = c(4, 5), 
                                                     warning = c(3, 4),
                                                     danger = c(1, 3)))
  )
  
  output$gauge_nordstrom_s <- flexdashboard::renderGauge(
    flexdashboard::gauge(round(mean(nordstrom_df$rating, na.rm=TRUE), 2), label = "Nordstrom.com", min = 0, max = 5, 
                         flexdashboard::gaugeSectors(success = c(4, 5), 
                                                     warning = c(3, 4),
                                                     danger = c(1, 3)))
  )
  
  output$gauge_amazon_s <- flexdashboard::renderGauge(
    flexdashboard::gauge(round(mean(amazon_df$star, na.rm=TRUE), 2), label = "Amazon.com", min = 0, max = 5, 
                         flexdashboard::gaugeSectors(success = c(4, 5), 
                                                     warning = c(3, 4),
                                                     danger = c(1, 3)))
  )
  
  # INSIGHT
  output$overview_price <- renderPlot({
    colors = c("Movado" = "#8e0000", "Macy's" = "#171b64", "Nordstrom" = "#9ec2e1", "Amazon" = "#e5a0a0")
    data.frame("Mean Prices" = c(round(mean(movado_df$price), 2), round(mean(macys_df$price), 2), 
                            round(mean(nordstrom_df$price), 2), round(mean(amazon_df$price), 2)),
          "Median Prices" = c(round(median(movado_df$price), 2), round(median(macys_df$price), 2), 
                              round(median(nordstrom_df$price), 2), round(median(amazon_df$price), 2)),
          "Seller"=c("Movado", "Macy's", "Nordstrom", "Amazon")) %>% 
      ggplot(aes(x = Median.Prices, y = Mean.Prices, color = Seller)) + geom_point(size=9) + 
      geom_text(aes(label=Seller), vjust=-2) + coord_cartesian(xlim = c(350, 750), ylim = c(450, 850)) +
      xlab("Median Prices") + ylab("Mean Prices") + ggtitle("Comparing Mean and Median Prices") +
      theme(legend.position="none") + guides(size=FALSE) + scale_y_continuous(labels=dollar_format(prefix = "$")) +
      scale_x_continuous(labels=dollar_format(prefix = "$")) + scale_color_manual(values = colors) 
  })
  
  output$price_density <- renderPlot({
    colors = c("Movado" = "#8e0000", "Macy's" = "#171b64", "Nordstrom" = "#9ec2e1", "Amazon" = "#ffb2b2")
    ggplot() + geom_density(data = movado_df, aes(x = price, alpha = .8, fill = "Movado"), outline.type = "full") + 
      geom_density(data = nordstrom_df, aes(x = price, alpha = .8, fill = "Nordstrom"), outline.type = "full") + 
      geom_density(data = amazon_df, aes(x = price, alpha = .8, fill = "Amazon"), outline.type = "full") + 
      geom_density(data = macys_df, aes(x = price, alpha = .8, fill = "Macy's"), outline.type = "full") + 
      ggtitle("Price Density by Retailer") + guides(alpha = FALSE) + 
      labs(x = "Price", y = "Density", fill = "Legend") + theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      scale_fill_manual(values = colors) + scale_x_continuous(labels=dollar_format(prefix = "$"))
  })
  
  output$differences_table <- DT::renderDataTable(
    datatable(colnames = c("Model Number", "Watch Model", "Movado's Price", "Macy's Price", "Amazon's Price", 
                           "Nordstrom's Price", "Max Difference"), prices_df, options=list(
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '80%'});",
                               "}"))) %>% 
      formatStyle(columns = c(T, T, T, T, T, T, T), fontSize = "90%", backgroundSize = '70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>% 
      formatCurrency(columns = c("price_movado", "price_macys", "price_amazon", "price_nordstrom", "difference"), currency ="$")
  )
  
  output$word_count <- renderPlot({
    colors = c("Nordstrom" = "#9ec2e1", "Macy's" = "#171b64")
    ggplot() + geom_density(aes(x=macys_df[macys_df$avg_w_count != 0, "avg_w_count"], alpha = 2, fill = "Macy's"), outline.type = "full") + 
      geom_density(aes(x=nordstrom_df[nordstrom_df$avg_w_count != 0, "avg_w_count"], alpha = 2, fill = "Nordstrom"), outline.type = "full") + 
      scale_fill_manual(values = colors) + ggtitle("Review Word Count Densities, per Retailer") + 
      labs(x = "Word Count", y = "Density", fill = "Legend")  +
      theme(legend.justification=c(1,1), legend.position=c(1,1)) + guides(alpha = FALSE)
  })
  
  # output$price_graph <- renderGvis(
  #   prices_df %>% filter(watch_model == input$select_model) %>%
  #     summarise(Movado = mean(price_movado, na.rm = TRUE), "Macy's" = mean(price_macys, na.rm = TRUE),
  #               "Amazon" = mean(price_amazon, na.rm = TRUE), "Nordstrom" = mean(price_nordstrom, na.rm = TRUE)) %>%
  #     pivot_longer(cols = c("Movado", "Amazon", "Macy's", "Nordstrom")) %>%
  #     gvisColumnChart(xvar = "name", yvar = "value",
  #                  options=list(width="auto", height="330px", bar = "{groupWidth: '95%'}",
  #                               legend = "{position: 'none'}", colors = "['#55c821']",
  #                               vAxis = "{format: 'currency', title: 'Price'}",
  #                               hAxis = "{title: 'Retailer'}"))
  # )
  # 
  # output$price_bubble <- renderGvis(
  #   prices_df %>% filter(watch_model == input$select_model) %>% 
  #     select(Model = watch_model, Movado = price_movado, "Macy's" = price_macys, "Amazon" = price_amazon, "Nordstrom" = price_nordstrom) %>% 
  #     pivot_longer(names_to = "Retailer", values_to = "Price", cols = c("Movado", "Amazon", "Macy's", "Nordstrom")) %>% 
  #     mutate(Count = if_else(Retailer == "Movado", sum(Retailer == "Movado" & !is.na(Price)), 
  #                           ifelse(Retailer == "Amazon", sum(Retailer == "Amazon" & !is.na(Price)), 
  #                                  sum(Retailer == "Macy's" & !is.na(Price))))) %>% 
  #     gvisBubbleChart(xvar = "Price", yvar = "Count", colorvar = "Retailer", #sizevar = "Count", 
  #                  options=list(width="auto", height="330px", hAxis = "{title: 'Price', format: 'currency'}",
  #                               vAxis = "{title: 'Frequency Count'}", vAxis='{minValue:0}',# maxValue:30}', 
  #                                legend = "{position: 'top'}", bubble = "{opacity: .4}"))
  # )
  
  output$prod_num_graph <- renderPlot(
    
    all_df %>% filter(., model_number == input$select_prod_num) %>%
      ggplot(aes(x=seller, y=price)) + 
      geom_bar(stat= "identity", fill = "lightblue") +
      ggtitle(paste(input$select_prod_num, "'s Price Distribution: ")) + 
      xlab("Seller") + ylab("Price")
    
  )
  
  # output$prod_num_graph <- renderGvis(
  #   prices_df %>% filter(model_number == input$select_prod_num) %>% 
  #     summarise(Movado = mean(price_movado, na.rm = TRUE), "Macy's" = mean(price_macys, na.rm = TRUE), 
  #               "Amazon" = mean(price_amazon, na.rm = TRUE), "Nordstrom" = mean(price_nordstrom, na.rm = TRUE)) %>% 
  #     pivot_longer(cols = c("Movado", "Amazon", "Macy's", "Nordstrom")) %>% 
  #     gvisColumnChart(xvar = "name", yvar = "value", 
  #                     options=list(width="auto", height="350px", bar = "{groupWidth: '95%'}", 
  #                                  legend = "{position: 'none'}", #colors = "['#55c821']", 
  #                                  vAxis = "{format: 'currency', title: 'Price'}",
  #                                  hAxis = "{title: 'Retailer'}"))
  # )
  
  output$selected_prod_num <- renderDataTable(
    all_df %>% filter(., model_number == input$select_prod_num)
  )
  
  
  
  # MACYS
  output$macys_stars <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=rating), fill="red") + ggtitle("Macy's Ratings Distribution") + 
      xlab("Rating (%)") + ylab("Count")
  )
  
  output$macys_zero_reviews_pie <- renderGvis(
    macys_df %>% transmute(reviewed = ifelse(review_count > 0, "Yes", "No")) %>% 
      group_by(reviewed) %>% tally() %>% 
      gvisPieChart(labelvar = "reviewed", numvar = "n", 
                   options=list(width="400px", height="350px", 
                                title = "Macy's Products With Reviews versus Products Without",
                                chartArea= "{left:40, top:30, bottom:0, right:0}", 
                                colors="['#db9081', '#81db92']"))
  )
  
  output$macys_review_count <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=review_count), fill="red") + ggtitle("Macy's Review per Product Distribution") + 
      xlab("Number of Reviews / Product") + ylab("Count")
  )
  
  output$macys_review_count_sans_0 <- renderPlot(
    macys_df %>% filter(review_count > 0) %>% ggplot() + geom_histogram(binwidth = 1, aes(x=review_count), fill="red") + 
      ggtitle("Macy's Review per Product Distribution, (Review Count > 0)") + 
      xlab("Number of Reviews / Product") + ylab("Count") + xlim(0, 25)
  )
  
  # output$macys_price <- renderPlot(
  #   macys_df %>% ggplot() + geom_histogram(aes(x=price), fill="red") + ggtitle("Macy's Price Distribution") + 
  #     xlab("Watch Price") + ylab("Count")
  # )
  
  output$combo_macys_price <- renderGvis(
     macys_df %>% select("Price" = price) %>%
      gvisHistogram(options=list(width="auto", height="450px", legend = "{position: 'top'}", #colors = "['#55c821']",
                                 hAxis = "{format: 'currency', title: 'Price'}",
                                 vAxis = "{title: 'Count'}"))
    
    # macys_df %>% group_by(group) %>% summarise(Price = mean(price, na.rm = TRUE), "Product Count" = n(), "Review Count" = sum(review_count)) %>% 
    #   gvisColumnChart(xvar = "Price", yvar = "Product Count", 
    #                   options=list(width="auto", height="450px", legend = "{position: 'top'}", #colors = "['#55c821']", 
    #                                hAxis = "{format: 'currency', title: 'Price'}",
    #                                vAxis = "{title: 'Count'}"))
  )
  
  # NORDSTROM
  output$nordstrom_stars <- renderPlot(
    nordstrom_df %>% ggplot() + geom_histogram(aes(x=rating), fill="gold") + ggtitle("Nordstrom's Ratings Distribution") + 
      xlab("Rating (1-5 Stars)") + ylab("Count")
  )
  
  output$nordstrom_zero_reviews_pie <- renderGvis(
    nordstrom_df %>% transmute(reviewed = ifelse(review_count > 0, "Yes", "No")) %>% 
      group_by(reviewed) %>% tally() %>% 
    gvisPieChart(labelvar = "reviewed", numvar = "n", 
                 options=list(width="400px", height="350px", 
                              title = "Nordstrom Products With Reviews versus Products Without",
                              chartArea= "{left:40, top:30, bottom:0, right:0}", 
                              colors="['#db9081', '#81db92']"))
  )
  
  output$nordstrom_review_count_sans_0 <- renderPlot(
    nordstrom_df %>% filter(review_count > 0) %>% ggplot() + geom_histogram(binwidth = 1, aes(x=review_count), fill="gold") + 
      ggtitle("Nordstrom's Review per Product Distribution, (Review Count > 0)") + 
      xlab("Number of Reviews / Product") + ylab("Count") + xlim(0, 70)
  )
  
  output$nordstrom_price <- renderPlot(
    nordstrom_df %>% ggplot() + geom_histogram(aes(x=price), fill="gold") + ggtitle("Nordstrom's Price Distribution") + 
      xlab("Watch Price") + ylab("Count")
  )
  
  
  # AMAZON
  output$amazon_stars <- renderPlot(
    amazon_d_df %>% ggplot() + geom_histogram(aes(x=star), fill="red") + ggtitle("Amazon Ratings Distribution") + 
      xlab("Rating") + ylab("Count")
  )
  
  output$amazon_zero_reviews_pie <- renderGvis(
    amazon_d_df %>% transmute(reviewed = ifelse(rev_count > 0, "Yes", "No")) %>% 
      group_by(reviewed) %>% tally() %>% 
      gvisPieChart(labelvar = "reviewed", numvar = "n", 
                   options=list(width="400px", height="350px", 
                                title = "Amazon's Products With Reviews versus Products Without",
                                chartArea= "{left:40, top:30, bottom:0, right:0}", 
                                colors="['#db9081', '#81db92']"))
  )
  
  output$amazon_review_count <- renderPlot(
    amazon_d_df %>% ggplot() + geom_histogram(aes(x=rev_count), fill="red") + ggtitle("Amazon's Review per Product Distribution") + 
      xlab("Number of Reviews / Product") + ylab("Count")
  )
  
  output$amazon_review_count_sans_0 <- renderPlot(
    amazon_d_df %>% filter(rev_count > 0) %>% ggplot() + geom_histogram(binwidth = 1, aes(x=rev_count), fill="red") + 
      ggtitle("Amazon's Review per Product Distribution, (Review Count > 0)") + 
      xlab("Number of Reviews / Product") + ylab("Count") + xlim(0, 25)
  )
  
  output$amazon_price <- renderPlot(
    amazon_d_df %>% ggplot() + geom_histogram(aes(x=price), fill = "lightblue") + ggtitle("Amazon's Price Distribution") + 
      xlab("Watch Price") + ylab("Count")
  )
  
  
  output$seller_table <- renderDataTable(
    
    amazon_df %>% filter(., seller == input$seller_var)
    
  )
  output$seller_price <- renderPlot(
    amazon_df %>% filter(., seller == input$seller_var) %>% ggplot() + geom_histogram(aes(x=price), fill = "lightblue") + ggtitle(paste(input$seller_var, " 's Price Distribution: ")) + 
      xlab("Watch Price") + ylab("Count")
  )
  
  output$watches_table <- renderDataTable(
    amazon_df %>% filter(., product == input$product_var)
  )
  
  output$watches_price <- renderPlot(
    amazon_df %>% filter(., product == input$product_var) %>% ggplot() + geom_histogram(aes(x=price), fill = "lightblue") + ggtitle(paste(input$product_var, " 's Price Distribution: ")) + 
      xlab("Watch Price") + ylab("Count")
  )
  
  output$full_table <- renderDataTable(
    amazon_df
  )
  
}