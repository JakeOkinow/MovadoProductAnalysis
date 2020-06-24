
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
  
  # MACYS
  output$macys_stars <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=rating), fill="red") + ggtitle("Macy's Ratings Distribution") + 
      xlab("Rating (%)") + ylab("Count")
  )
  
  output$macys_zero_reviews_pie <- googleVis::renderGvis({
    macys_df %>% transmute(reviewed = ifelse(review_count > 0, "Yes", "No")) %>% 
      group_by(reviewed) %>% tally() %>% 
      googleVis::gvisPieChart(labelvar = "reviewed", numvar = "n", 
                   options=list(width="400px", height="350px", 
                                title = "Macy's Products With Reviews versus Products Without",
                                chartArea= "{left:40, top:30, bottom:0, right:0}", 
                                colors="['#db9081', '#81db92']"))
  })
  
  output$macys_review_count <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=review_count), fill="red") + ggtitle("Macy's Review per Product Distribution") + 
      xlab("Number of Reviews / Product") + ylab("Count")
  )
  
  output$macys_review_count_sans_0 <- renderPlot(
    macys_df %>% filter(review_count > 0) %>% ggplot() + geom_histogram(binwidth = 1, aes(x=review_count), fill="red") + 
      ggtitle("Macy's Review per Product Distribution, (Review Count > 0)") + 
      xlab("Number of Reviews / Product") + ylab("Count") + xlim(0, 25)
  )
  
  output$macys_price <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=price), fill="red") + ggtitle("Macy's Price Distribution") +
      xlab("Watch Price") + ylab("Count")
  )
  
  # output$combo_macys_price <- renderGvis(
  #    macys_df %>% select("Price" = price) %>%
  #     gvisHistogram(options=list(width="auto", height="450px", legend = "{position: 'top'}", #colors = "['#55c821']",
  #                                hAxis = "{format: 'currency', title: 'Price'}",
  #                                vAxis = "{title: 'Count'}"))
    
    # macys_df %>% group_by(group) %>% summarise(Price = mean(price, na.rm = TRUE), "Product Count" = n(), "Review Count" = sum(review_count)) %>% 
    #   gvisColumnChart(xvar = "Price", yvar = "Product Count", 
    #                   options=list(width="auto", height="450px", legend = "{position: 'top'}", #colors = "['#55c821']", 
    #                                hAxis = "{format: 'currency', title: 'Price'}",
    #                                vAxis = "{title: 'Count'}"))
  # )
  
  # NORDSTROM
  output$nordstrom_stars <- renderPlot(
    nordstrom_df %>% ggplot() + geom_histogram(aes(x=rating), fill="gold") + ggtitle("Nordstrom's Ratings Distribution") + 
      xlab("Rating (1-5 Stars)") + ylab("Count")
  )
  
  output$nordstrom_zero_reviews_pie <- googleVis::renderGvis(
    nordstrom_df %>% transmute(reviewed = ifelse(review_count > 0, "Yes", "No")) %>% 
      group_by(reviewed) %>% tally() %>% 
      googleVis::gvisPieChart(labelvar = "reviewed", numvar = "n", 
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
  
  output$amazon_zero_reviews_pie <- googleVis::renderGvis(
    amazon_d_df %>% mutate(reviewed = ifelse(rev_count > 0, "Yes", "No")) %>% select(reviewed) %>% 
      group_by(reviewed) %>% tally() %>% 
      googleVis::gvisPieChart(labelvar = "reviewed", numvar = "n", 
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
  
  
  output$seller_table <- DT::renderDataTable(
    amazon_df %>% filter(., seller == input$seller_var)
  )
  
  output$seller_price <- renderPlot(
    amazon_df %>% filter(., seller == input$seller_var) %>% ggplot() + 
      geom_histogram(aes(x=price), fill = "lightblue") + 
      ggtitle(paste(input$seller_var, " 's Price Distribution: ")) + 
      xlab("Watch Price") + ylab("Count")
  )
  
  output$watches_table <- DT::renderDataTable(
    datatable(amazon_df %>% filter(., product == input$product_var))
  )
  
  output$watches_price <- renderPlot(
    amazon_df %>% filter(., product == input$product_var) %>% ggplot() + 
      geom_histogram(aes(x=price), fill = "lightblue") + 
      ggtitle(paste(input$product_var, " 's Price Distribution: ")) + 
      xlab("Watch Price") + ylab("Count")
  )
  
  output$full_table <- DT::renderDataTable(
    amazon_df
  )
  
  # PRODUCT COMPARISON
  output$price_graph <- googleVis::renderGvis(
    prices_df %>% filter(watch_model == input$select_model) %>% 
      summarise(Movado = mean(price_movado, na.rm = TRUE), "Macy's" = mean(price_macys, na.rm = TRUE), 
                "Amazon" = mean(price_amazon, na.rm = TRUE), "Nordstrom" = mean(price_nordstrom, na.rm = TRUE)) %>% 
      pivot_longer(cols = c("Movado", "Amazon", "Macy's", "Nordstrom")) %>% 
      googleVis::gvisColumnChart(xvar = "name", yvar = "value", 
                                 options=list(width="auto", height="330px", bar = "{groupWidth: '95%'}", 
                                              legend = "{position: 'none'}", colors = "['#55c821']", 
                                              vAxis = "{format: 'currency', title: 'Price'}",
                                              hAxis = "{title: 'Retailer'}"))
  )
  
  output$price_bubble <- googleVis::renderGvis(
    prices_df %>% filter(watch_model == input$select_model) %>% 
      select(Model = watch_model, Movado = price_movado, "Macy's" = price_macys, "Amazon" = price_amazon, "Nordstrom" = price_nordstrom) %>% 
      pivot_longer(names_to = "Retailer", values_to = "Price", cols = c("Movado", "Amazon", "Macy's", "Nordstrom")) %>% 
      mutate(Count = if_else(Retailer == "Movado", sum(Retailer == "Movado" & !is.na(Price)), 
                             ifelse(Retailer == "Amazon", sum(Retailer == "Amazon" & !is.na(Price)), 
                                    sum(Retailer == "Macy's" & !is.na(Price))))) %>% 
      googleVis::gvisBubbleChart(xvar = "Price", yvar = "Count", colorvar = "Retailer", #sizevar = "Count", 
                                 options=list(width="auto", height="330px", hAxis = "{title: 'Price', format: 'currency'}",
                                              vAxis = "{title: 'Frequency Count'}", vAxis='{minValue:0}',# maxValue:30}', 
                                              legend = "{position: 'top'}", bubble = "{opacity: .4}"))
  )
  
  output$prod_num_graph <- googleVis::renderGvis(
    prices_df %>% filter(model_number == input$select_prod_num) %>% 
      summarise(Movado = mean(price_movado, na.rm = TRUE), "Macy's" = mean(price_macys, na.rm = TRUE), 
                "Amazon" = mean(price_amazon, na.rm = TRUE), "Nordstrom" = mean(price_nordstrom, na.rm = TRUE)) %>% 
      pivot_longer(cols = c("Movado", "Amazon", "Macy's", "Nordstrom")) %>% 
      googleVis::gvisColumnChart(xvar = "name", yvar = "value", 
                                 options=list(width="auto", height="350px", bar = "{groupWidth: '95%'}", 
                                              legend = "{position: 'none'}", #colors = "['#55c821']", 
                                              vAxis = "{format: 'currency', title: 'Price'}",
                                              hAxis = "{title: 'Retailer'}"))
  )
  
  output$selected_prod_num <- renderValueBox(
    infoBox(title = movado_df[movado_df["model_number"] == input$select_prod_num, "watch_model"], 
            value = paste(movado_df[movado_df["model_number"] == input$select_prod_num, c("case_diameter", "dial")], collapse = "mm "),
            color = "black", icon = icon("clock"), 
            href = movado_df[movado_df["model_number"] == input$select_prod_num, "url"])
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
  
  price_hist <- reactive({
    full_prices_df %>% group_by(model_number) %>% mutate(price_amazon = mean(price_amazon, na.rm = TRUE)) %>% 
      rename("Macy's" = price_macys, "Movado" = price_movado, "Nordstrom" = price_nordstrom, "Amazon" = price_amazon) %>% 
      top_n(1) %>% pivot_longer(cols = c("Movado", "Macy's", "Nordstrom", "Amazon"), 
                                           names_to = "Retailer", values_to = "Price") %>% 
      filter(!is.na(Price), Retailer %in% c(input$select_retailer)) %>% distinct(.keep_all = TRUE)
  })
  
  output$price_histogram <- renderPlot({
    colors = c("Movado" = "#8e0000", "Macy's" = "#171b64", "Nordstrom" = "#9ec2e1", "Amazon" = "#ffb2b2")
    price_hist() %>% ggplot() + geom_histogram(aes(x = Price, fill = Retailer), binwidth = 100, position = "dodge") + 
      ggtitle("Price Histogram by Retailer") + labs(x = "Price", y = "Frequency", fill = "Legend") +
       theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      scale_fill_manual(values = colors) + scale_x_continuous(labels=dollar_format(prefix = "$"), breaks = c(seq(0, 3500, 200)))
  })
  
  output$differences_table <- DT::renderDataTable(
    datatable(colnames = c("Model Number", "Watch Model", "Movado's Price", "Movado.com Stock", "Macy's Price", "Amazon's Price", 
                           "Nordstrom's Price", "Max Difference"), prices_df, options=list(
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '61%'});",
                               "}"))) %>% 
      formatStyle(columns = c(T, T, T, T, T, T, T, T), fontSize = "85%", backgroundSize = '75%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center') %>% 
      formatStyle("difference", background = styleColorBar(prices_df$difference, '#dd9787')) %>% 
      formatCurrency(columns = c("price_movado", "price_macys", "price_amazon", "price_nordstrom", "difference"), currency ="$")
  )
  
  output$sankey_avail <- networkD3::renderSankeyNetwork({
    links <- data.frame(stringsAsFactors = FALSE, 
                        "source" = c("All Unique Products Scraped", "All Unique Products Scraped", "All Unique Products Scraped", "All Unique Products Scraped", 
                                     "All Unique Products Scraped", "All Unique Products Scraped"), 
                        "target" = c("Only Movado.com", "Only Nordstrom.com", "Only Macys.com", "Only Amazon.com", "Movado.com and >= 1 Retailer", "Not avail on Movado.com, Only at Retailer"),
                        "value" = c(length(unique(full_prices_df[full_prices_df$in_stock == "In Stock" & is.na(full_prices_df["price_macys"]) & 
                                                                   is.na(full_prices_df["price_nordstrom"]) & is.na(full_prices_df["price_amazon"]), "model_number"])), 
                                    length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | is.na(full_prices_df$in_stock)) & is.na(full_prices_df["price_macys"]) & 
                                                                   is.na(full_prices_df["price_amazon"]), "model_number"])),
                                    length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | is.na(full_prices_df$in_stock)) & is.na(full_prices_df["price_nordstrom"]) & 
                                                                   is.na(full_prices_df["price_amazon"]), "model_number"])), 
                                    length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | is.na(full_prices_df$in_stock)) & is.na(full_prices_df["price_macys"]) & 
                                                                   is.na(full_prices_df["price_nordstrom"]), "model_number"])), 
                                    length(unique(full_prices_df[full_prices_df$in_stock == "In Stock" & -(is.na(full_prices_df["price_macys"]) & is.na(full_prices_df["price_nordstrom"]) &
                                                                                                             is.na(full_prices_df["price_amazon"])), "model_number"])), 
                                    length(unique(full_prices_df[(full_prices_df$in_stock != "In Stock" | is.na(full_prices_df$in_stock)) & 
                                                                   !(is.na(full_prices_df["price_macys"]) & is.na(full_prices_df["price_nordstrom"]) &
                                                                       is.na(full_prices_df["price_amazon"])), "model_number"]))
                        )
    )
    nodes <-data.frame(name = c(links$source, links$target) %>% unique())
    links$IDsource = match(links$source, nodes$name) - 1
    links$IDtarget = match(links$target, nodes$name) - 1
    networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource",
                  Target = "IDtarget", Value = "value", NodeID = "name", sinksRight = TRUE, 
                  fontSize = 15, nodeWidth = 30)
  })
  
  output$word_count <- renderPlot({
    colors = c("Nordstrom" = "#9ec2e1", "Macy's" = "#171b64")
    ggplot() + geom_density(aes(x=macys_df[macys_df$avg_w_count != 0, "avg_w_count"], alpha = 2, fill = "Macy's"), outline.type = "full") + 
      geom_density(aes(x=nordstrom_df[nordstrom_df$avg_w_count != 0, "avg_w_count"], alpha = 2, fill = "Nordstrom"), outline.type = "full") + 
      scale_fill_manual(values = colors) + ggtitle("Review Word Count Densities, per Retailer (Excludes Unreviewed Products)") + 
      labs(x = "Word Count", y = "Density", fill = "Legend")  +
      theme(legend.justification=c(1,1), legend.position=c(1,1)) + guides(alpha = FALSE)
  })
  
  output$avg_word_count <- renderPlot(
    data.frame("Retailer" = c("Macy's", "Nordstrom"), "Average" = c(mean(macys_df$avg_w_count), mean(nordstrom_df$avg_w_count))) %>% 
      ggplot(aes(x = Retailer, y=Average, fill = Retailer)) + geom_col(alpha = .8) + 
      geom_text(aes(label = round(Average, 1), vjust = 2), color = c("white", "black")) + 
      scale_fill_manual(values = c("Nordstrom" = "#9ec2e1", "Macy's" = "#171b64")) + ggtitle("Average Word Count, per Retailer (All Products Included)")
  )
  
  output$missing_reviews <- renderPlot(
    data.frame("Retailer" = c("Macy's", "Nordstrom", "Amazon"), "Unreviewed" = c(sum(macys_df$review_count == 0), 
                                                                                 sum(nordstrom_df$review_count == 0), sum(amazon_df$rev_count == 0)), 
               "Reviewed" = c(sum(macys_df$review_count != 0), sum(nordstrom_df$review_count != 0), sum(amazon_df$rev_count != 0))) %>% 
      pivot_longer(cols = c(Reviewed, Unreviewed)) %>% 
      ggplot(aes(x = Retailer)) + geom_col(aes(y = value, fill = name), position = position_fill(reverse = TRUE)) + 
      labs(y = "Percentage", fill = "Legend") + scale_fill_manual(values = c("Unreviewed" = "#dd9787", "Reviewed" = "#678d58")) + 
      ggtitle("Proportion of Products with Reviews, per Retailer")
  )
  
  output$review_word_cloud <- renderImage({
    outfile <- tempfile(fileext = '.png')
    png(outfile, width=1350, height=1125, res = 300, bg="transparent")
    wordcloud(max.words = 100, words = word_freq_df$word, freq = word_freq_df$frequency, scale=c(3.5, 0.3),
              color = wesanderson::wes_palette(type = "continuous", name = "Darjeeling1"))
    dev.off()
    list(src = outfile,
         contentType = 'image/png',
         width = 450,
         height = 375)
  })
  
  output$incorrect_nordstrom <- DT::renderDataTable(
    nordstrom_df %>% filter(bullet_d_case_d != case_diameter) %>% 
      left_join(select(movado_df, movado_url = url, model_number), by = "model_number") %>% 
      select(watch_model, url, "Color" = color, model_number, "Title's Case Diameter" = case_diameter, 
             "Description's Case Diameter" = bullet_d_case_d, "Description" = bullet_details, movado_url) %>% 
      mutate(watch_model = paste0("<a href='", url, "'>", watch_model, "</a>")) %>% 
      mutate(model_number = paste0("<a href='", movado_url, "'>", model_number, "</a>")) %>%
      select(-url, -movado_url, "Nordstrom Watch Title" = watch_model, "Model Number" = model_number) %>% 
      datatable(escape = 1,  options = list(columnDefs = list(list(
        targets = 6,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 35 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
          "}")
      ))
      ))
  )
  
  output$vague_nordstrom <- DT::renderDataTable(
    nordstrom_df %>% left_join(select(movado_df, movado_model = watch_model, model_number, movado_url = url), by = "model_number") %>% 
      mutate(watch_model = paste0("<a href='", url, "'>", watch_model, "</a>")) %>% 
      mutate(model_number = paste0("<a href='", movado_url, "'>", model_number, "</a>")) %>%
      select("Nordstrom's Watch Title" = watch_model, "Color" = color, "Inferred Collection" = collection, "Actual Model Name" = movado_model, "Model Number" = model_number, "Nordstrom's Price" = price) %>% 
      datatable(escape = 1) %>% 
      formatStyle("Inferred Collection", backgroundColor = styleEqual("Unknown", '#dd9787')) %>% 
      formatCurrency(columns = "Nordstrom's Price", currency ="$")
  )
  
  output$vague_description_nordstrom <- DT::renderDataTable(
    nordstrom_df %>% left_join(select(movado_df, movado_model = watch_model, model_number, movado_url = url), by = "model_number") %>% 
      mutate(watch_model = paste0("<a href='", url, "'>", watch_model, "</a>")) %>% 
      mutate(model_number = paste0("<a href='", movado_url, "'>", model_number, "</a>")) %>%
      select("Nordstrom's Watch Title" = watch_model, "Color" = color, "Patterns in Description" = dial, "Movado's Model Name" = movado_model, "Model Number" = model_number, "Nordstrom's Description" = description) %>% 
      datatable(escape = 1, options = list(columnDefs = list(list(
        targets = 6,
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 35 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
          "}")
      ))))# %>% 
      # formatStyle("Inferred Collection", backgroundColor = styleEqual("Unknown", '#dd9787')) %>% 
  )
  
}