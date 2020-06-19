
function(input, output, session){
  
  # MOVADO
  output$movado_price <- renderPlot(
    movado_df %>% ggplot() + geom_histogram(aes(x=price), fill="skyblue") + ggtitle("Movado's Price Distribution") + 
      xlab("Watch Price") + ylab("Count")
  )
  
  
  # OVERVIEW
  output$overview_price <- renderPlot(
    cbind("Mean Prices" = c(round(mean(movado_df$price), 2), round(mean(macys_df$price), 2), 
                            round(mean(nordstrom_df$price), 2), round(mean(amazon_df$price), 2)),
          "Median Prices" = c(round(median(movado_df$price), 2), round(median(macys_df$price), 2), 
                              round(median(nordstrom_df$price), 2), round(median(amazon_df$price), 2)),
          "Seller"=c("Movado", "Macy's", "Nordstrom", "Amazon")) %>% 
      data.frame() %>% 
      ggplot(aes(x = Median.Prices, y=Mean.Prices, color=Seller)) + geom_point(size=9) + 
      geom_text(aes(label=Seller), vjust=-2) +
      xlab("Median Prices") + ylab("Mean Prices") + ggtitle("Comparing Mean and Median Prices") +
      theme(legend.position="none") + guides(size=FALSE)
  )
  
  output$price_graph <- renderGvis(
    prices_df %>% filter(watch_model_movado == input$select_model) %>% 
      summarise(Movado = mean(price_movado, na.rm = TRUE), "Macy's" = mean(price_macys, na.rm = TRUE), 
                "Amazon" = mean(price, na.rm = TRUE)) %>% pivot_longer(cols = c("Movado", "Amazon", "Macy's")) %>% 
      gvisColumnChart(xvar = "name", yvar = "value", 
                   options=list(width="auto", height="350px", bar = "{groupWidth: '95%'}", 
                                legend = "{position: 'none'}", colors = "['#55c821']", 
                                vAxis = "{format: 'currency', title: 'Price'}",
                                hAxis = "{title: 'Retailer'}"))
  )
  
  output$price_bubble <- renderGvis(
    prices_df %>% filter(watch_model_movado == input$select_model) %>% 
      select(Model = watch_model_movado, Movado = price_movado, "Macy's" = price_macys, "Amazon" = price) %>% 
      pivot_longer(names_to = "Retailer", values_to = "Price", cols = c("Movado", "Amazon", "Macy's")) %>% 
      mutate(Count = if_else(Retailer == "Movado", sum(Retailer == "Movado" & !is.na(Price)), 
                            ifelse(Retailer == "Amazon", sum(Retailer == "Amazon" & !is.na(Price)), 
                                   sum(Retailer == "Macy's" & !is.na(Price))))) %>% 
      gvisBubbleChart(xvar = "Price", yvar = "Count", colorvar = "Retailer", #sizevar = "Count", 
                   options=list(width="auto", height="350px", hAxis = "{title: 'Price', format: 'currency'}",
                                vAxis = "{title: 'Frequency Count'}", vAxis='{minValue:0}',# maxValue:30}', 
                                 legend = "{position: 'top'}", bubble = "{opacity: .4}"))
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
  
  output$macys_price <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=price), fill="red") + ggtitle("Macy's Price Distribution") + 
      xlab("Watch Price") + ylab("Count")
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
  output$amazon_price <- renderPlot(
    amazon_df %>% ggplot() + geom_histogram(aes(x=price), fill = "lightblue") + ggtitle("Amazon's Price Distribution") + 
      xlab("Watch Price") + ylab("Count")
  )
  
}