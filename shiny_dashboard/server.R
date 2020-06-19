
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
  
  
  # AMAZON COPY PASTE #
  
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
  
  
  output$seller_table <- renderDataTable(datatable(
    
    data,
    escape = -2, # raw HTML in column 2
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0,nested_columns) ), # Hide row numbers and nested columns
        list(orderable = FALSE, className = 'details-control', targets = 1) # turn first column into control column
      )
    ),
    callback = JS(callback)

  ))
  
  output$watches_table <- renderDataTable(
    amazon_df %>% group_by(product) %>% summarize(count = n()) %>% arrange(., -count)
  )
  
  #####AMAZON COPY PASTE#########
  
}