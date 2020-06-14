
function(input, output, session){
  
  # MOVADO
  # output$movado_stars <- renderPlot(
    # movado_df %>% ggplot() + geom_histogram(aes(x=rating))
  # )
  
  output$movado_price <- renderPlot(
    movado_df %>% ggplot() + geom_histogram(aes(x=price))
  )
  
  
  # MACYS
  output$macys_stars <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=rating))
  )
  
  output$macys_review_count <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=review_count))
  )
  
  # pie chart of products reiewed versus never reviewed
  
  output$macys_price <- renderPlot(
    macys_df %>% ggplot() + geom_histogram(aes(x=price))
  )
  
  # NORDSTROM
  output$nordstrom_stars <- renderPlot(
    nordstrom_df %>% ggplot() + geom_histogram(aes(x=rating))
  )
  
  output$nordstrom_review_count <- renderPlot(
    nordstrom_df %>% ggplot() + geom_histogram(aes(x=review_count))
  )
  
  # pie chart of products reiewed versus never reviewed
  
  output$nordstrom_price <- renderPlot(
    nordstrom_df %>% ggplot() + geom_histogram(aes(x=price))
  )
  
}