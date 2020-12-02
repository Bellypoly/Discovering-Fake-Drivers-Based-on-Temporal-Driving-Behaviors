# org_df <- read.csv(file = "../../dataset/Driving_Data_KIA_SOUL.csv", header = T, stringsAsFactors = T)

s <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })
  
  output$plot <- renderPlot({
    dist <- switch(
      input$dist,
      norm = rnorm,
      unif = runif,
      lnorm = rlnorm,
      exp = rexp,
      rnorm
    )
    
    hist(dist(500))
  })
  
  # argonTable
  observeEvent(input$columns,{
    cols <- as.numeric(input$columns)
    if(length(input$columns) == 1){
      df <- data.frame(org_df[,cols])
      names(df) <- names(org_df)[cols]
      output$data = renderDataTable(df)
      
      # output$data1 <- renderUI({
      #   argonTable(
      #     cardWrap = T,
      #     headTitles = names(df),
      #     argonTableItems(
      #       argonTableItem(
      #         # dataCell = TRUE,
      #         # org_df[,cols]
      #         "row1"
      #       ),
      #     )
      #   )
      # })
    }else{
      output$data = renderDataTable(org_df[,cols])
    }
  })
  
  # output$argonTable <- renderUI({
  #   # wrap <- if (input$cardWrap == "Enable") TRUE else FALSE
  #   argonTable(
  #     cardWrap = T,
  #     headTitles = c(
  #       "PROJECT",
  #       "BUDGET",
  #       "STATUS",
  #       "USERS",
  #       "COMPLETION",
  #       ""
  #     ),
  #     argonTableItems(
  #       argonTableItem("Argon Design System"),
  #       argonTableItem(dataCell = TRUE, "$2,500 USD"),
  #       argonTableItem(
  #         dataCell = TRUE, 
  #         argonBadge(
  #           text = "Pending",
  #           status = "danger"
  #         )
  #       ),
  #       argonTableItem(
  #         argonAvatar(
  #           size = "sm",
  #           src = "https://image.flaticon.com/icons/svg/219/219976.svg"
  #         )
  #       ),
  #       argonTableItem(
  #         dataCell = TRUE, 
  #         argonProgress(value = 60, status = "danger")
  #       ),
  #       argonTableItem(
  #         argonButton(
  #           name = "Click me!",
  #           status = "warning",
  #           icon = "atom",
  #           size = "sm"
  #         )
  #       )
  #     )
  #   )
  # })
  
  output$Plot <- renderPlot({
    data = sample %>%
      filter(Driver %in% input$drivers) %>%
      select(one_of(c("Driver", input$alg))) %>%
      gather(Algorithm, Accuracy, -Driver)
    
    ggplot(data = data, aes(x = Driver, y= Accuracy)) + 
      geom_bar(aes(group = Algorithm, fill = Algorithm), stat = "identity", position = "dodge", color = "black") 
  })
}