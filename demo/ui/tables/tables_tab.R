v <- 1:ncol(org_df)
names(v) <- names(org_df)

data_tab <- argonTabItem(
  tabName = "data",
  # h1("Data Samples"),
  # sidebarLayout(
  #   sidebarPanel(
  #     checkboxGroupInput("columns","Select Columns ::",choices=v,inline = F, selected = list(7,48,18,8,24,16,5,12,23,51,15)),
  #     style = "height:550px; overflow-y: scroll;"
  #   ),
  #   mainPanel(
  #     # uiOutput("data1"),
  #     # uiOutput("argonTable"),
  #     tags$div(class = 'table-responsive card shadow',
  #              dataTableOutput('data'),
  #              style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
  #     ),
  #   )
  # )
  # classic cards
  argonH1("Dataset", display = 4),
  argonRow(
    argonInfoCard(
      value = "10", 
      title = "DRIVERS", 
      # stat = 3.48,
      description = "Who have driven the car for 23 hours",
      icon = icon("users"), 
      icon_background = "danger",
      # background_color = "default",
      shadow = T
    ),
    argonInfoCard(
      value = "51", 
      title = "SENSOR DATA",
      # stat_icon = icon("arrow-down"),
      description = "From the real driving of a KIA car", 
      icon = icon("car"), 
      icon_background = "warning",
      shadow = TRUE
    ),
    argonInfoCard(
      value = "11", 
      title = "SIGNAL SELECTION", 
      # stat = 12, 
      # stat_icon = icon("arrow-up"),
      description = "By using Random Forest", 
      icon = icon("vote-yea"),
      icon_background = "info",
      shadow = T
      # gradient = TRUE,
      # background_color = "default",
      # hover_lift = F
    ),
    argonInfoCard(
      value = "94,380", 
      title = "Data", 
      # stat = -1.10, 
      # stat_icon = icon("arrow-down"),
      description = "with a size of 16.9Mb in total",
      icon = icon("table"), 
      icon_background = "yellow",
      # background_color = "default",
      shadow = T
    )
  ),
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      # icon = icon("cogs"),
      # status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      # title = "Shiny Inputs",
      argonRow(
        argonColumn(
          width = 3,
          checkboxGroupInput("columns","Select Columns ::",choices=v,inline = F, selected = list(7,48,18,8,24,16,5,12,23,51,15)),
          style = "height:400px; overflow-y: scroll;"
        ),
        argonColumn(
          width = 9,
          dataTableOutput('data'),
          style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
        )
      )
    ) 
  )
)
