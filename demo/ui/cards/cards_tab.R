# source()
# don't forget to change to call a fn after present
# change_driver :: w=1 driver=[2,4,6,10] seg=10
change_driver <- data.frame("Driver" = c("2", "4", "6", "10"),
                     "Ctree" = c(1,0.925,0.8666667 ,0.79 ),
                     "SVM" = c(1,0.9875,0.9583333 ,0.925 ),
                     "Randome_Forest" = c(1,0.95 ,0.95 ,0.91 ),
                     "KNN_3" = c(1,0.9375 ,0.8666667 ,0.84 ),
                     "KNN_5" = c(1,0.9125,0.9 ,0.83 ),
                     "KNN_7" = c(1,0.9 ,0.9 ,0.835 ),
                     "KNN_sqrt.n" = c(1,0.9125 ,0.8583333 ,0.77 ))

alg_driver <- change_driver %>%
  select(Ctree:KNN_sqrt.n) %>%
  names() 
driver <- unique(change_driver$Driver)


# change_w :: w=[1,2,5,6] driver=10 seg=30
change_w <- data.frame("w" = c("1", "2", "5", "6"),
                       "Ctree" = c(0.745,0.83,0.83,0.89),
                       "SVM" = c(0.87,0.915,0.88,0.775),
                       "Randome_Forest" = c(0.885,0.905,0.905,0.925  ),
                       "KNN_3" = c(0.79,0.925,0.855,0.7),
                       "KNN_5" = c(0.79,0.915,0.84,0.715),
                       "KNN_7" = c(0.785,0.905,0.81,0.695),
                       "KNN_sqrt.n" = c(0.695,0.85,0.73,0.605))

alg_w <- change_w %>%
  select(Ctree:KNN_sqrt.n) %>%
  names() 
w <- unique(change_w$w)

# change_seg :: w=2 driver=10 seg=[10,20,30,60]
change_seg <- data.frame("seg" = c("10", "20", "30", "60"),
                         "Ctree" = c(0.935 ,0.845 ,0.83 ,0.68 ),
                         "SVM" = c(0.98 ,0.96 ,0.915 ,0.745 ),
                         "Randome_Forest" = c(0.975 ,0.935 ,0.905 ,0.795 ),
                         "KNN_3" = c(0.93 ,0.9 ,0.925 ,0.81 ),
                         "KNN_5" = c(0.93 ,0.89 ,0.915 ,0.78 ),
                         "KNN_7" = c(0.945 ,0.865 ,0.905 ,0.755 ),
                         "KNN_sqrt.n" = c(0.85 ,0.81 ,0.85 ,0.625 ))
alg_seg <- change_seg %>%
  select(Ctree:KNN_sqrt.n) %>%
  names() 
seg <- unique(change_seg$seg)

cards_tab <- argonTabItem(
  tabName = "result",
  argonH1("Classification Result", display = 4),
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = "The Effect of Increasing the Action Space",
      argonRow(
        argonColumn(
          width = 3,
          selectInput(inputId = "drivers", choices = driver,
                      label = "Select # of Driver", multiple = TRUE),
          selectInput(inputId = "alg_driver", choices = alg_driver, label = "Select Algorithm",
                      multiple = TRUE,
                      selected = "Ctree")
        ),
        argonColumn(
          width = 9,
          plotOutput("plot_driver")
        )
      )
    ),
    br(), br(),
    argonCard(
      width = 12,
      title = "The Effect of Segmentation Splitting",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      argonRow(
        argonColumn(
          width = 3,
          selectInput(inputId = "w", choices = w,
                      label = "Select # of windows during feature generation", multiple = TRUE),
          selectInput(inputId = "alg_w", choices = alg_w, label = "Select Algorithm",
                      multiple = TRUE,
                      selected = "Ctree")
        ),
        argonColumn(
          width = 9,
          plotOutput("plot_w")
        )
      )
    ),br(), br(),
    argonCard(
      width = 12,
      title = "The Effect of Segmentation",
      src = NULL,
      hover_lift = TRUE,
      shadow = TRUE,
      shadow_size = NULL,
      hover_shadow = FALSE,
      border_level = 0,
      icon = argonIcon("atom"),
      status = "primary",
      background_color = NULL,
      gradient = FALSE, 
      floating = FALSE,
      argonRow(
        argonColumn(
          width = 3,
          selectInput(inputId = "seg", choices = seg,
                      label = "Select segmentation size", multiple = TRUE),
          selectInput(inputId = "alg_seg", choices = alg_seg, label = "Select Algorithm",
                      multiple = TRUE,
                      selected = "Ctree")
        ),
        argonColumn(
          width = 9,
          plotOutput("plot_seg")
        )
      )
    ) 
  )
)