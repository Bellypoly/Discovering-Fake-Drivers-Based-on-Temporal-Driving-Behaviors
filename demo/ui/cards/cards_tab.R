# source()
# Sample workable data
sample <- data.frame("Driver" = c("2", "4", "6", "10"),
                     "Ctree" = c(1,0.925,0.8666667 ,0.79 ),
                     "SVM" = c(1,0.9875,0.9583333 ,0.925 ),
                     "Randome_Forest" = c(1,0.95 ,0.95 ,0.91 ),
                     "KNN_3" = c(1,0.9375 ,0.8666667 ,0.84 ),
                     "KNN_5" = c(1,0.9125,0.9 ,0.83 ),
                     "KNN_7" = c(1,0.9 ,0.9 ,0.835 ),
                     "KNN_sqrt.n" = c(1,0.9125 ,0.8583333 ,0.77 ))

alg <- sample %>%
  select(Ctree:KNN_sqrt.n) %>%
  names() 

driver <- unique(sample$Driver)

# alg <- data.frame("Ctree" = "ctree",
#                   "SVM" = "svm",
#                   "Random Forest" = "rf",
#                   "KNN(3)" = "knn")
# Algorithm <-  alg %>%
#               select(Ctree:KNN(3)) %>%
#               names() 

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
          selectInput(inputId = "alg", choices = alg, label = "Select Algorithm",
                      multiple = TRUE,
                      selected = "Ctree")
        ),
        argonColumn(
          width = 9, 
          # plotOutput("distPlot")
          plotOutput("Plot")
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
          width = 6,
          radioButtons(
            "dist", 
            "Distribution type:",
            c("Normal" = "norm",
              "Uniform" = "unif",
              "Log-normal" = "lnorm",
              "Exponential" = "exp")
          )
        ),
        argonColumn(width = 6, plotOutput("plot"))
      )
    ) 
  )
)