microbenchmark(
  classifier(df_train, df_test, classifier_type = "ctree"),
  classifier(df_train, df_test, classifier_type = "svm")  
)
