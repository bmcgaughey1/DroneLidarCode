# start with confusion matrix from caret::confusionMatrix()
classificationMetrics <- function(cm) {
  TP <- cm$table[1,1]
  FP <- cm$table[1,2]
  FN <- cm$table[2,1]
  TN <- cm$table[2,2]

  N <- TP + TN + FP + FN
  accuracy <- (TP + TN) / N
  recall <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  specificity <- TN / (TN + FP)
  F1 <- 2 * (precision * recall) / (precision + recall)
  kappa <- (N *(TP + TN) - (((TP + FP) * (TP + FN)) + ((FN + TN) * (FP + TN)))) / (N * N - (((TP + FP) * (TP + FN)) + ((FN + TN) * (FP + TN))))

  cat("accuracy: ", round(accuracy, 4), "\n")
  cat("recall (sensitivity): ", round(recall, 4), "\n")
  cat("specificity: ", round(specificity, 4), "\n")
  cat("precision: ", round(precision, 4), "\n")
  cat("F1 score: ", round(F1, 4), "\n")
  cat("kappa: ", round(kappa, 4), "\n")
}

#classificationMetrics(allCM)
