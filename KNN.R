# Wczytanie pakietów
library(class)

# Wczytanie danych
dane <- read.csv("C:/Users/Wojtek/Desktop/Projekt końcowy 2/diabetes.csv",header=TRUE,  sep = ',')

# Wyliczenia jakości klasyfikacji
class_quality <- function(training_data, test_data, k) {
  model <- knn(training_data[, 1:8], test_data[, 1:8], training_data[, 9], k = k)
  confusion_matrix <- table(test_data[, 9], model)
  qual <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(confusion_matrix)
  return(qual)
  
}

# Powtórzenie obliczeń 
class_quality_xn <- replicate(100, {
  # Podział na zbiór treningowy i testowy
  idx=sample(2,nrow(dane),replace=T,prob=c(0.7,0.3)) 
  train=dane[idx==1,]
  test=dane[idx==2,]
  class_quality(train, test, k = 10)
})

# Obliczenie i wyświetlenie odchylenia standardowego i dokładności
meanAccuracy <- mean(class_quality_xn)
sdAccuracy <- sd(class_quality_xn)
cat("Dokładność:", meanAccuracy, "\n")
cat("Odchylenie standardowe:", sdAccuracy, "\n")

