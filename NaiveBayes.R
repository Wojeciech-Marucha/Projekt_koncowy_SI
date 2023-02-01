# Wczytanie pakietów
library(naivebayes)

# Wczytanie danych
dane <- read.csv("C:/Users/Wojtek/Desktop/Projekt końcowy 2/diabetes.csv",header=TRUE,  sep = ',')

# Wyliczenia jakości klasyfikacji
Accuracy <- function(){
  
  # Podział na zbiór treningowy i testowy
  idx=sample(2,nrow(dane),replace=T,prob=c(0.7,0.3))
  train=dane[idx==1,]
  test=dane[idx==2,]
  
  # Wytrenowanie modelu drzewa decyzyjnego
  model=naive_bayes(as.character(Outcome) ~ ., data=train, usekernel=FALSE)  
  plot(model)
  
  # Predykcja
  p=predict(model,test)
  confMatrix = table(p, test$Outcome)
  print(confMatrix)
  pcf=confMatrix/sum(confMatrix)
  
  # DOKŁADNOŚĆ
  return (sum(diag(confMatrix))/sum(confMatrix))
}
results <- replicate(100, Accuracy())

# Obliczenie i wyświetlenie odchylenia standardowego i dokładności
meanAccuracy <- mean(results)
sdAccuracy <- sd(results)
cat("Dokładność:", meanAccuracy, "\n")
cat("Odchylenie standardowe:", sdAccuracy, "\n")


