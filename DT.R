# Wczytanie pakietów
library(rpart)
library(caret)

# Wczytanie danych
data <- read.csv("C:/Users/Wojtek/Desktop/Projekt końcowy 2/diabetes.csv",header=TRUE,  sep = ',')

# Powtórzenie 100 razy
accuracies <- c()
for (i in 1:100) {
  
  # Podział na zbiór treningowy i testowy
  idx <- sample(1:nrow(data), 0.3 * nrow(data))
  testData <- data[-idx, ]
  trainData <- data[idx, ]
  
  
  # Wytrenowanie modelu drzewa decyzyjnego
  model <- rpart(Outcome ~ ., data = trainData, method = "class") 
  #model <- rpart(Outcome ~ ., data = trainData, method = "class",  control = rpart.control (maxdepth = 10)) 
  plot(model)
  
  # Ocena jakości modelu
  pred <- predict(model, newdata = testData, type = "class")
  confMatrix <- confusionMatrix(pred,as.factor(testData$Outcome))
  
  # Zapisanie dokładności z i-tego powtórzenia
  accuracies[i] <- confMatrix$overall[1]
}

# Wyświetlenie 'confusion matrix'
print(confMatrix)

# Obliczenie i wyświetlenie odchylenia standardowego i dokładności
meanAccuracy <- mean(accuracies)
sdAccuracy <- sd(accuracies)
print(paste("Średnia dokładność:", meanAccuracy))
print(paste("Odchylenie standardowe:", sdAccuracy))




