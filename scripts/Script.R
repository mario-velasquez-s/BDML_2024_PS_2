# 
# Big Data y Machine Learning para Economia Aplicada
# Maria Camila Arias, Martin Velasquez, Mario Velasquez, Daniela Vlasak
# Problem Set 2
# 

# Initial Setup -----------------------------------------------------------

rm(list = ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       gridExtra, ## visualizing missing data
       corrplot, ## Correlation Plots 
       stargazer, ## tables/output to TEX. 
       MASS,
       rvest,
       httr,
       dplyr,
       ggplot2,
       visdat,
       caret,
       xtable,  # For predictive model assessment
       fixest, 
       zoo,
       smotefamily,
       ROSE)  # Fixed effects 

# 1: Initial Data Manipulation -----------------------------------------------


username <- Sys.info()["user"]
if (username == "Maria.Arias") {
  setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 2")
} else if (username == "marti") {
  setwd("C:/Users/marti/OneDrive - Universidad de los andes/BDML - Datos")
} else if (username == "") {
  # Mario
  setwd("/default/path/for/other/users")
} else {
  #Daniela
  setwd("/Users/danielavlasak/Library/CloudStorage/OneDrive-UniversidaddelosAndes/ANDES/Semestre 8/BDML/Datos_PS2")
}

train_hogares <- read.csv("data/train_hogares.csv")
train_personas <- read.csv("data/train_personas.csv")
test_hogares <- read.csv("data/test_hogares.csv")
test_personas <- read.csv("data/test_personas.csv")

str(train_hogares)
str(train_personas)

## I choose the individual-level variables that could be helpful

pre_process_personas<-  function(data, ...) {
  
  data <- data %>% mutate(
    mujer = ifelse(P6020==2,1,0), 
    H_Head = ifelse(P6050== 1, 1, 0), #Household head
    menor = ifelse(P6040<=11,1,0), # Menores
    EducLevel = ifelse(P6210==9,0,P6210), #Replace 9 with 0
    ocupado = ifelse(is.na(Oc),0,1),
    afiliadoSalud = ifelse(P6090 == 9, NA, ifelse(P6090==2,0,P6090)),
    edad = P6040,
  ) %>% 
    dplyr::select(id, Orden,mujer,H_Head,menor,EducLevel,ocupado, afiliadoSalud, edad)
  
  
}

train_personas <- pre_process_personas(train_personas)
test_personas <- pre_process_personas(test_personas)


## We detect missing values

train_miss <- skim(train_personas)
print(train_miss) ## Missing values in EducLevel and afiliadoSalud



## Imputation of afiliadoSalud

train_personas_AfiSaludmiss <- train_personas %>% 
  mutate(AfiSalud_miss = ifelse(is.na(afiliadoSalud) == TRUE,1,0)) %>% 
  dplyr::filter(AfiSalud_miss == 1)

ggplot(train_personas_AfiSaludmiss, aes(x=edad))+
  geom_histogram(fill="#0099F8") +
  labs(x="Edad") + 
  theme_bw()

summary(train_personas_AfiSaludmiss$edad) ## Conclusion: Those missing in afiliacionSalud are children and someone with 84 years. 
## I can impute them with the values of their parents. I assume that if someone in the household has health security, the
## child must have.

train_personas$afiliadoSalud <- ave(train_personas$afiliadoSalud, 
                                    train_personas$id, FUN = function(x) {
                                      if (all(is.na(x))) {
                                        return(0)  # Asigna 0 si todos los valores son NA
                                      } else {
                                        return(ifelse(is.na(x), max(x, na.rm = TRUE), x))  # Calcula el máximo si hay valores no NA
                                      }
                                    })

test_personas$afiliadoSalud <- ave(test_personas$afiliadoSalud, 
                                    test_personas$id, FUN = function(x) {
                                      if (all(is.na(x))) {
                                        return(0)  # Asigna 0 si todos los valores son NA
                                      } else {
                                        return(ifelse(is.na(x), max(x, na.rm = TRUE), x))  # Calcula el máximo si hay valores no NA
                                      }
                                    })


## Imputation of EducLevel

train_personas_Edumiss <- train_personas %>% 
  mutate(Edu_miss = ifelse(is.na(EducLevel) == TRUE,1,0)) %>% 
  dplyr::filter(Edu_miss == 1)

ggplot(train_personas_Edumiss, aes(x=edad))+
  geom_histogram(fill="#0099F8") +
  labs(x="Edad") + 
  theme_bw()

summary(train_personas_Edumiss$edad) ## Conclusion: Those missing in EducLevel are children. 
##  This makes sense because below 2 years children haven't gone to school. I can impute them with EducLevel=0.
impute_Educlevel <- function(data) {
  data <- data %>% mutate(EducLevel = ifelse(is.na(EducLevel) ==TRUE,0,EducLevel))
}

train_personas <- impute_Educlevel(train_personas)
test_personas <- impute_Educlevel(test_personas)

## Creation of variables at individual level
train_personas_nivel_hogar<- train_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            noafiliados = sum(afiliadoSalud, na.rm=TRUE)
  )

train_personas_hogar<- train_personas %>% 
  filter(H_Head==1) %>% 
  dplyr::select(id,mujer,EducLevel,ocupado,afiliadoSalud) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado,
         H_Head_afiliadoSalud = afiliadoSalud) %>% 
  left_join(train_personas_nivel_hogar)

test_personas_nivel_hogar<- test_personas %>% 
  group_by(id) %>% 
  summarize(nmujeres=sum(mujer,na.rm=TRUE),
            nmenores=sum(menor,na.rm=TRUE),
            maxEducLevel=max(EducLevel,na.rm=TRUE),
            nocupados=sum(ocupado,na.rm=TRUE),
            noafiliados = sum(afiliadoSalud, na.rm=TRUE)
  )

test_personas_hogar<- test_personas %>% 
  filter(H_Head==1) %>% 
  dplyr::select(id,mujer,EducLevel,ocupado,afiliadoSalud) %>% 
  rename(H_Head_mujer=mujer,
         H_Head_Educ_level=EducLevel,
         H_Head_ocupado=ocupado,
         H_Head_afiliadoSalud = afiliadoSalud) %>% 
  left_join(test_personas_nivel_hogar)


## Household variables
## (POR AHORA DEJÉ LAS VARIABLES DE HOGAR QUE PUSO ANDRÉS PERO TENEMOS QUE ESCOGER MEJOR)
train_hogares<- train_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  dplyr::select(id,Dominio,arrienda,Pobre)


test_hogares<- test_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  dplyr::select(id,Dominio,arrienda) 

## Finally, Train & Test data
train<- train_hogares %>% 
  left_join(train_personas_hogar) %>% 
  dplyr::select(-id) #no longer need id

test<- test_hogares %>% 
  left_join(test_personas_hogar)

train<- train %>% 
  mutate(Dominio=factor(Dominio),
         arrienda=factor(arrienda,levels=c(0,1),labels=c("No","Yes")),
         H_Head_mujer = factor(H_Head_mujer, levels= c(0,1), labels=c("No", "Yes")),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         H_Head_ocupado = factor(H_Head_ocupado, levels= c(0,1), labels= c("No", "Yes")),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud, levels = c(0,1), labels=c("No", "Yes"))
  )

test<- test %>% 
  mutate(Dominio=factor(Dominio),
         arrienda=factor(arrienda,levels=c(0,1),labels=c("No","Yes")),
         H_Head_mujer = factor(H_Head_mujer, levels= c(0,1), labels=c("No", "Yes")),
         H_Head_Educ_level=factor(H_Head_Educ_level,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
         H_Head_ocupado = factor(H_Head_ocupado, levels= c(0,1), labels= c("No", "Yes")),
         H_Head_afiliadoSalud = factor(H_Head_afiliadoSalud, levels = c(0,1), labels=c("No", "Yes"))
  )

#2: CLASSIFICATION APPROACH ----------------------------------------------------

## 2.1: Linear Regression
set.seed(685397)

colnames(train)

## I will use k-fold validation to test my training models
k <- 5
nrow(train)/k

## Generate an index for each fold
train <-train  %>% mutate(fold=c(rep(1,32992),
                              rep(2,32992),
                              rep(3,32992),
                              rep(4,32992),
                              rep(5,32992)))

## Models
    
    mod1 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores*H_Head_mujer +
      H_Head_afiliadoSalud + H_Head_Educ_level*H_Head_mujer + arrienda + Dominio*H_Head_mujer + noafiliados
    
    mod2 <- Pobre ~ H_Head_mujer*H_Head_ocupado + nocupados + nmujeres  + nmenores + H_Head_Educ_level
    
    mod3 <- Pobre ~ H_Head_mujer*H_Head_ocupado + poly(nocupados, 3, raw= TRUE) + nmujeres  + nmenores
    mod4 <- Pobre ~ .
      


cv_mse_f1 <- function(base,fold_size,modelo,c,...){
  l <- fold_size
  
  db_train <- list()
  db_test <- list()
  
  for(i in 1:l){
    db_train[[i]] <- base %>% filter(fold!=i) 
    db_test[[i]] <- base %>% filter(fold==i)
    
    fit <-lm(modelo, data= db_train[[i]])
    db_test[[i]] <- db_test[[i]] %>% mutate(Pobre_hat = ifelse(predict(fit, newdata=db_test[[i]]) >= c, 1, 0))
  }
  
  MSE <- list()
  for(i in 1:l){
    MSE[[i]] <- mean((db_test[[i]]$Pobre - db_test[[i]]$Pobre_hat)^2, na.rm=TRUE)
  }
  MSE<-do.call(rbind,MSE)
  print(paste("MSE:",round(mean(MSE, na.rm = TRUE), digits=3)))
  
  
  F1 <- list()
  for(i in 1:l){
    TP <- sum(db_test[[i]]$Pobre == 1 & db_test[[i]]$Pobre_hat == 1 )
    TN <- sum(db_test[[i]]$Pobre == 0 & db_test[[i]]$Pobre_hat == 0 )
    FP <- sum(db_test[[i]]$Pobre == 0 & db_test[[i]]$Pobre_hat == 1 )
    FN <- sum(db_test[[i]]$Pobre == 1 & db_test[[i]]$Pobre_hat == 0 )
    
    recall <- TP / (FN + TP)
    precision <- TP / (TP + FP)
    
    F1[[i]] <- 2 * precision * recall / (precision + recall)
  }
  F1<-do.call(rbind,F1)
  print(paste("F1:",round(mean(F1, na.rm = TRUE), digits=3)))
 
  return(mean(F1, na.rm = TRUE))
      
}
  

## I choose models minimizing MSE
cv_mse_f1(train,k,mod4,0.5)


## Choosing the best thresholds



best_thresh_cv<- function(base,nfolds,model,...){
  thresholds <- seq(0.25, 0.35, by = 0.005)
  f1_scores <- numeric(length(thresholds))
  max_f1 <- 0
  best_threshold <- 0
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    
    # Store the F1 score for this threshold
    f1_scores[i] <- cv_mse_f1(base,nfolds,model,threshold)
    
    # Update max_f1 and best_threshold if current F1 score is higher
    if (f1_scores[i] > max_f1) {
      max_f1 <- f1_scores[i]
      best_threshold <- threshold
    }
  }
  
  # Create a data frame with threshold and F1 score data
  threshold_f1_data <- data.frame(threshold = thresholds, f1_score = f1_scores)
  
  # Plot the relationship between threshold and F1 score
  graph_thresh_f1 <- ggplot(threshold_f1_data, aes(x = threshold, y = f1_score)) +
    geom_line() +
    geom_point() +
    labs(x = "Threshold", y = "F1 Score", title = "F1 Score vs. Threshold")
  
  print(paste("Best Threshold:",best_threshold))
  print(paste("F1:",max_f1))
  graph_thresh_f1
}

best_thresh_cv(train,k,mod1)


## Precicting and generating prediction file
    predictSample <- test %>%
      mutate(pobre_lab = ifelse(predict(lm(mod1, train), newdata=test) >= 0.305, 1, 0)) %>%
      dplyr::select(id,pobre_lab)
    
    head(predictSample)
    write.csv(predictSample,"predictions/classification_linearRegression.csv", row.names = FALSE)

## Predicting and generating prediction file
predictSample <- test %>%
  mutate(pobre_lab = ifelse(predict(lm(mod1, train), newdata=test) >= 0.5, 1, 0)) %>%
  dplyr::select(id,pobre_lab)


head(predictSample)
write.csv(predictSample,"predictions/classification_linearRegression.csv", row.names = FALSE)



## 2.2: ElasticNet

## 2.3: CART - Logit

train_control <- trainControl(
  method = "cv",
  number = 30,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = TRUE
)


X1 <- c("nmenores", "arrienda")

glm <- train(
  formula(paste0("Pobre ~", paste0(X1, collapse = " + "))),
  method = "glm",
  data = train,
  family = "binomial",
  trControl = train_control
)

confusionMatrix(data = glm$pred$pred, 
                reference = glm$pred$obs, 
                positive="Yes", mode = "prec_recall")


predicted_probabilities <- predict(glm, newdata = train, type = "prob")[, "Yes"]
predict(glm, newdata = train, type = "prob")[, "Yes"]

thresholds <- seq(0, 1, by = 0.01)
f1_scores <- numeric(length(thresholds))
max_f1 <- 0
best_threshold <- 0

for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  print(threshold)
  # Convert probabilities to binary predictions based on the threshold
  binary_predictions <- ifelse(predicted_probabilities > threshold, "Yes", "No")
  
  # Compute confusion matrix
  confusion <- table(binary_predictions, train$Pobre)
  
  # Check if confusion matrix is 2x2
  if (ncol(confusion) != 2 || nrow(confusion) != 2) {
    next  # Skip to the next threshold if the confusion matrix is not 2x2
  }
  
  # Calculate precision, recall, and F1 score
  precision <- confusion[2, 2] / sum(confusion[, 2])
  recall <- confusion[2, 2] / sum(confusion[2, ])
  f1_score <- 2 * precision * recall / (precision + recall)
  
  # Store the F1 score for this threshold
  f1_scores[i] <- f1_score
  
  # Update max_f1 and best_threshold if current F1 score is higher
  if (f1_score > max_f1) {
    max_f1 <- f1_score
    best_threshold <- threshold
  }
}

# Print the best threshold and corresponding max F1 score
cat("Best Threshold:", best_threshold, "\n")
cat("Max F1 Score:", max_f1, "\n")

# Create a data frame with threshold and F1 score data
threshold_f1_data <- data.frame(threshold = thresholds, f1_score = f1_scores)

# Plot the relationship between threshold and F1 score
ggplot(threshold_f1_data, aes(x = threshold, y = f1_score)) +
  geom_line() +
  geom_point() +
  labs(x = "Threshold", y = "F1 Score", title = "F1 Score vs. Threshold")

predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)

## 2.4: CART - LDA

# Convert Pobre into a factor
train$Pobre <- factor(train$Pobre)
levels(train$Pobre) <- make.names(levels(train$Pobre))


# Define train control
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     verbose = FALSE
)

# Perform LDA classification
lda <- train(Pobre ~ nmenores + arrienda, 
             data = train, 
             method = "lda",
             trControl = ctrl)

# Make predictions using your trained model
predictions <- predict(lda, newdata = test)


# Compute confusion matrix
conf_matrix <- confusionMatrix(predictions, test$Pobre)

# Calculate F1 score
f1_score <- as.numeric(conf_matrix$byClass['F1'])


#3: INCOME REGRESSION APPROACH -------------------------------------------------



