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
       zoo)  # Fixed effects 

# 1: Initial Data Manipulation -----------------------------------------------


## Uploading the data
## Set Working directory
## Ma Camila:
setwd("C:/Users/Maria.Arias/OneDrive - Universidad de los andes/MSc Economics/Big Data & Machine Learning/Problem set 2")
## Mario:
## Martín:
## Dani:

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
      


cv_mse <- function(base,fold_size,modelo,...){
  l <- fold_size
  
  db_train <- list()
  db_test <- list()
  
  for(i in 1:l){
    db_train[[i]] <- base %>% filter(fold!=i) 
    db_test[[i]] <- base %>% filter(fold==i)
    
    fit <-lm(modelo, data= db_train[[i]])
    db_test[[i]] <- db_test[[i]] %>% mutate(Pobre_hat = ifelse(predict(fit, newdata=db_test[[i]]) >= 0.5, 1, 0))
  }
  
  MSE <- list()
  for(i in 1:l){
    MSE[[i]] <- mean((db_test[[i]]$Pobre - db_test[[i]]$Pobre_hat)^2, na.rm=TRUE)
  }
  MSE<-do.call(rbind,MSE)
  print(mean(MSE, na.rm = TRUE))
}

## I choose models minimizing MSE
cv_mse(train,k,mod1)

## Precicting and generating prediction file
predictSample <- test %>%
  mutate(pobre_lab = ifelse(predict(lm(mod1, train), newdata=test) >= 0.5, 1, 0)) %>%
  dplyr::select(id,pobre_lab)


head(predictSample)
write.csv(predictSample,"predictions/classification_linearRegression.csv", row.names = FALSE)


## 2.2: ElasticNet

## 2.3: CART - Logit

## 2.4: CART - LDA

#3: INCOME REGRESSION APPROACH -------------------------------------------------



