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
       fixest)  # Fixed effects 

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

#2: CLASSIFICATION APPROACH ----------------------------------------------------

## 2.1: Linear Regression

## 2.2: ElasticNet

## 2.3: CART - Logit

## 2.4: CART - LDA

#3: INCOME REGRESSION APPROACH -------------------------------------------------



