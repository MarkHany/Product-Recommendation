library(dplyr)
library(ggplot2)
library(data.table)


#read Data
df<- fread("train_ver2.csv",nrows=1000000)
str(df)
summary(df)

#GetUniqueIDs and make a sample of the dataset of 350,000 unique Customer
uniqueId <- unique(df$ncodpers)
length(uniqueId)
uniqueId <- uniqueId[sample(length(uniqueId),350000)]

df <- df[df$ncodpers %in% uniqueId]
str(df)
summary(df)

#Preprocessing and Preparing Data
sapply(df,function(x)any(is.na(x)))

#age, ind_nuevo, antiguedad, indrel, indrel_1mes, tipodom, cod_prov, ind_nomina_ult1 and ind_nom_pens_ult1 are TRUE to have nulls

#Data Cleaning

#Age Cleaning
summary(df$age)
sum(is.na(df$age))
df$age[(df$age<18)] <- 18
df$age[(df$age>100)] <- mean(df$age[(df$age>18) & (df$age<100)],na.rm = TRUE)
df$age[is.na(df$age)] <- median(df$age,na.rm = TRUE)
df$age <- round(df$age)

summary(df$age)

ggplot(data=df,aes(x=age)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(18,100)) + ggtitle("Age Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#ind_nuevo Cleaning
sum(is.na(df$ind_nuevo))

df$ind_nuevo[is.na(df$ind_nuevo)] <- 1
summary(df$ind_nuevo)


#antiguedad Cleaning
summary(df$antiguedad)
df$antiguedad[df$antiguedad<0]<- 0
df$antiguedad[is.na(df$antiguedad)]<- min(df$antiguedad,na.rm=TRUE)
summary(df$antiguedad)

#indrel Cleaning
summary(df$indrel)

table(df$indrel)

df$indrel[is.na(df$indrel)] <- 1

table(df$indrel)

#indrel_1mes

table(df$indrel_1mes)
summary(df$indrel_1mes)
df$indrel_1mes[is.na(df$indrel_1mes)] <- 1

table(df$indrel_1mes)



#tipodom Cleaning
#all elements are 1
#No need for it

df <- df %>% select(-tipodom)


#cod_prov Cleaning
#The Codes of the province but No need for it as we have nom_prov (The Name of the province)

df <- df %>% select(-cod_prov)



sapply(df,function(x)any(is.na(x)))


#ind_actividad_cliente Cleaning
sum(is.na(df$ind_actividad_cliente))
table(df$ind_actividad_cliente)
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- median(df$ind_actividad_cliente,na.rm = TRUE)
table(df$ind_actividad_cliente)



#renta Cleaning
sum(is.na(df$renta))
summary(df$renta)
df$renta[is.na(df$renta)] <- mean(df$renta,na.rm = TRUE)
summary(df$renta)



sapply(df,function(x)any(is.na(x)))

#ind_nomina_ult1 Cleaning
table(df$ind_nomina_ult1)
sum(is.na(df$ind_nomina_ult1))
df$ind_nomina_ult1[is.na(df$ind_nomina_ult1)] <- 0
table(df$ind_nomina_ult1)

#ind_nom_pens_ult1
table(df$ind_nom_pens_ult1)
sum(is.na(df$ind_nom_pens_ult1))
df$ind_nom_pens_ult1[is.na(df$ind_nom_pens_ult1)] <- 0
table(df$ind_nom_pens_ult1)

