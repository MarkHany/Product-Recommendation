library(dplyr)
library(ggplot2)
library(data.table)
library(arules)
library(xgboost)
library(caret)
library(mlr)
library(OpenML)

rm(list = ls())
#read Data
df<- fread("D:/Mark/Study/GP/Product Recommendation/Product Recommendation/train_ver2.csv",nrows=1000000)
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

#age, ind_nuevo, antiguedad, indrel, indrel_1mes, tipodom, cod_prov, ind_nomina_ult1 and ind_nom_pens_ult1 are "TRUE" to have nulls

#Data Cleaning

#Age Cleaning
summary(df$age)
sum(is.na(df$age))
df$age[(df$age<18)] <- 18
df$age[(df$age>100)] <- mean(df$age[(df$age>18) & (df$age<100)],na.rm = "TRUE")
df$age[is.na(df$age)] <- median(df$age,na.rm = "TRUE")
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
df$antiguedad[is.na(df$antiguedad)]<- min(df$antiguedad,na.rm="TRUE")
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
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- median(df$ind_actividad_cliente,na.rm = "TRUE")
table(df$ind_actividad_cliente)



#renta Cleaning
sum(is.na(df$renta))
summary(df$renta)
df$renta[is.na(df$renta)] <- mean(df$renta,na.rm = "TRUE")
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


df$pais_residencia[df$pais_residencia==""] <- "UNKNOWN"
df$sexo[df$sexo==""]                       <- "UNKNOWN"
df$ult_fec_cli_1t[df$ult_fec_cli_1t==""]   <- "UNKNOWN"
df$ind_empleado[df$ind_empleado==""]       <- "UNKNOWN"
df$indext[df$indext==""]                   <- "UNKNOWN"
df$indresi[df$indresi==""]                 <- "UNKNOWN"
df$conyuemp[df$conyuemp==""]               <- "UNKNOWN"
df$segmento[df$segmento==""]               <- "UNKNOWN"
df$nomprov[df$nomprov==""]                 <- "UNKNOWN"
df$tiprel_1mes[df$tiprel_1mes==""]         <- "UNKNOWN"
df$indfall[df$indfall==""]                 <- "N"

#Remove ult_fec_cli_1t as almost all dates are unknown
df <- df %>% select(-ult_fec_cli_1t)

#Remove conyuemp as almost all records are unknown
df <- df %>% select(-conyuemp)


df$canal_entrada[df$canal_entrada==""]               <- "UNKNOWN"

####Data Visualisation####

str(df)

plot(df$age,df$renta,xlab = "Age",ylab = "Renta",type = "h")

plot(df$age,df$ind_cco_fin_ult1)

#Current Accounts
ggplot(data=df,aes(x=ind_cco_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Current Accounts Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Savings Accounts
ggplot(data=df,aes(x=ind_ahor_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Saving  Accounts Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Guarantees
ggplot(data=df,aes(x=ind_aval_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Guarantees Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Derivada Account
ggplot(data=df,aes(x=ind_cder_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Derivada Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Payroll Account
ggplot(data=df,aes(x=ind_cno_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Payroll Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Junior Account
ggplot(data=df,aes(x=ind_ctju_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Junior Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Mas Particular Account
ggplot(data=df,aes(x=ind_ctma_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Más particular Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Particular Account
ggplot(data=df,aes(x=ind_ctop_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("particular Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Particular Plus Account
ggplot(data=df,aes(x=ind_ctpp_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("particular Plus Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Short Term Deposits
ggplot(data=df,aes(x=ind_deco_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Short-term deposits Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Medium Term Deposits
ggplot(data=df,aes(x=ind_deme_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Medium-term deposits Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Long Term Deposits
ggplot(data=df,aes(x=ind_dela_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Long-term deposits Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#E-account
ggplot(data=df,aes(x=ind_ecue_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("e-account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Funds
ggplot(data=df,aes(x=ind_fond_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Funds Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Mortgage
ggplot(data=df,aes(x=ind_hip_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Mortgage Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Pensions
ggplot(data=df,aes(x=ind_plan_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Pensions Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Loans
ggplot(data=df,aes(x=ind_pres_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Loans Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Taxes
ggplot(data=df,aes(x=ind_reca_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Taxes Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Credit Card
ggplot(data=df,aes(x=ind_tjcr_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Credit Card Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Securities
ggplot(data=df,aes(x=ind_valo_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Securities Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Home Account
ggplot(data=df,aes(x=ind_viv_fin_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Home Account Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Payroll
ggplot(data=df,aes(x=ind_nomina_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Payroll Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Pensions
ggplot(data=df,aes(x=ind_nom_pens_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Pensions Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Direct Debit
ggplot(data=df,aes(x=ind_recibo_ult1)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Direct Debit Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Customer Activity
ggplot(data=df,aes(x=ind_actividad_cliente)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("Customer Activity Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Primary Customer
ggplot(data=df,aes(x=indrel)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,100)) + ggtitle("Primary Customer Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Customer seniority (in months)
ggplot(data=df,aes(x=antiguedad)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,100)) + ggtitle("Customer seniority Distribution (in months)") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#New customer Index. 1 if the customer registered in the last 6 months.
ggplot(data=df,aes(x=ind_nuevo)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,2)) + ggtitle("New customer Index Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Gross income of the household
ggplot(data=df,aes(x=renta)) + geom_bar(alpha=0.75,fill="tomato",color="black") + xlim(c(-1,250000)) + ggtitle("Gross Income Distribution") + 
  theme_bw() +theme(axis.title=element_text(size=24),plot.title=element_text(size=36),axis.text =element_text(size=16))

#Province Distribution
ggplot(df, aes(nomprov, fill = nomprov)) + geom_bar()+
  labs(title = "Province Distribution", x = "Province name", y = "Count of Customers")

#Customer Type Distribution: Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
ggplot(df, aes(indrel_1mes, fill = indrel_1mes)) + geom_bar()+
  labs(title = "Customer Type Distribution", x = "Customer Type", y = "Count of Customers")

#Customer relationship Type Distribution: Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)
ggplot(df, aes(tiprel_1mes, fill = tiprel_1mes)) + geom_bar()+
  labs(title = "Customer relationship Type Distribution", x = "Customer relationship Type", y = "Count of Customers")

#Country Distribution
ggplot(df, aes(pais_residencia, fill = pais_residencia)) + geom_bar()+
  labs(title = "Country Distribution", x = "Country Code", y = "Count of Customers")

#Gender Distribution
ggplot(df, aes(sexo, fill = sexo)) + geom_bar()+
  labs(title = "Gender Distribution", x = "Gender", y = "Count of Customers")

#Employee index
ggplot(df, aes(ind_empleado, fill = ind_empleado)) + geom_bar()+
  labs(title = "Employee index Distribution", x = "Index", y = "Count of Customers")

#Segmentation Distribution
ggplot(df, aes(segmento, fill = segmento)) + geom_bar()+
  labs(title = "Segmentation Distribution", x = "Segmentation name", y = "Count of Customers")

#Date of Transaction Distribution
ggplot(df, aes(fecha_dato, fill = fecha_dato)) + geom_bar()+
  labs(title = "Date of Transaction Distribution", x = "Date", y = "Count of Customers")

#Date of Being Customer
ggplot(df, aes(fecha_alta, fill = fecha_alta)) + geom_bar()+
  labs(title = "Date of Being Customer Distribution", x = "Date", y = "Count of Customers")

#Last date as primary customer (if he isn't at the end of the month)
ggplot(df, aes(ult_fec_cli_1t, fill = ult_fec_cli_1t)) + geom_bar()+
  labs(title = "Last date as primary customer Distribution", x = "Date", y = "Count of Customers")

#Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)
ggplot(df, aes(indresi, fill = indresi)) + geom_bar()+
  labs(title = "Residence Index Distribution", x = "Index", y = "Count of Customers")

#Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)
ggplot(df, aes(indext, fill = indext)) + geom_bar()+
  labs(title = "Foreigner Index Distribution", x = "Index", y = "Count of Customers")

#Spouse index. 1 if the customer is spouse of an employee
ggplot(df, aes(conyuemp, fill = conyuemp)) + geom_bar()+
  labs(title = "Spouse Index Distribution", x = "Index", y = "Count of Customers")

#channel used by the customer to join
ggplot(df, aes(canal_entrada, fill = canal_entrada)) + geom_bar()+
  labs(title = "Channel Distribution", x = "Channel", y = "Count of Customers")

#Deceased index. N/S
ggplot(df, aes(indfall, fill = indfall)) + geom_bar()+
  labs(title = "Deceased Index Distribution", x = "Index", y = "Count of Customers")


##############



#Relation of Age with Customer Seniority
plot(df$age,df$antiguedad,xlab = "Age",ylab = "Customer Seniority(in months)")


table(df$tiprel_1mes)
table(df$indrel_1mes)
table(df$nomprov)


#Month can be a feature which can help us
df$month <- month(df$fecha_dato)


#
plot(df$antiguedad,df$ind_aval_fin_ult1,type = "h",xlab = "Seniority Level (in months)",ylab = "Guarantees")

#This shows that the low seniority level doesn't buy savings accounts
plot(df$antiguedad,df$ind_ahor_fin_ult1,type = "h",xlab = "Seniority Level (in months)",ylab = "Saving Accounts")


#
ggplot(df, aes(fecha_dato, antiguedad)) + geom_boxplot(fill = "red")+
  scale_y_continuous("antiguedad", breaks= seq(0,250, by=25))+
  labs(title = "Box Plot", x = "Date")


#Date with Current Accounts
ggplot(df, aes(fecha_dato, ind_cco_fin_ult1)) + geom_bar(stat = "identity", fill = "darkblue") + 
  scale_x_discrete("Date")+ 
  scale_y_discrete("Current Accounts")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title = "Bar Chart")

# Idea of having each product as a target variable using (*logistic regression*)


#Output Cleaned File
write.csv(df,file="Clean_Train_Data.csv")



#Read Cleaned File
#cleandf <- read.transactions("Clean_Train_Data.csv",sep=",")
#inspect(cleandf)
#itemFrequency(cleandf)



#Machine Learning Part

#Training

#Preparing the task

Training_df <- df %>% select(-fecha_dato,-fecha_alta)
Training_df <- Training_df %>% select(-conyuemp)
Training_df <- Training_df %>% select(-pais_residencia)
Training_df <- Training_df %>% select(-canal_entrada)
Training_df <- Training_df %>% select(-ncodpers)
set.seed(2000)

labels = colnames(Training_df)[16:39]
labels


Training_df$ind_ahor_fin_ult1 <- as.logical(Training_df$ind_ahor_fin_ult1)
Training_df$ind_aval_fin_ult1 <- as.logical(Training_df$ind_aval_fin_ult1)
Training_df$ind_cco_fin_ult1 <- as.logical(Training_df$ind_cco_fin_ult1)
Training_df$ind_cder_fin_ult1 <- as.logical(Training_df$ind_cder_fin_ult1)
Training_df$ind_cno_fin_ult1 <- as.logical(Training_df$ind_cno_fin_ult1)
Training_df$ind_ctju_fin_ult1 <- as.logical(Training_df$ind_ctju_fin_ult1)
Training_df$ind_ctma_fin_ult1 <- as.logical(Training_df$ind_ctma_fin_ult1)
Training_df$ind_ctop_fin_ult1 <- as.logical(Training_df$ind_ctop_fin_ult1)
Training_df$ind_ctpp_fin_ult1 <- as.logical(Training_df$ind_ctpp_fin_ult1)
Training_df$ind_deco_fin_ult1 <- as.logical(Training_df$ind_deco_fin_ult1)
Training_df$ind_deme_fin_ult1 <- as.logical(Training_df$ind_deme_fin_ult1)
Training_df$ind_dela_fin_ult1 <- as.logical(Training_df$ind_dela_fin_ult1)
Training_df$ind_ecue_fin_ult1 <- as.logical(Training_df$ind_ecue_fin_ult1)
Training_df$ind_fond_fin_ult1 <- as.logical(Training_df$ind_fond_fin_ult1)
Training_df$ind_hip_fin_ult1 <- as.logical(Training_df$ind_hip_fin_ult1)
Training_df$ind_plan_fin_ult1 <- as.logical(Training_df$ind_plan_fin_ult1)
Training_df$ind_pres_fin_ult1 <- as.logical(Training_df$ind_pres_fin_ult1)
Training_df$ind_reca_fin_ult1 <- as.logical(Training_df$ind_reca_fin_ult1)
Training_df$ind_tjcr_fin_ult1 <- as.logical(Training_df$ind_tjcr_fin_ult1)
Training_df$ind_valo_fin_ult1 <- as.logical(Training_df$ind_valo_fin_ult1)
Training_df$ind_viv_fin_ult1 <- as.logical(Training_df$ind_viv_fin_ult1)
Training_df$ind_nomina_ult1 <- as.logical(Training_df$ind_nomina_ult1)
Training_df$ind_nom_pens_ult1 <- as.logical(Training_df$ind_nom_pens_ult1)
Training_df$ind_recibo_ult1 <- as.logical(Training_df$ind_recibo_ult1)

Training_df$ind_empleado <- as.factor(Training_df$ind_empleado)
Training_df$sexo <- as.factor(Training_df$sexo)
Training_df$tiprel_1mes <- as.factor(Training_df$tiprel_1mes)
Training_df$indresi <- as.factor(Training_df$indresi)
Training_df$indext <- as.factor(Training_df$indext)
Training_df$indfall <- as.factor(Training_df$indfall)
Training_df$nomprov <- as.factor(Training_df$nomprov)
Training_df$segmento <- as.factor(Training_df$segmento)
Training_df$ind_actividad_cliente <- as.factor(Training_df$ind_actividad_cliente)
Training_df$ind_nuevo <- as.factor(Training_df$ind_nuevo)
Training_df$indrel <- as.factor(Training_df$indrel)
Training_df$indrel_1mes <- as.factor(Training_df$indrel_1mes)




Training_df$ind_ahor_fin_ult1[Training_df$ind_ahor_fin_ult1== 1] <- "TRUE"
Training_df$ind_ahor_fin_ult1[Training_df$ind_ahor_fin_ult1== 0] <- "FALSE"
Training_df$ind_aval_fin_ult1[Training_df$ind_aval_fin_ult1== 1] <- "TRUE"
Training_df$ind_aval_fin_ult1[Training_df$ind_aval_fin_ult1== 0] <- "FALSE"
Training_df$ind_cco_fin_ult1[Training_df$ind_cco_fin_ult1== 1] <- "TRUE"
Training_df$ind_cco_fin_ult1[Training_df$ind_cco_fin_ult1== 0] <- "FALSE"
Training_df$ind_cder_fin_ult1[Training_df$ind_cder_fin_ult1== 1] <- "TRUE"
Training_df$ind_cder_fin_ult1[Training_df$ind_cder_fin_ult1== 0] <- "FALSE"
Training_df$ind_cno_fin_ult1[Training_df$ind_cno_fin_ult1== 1] <- "TRUE"
Training_df$ind_cno_fin_ult1[Training_df$ind_cno_fin_ult1== 0] <- "FALSE"
Training_df$ind_ctju_fin_ult1[Training_df$ind_ctju_fin_ult1== 1] <- "TRUE"
Training_df$ind_ctju_fin_ult1[Training_df$ind_ctju_fin_ult1== 0] <- "FALSE"
Training_df$ind_ctma_fin_ult1[Training_df$ind_ctma_fin_ult1== 1] <- "TRUE"
Training_df$ind_ctma_fin_ult1[Training_df$ind_ctma_fin_ult1== 0] <- "FALSE"
Training_df$ind_ctop_fin_ult1[Training_df$ind_ctop_fin_ult1== 1] <- "TRUE"
Training_df$ind_ctop_fin_ult1[Training_df$ind_ctop_fin_ult1== 0] <- "FALSE"
Training_df$ind_ctpp_fin_ult1[Training_df$ind_ctpp_fin_ult1== 1] <- "TRUE"
Training_df$ind_ctpp_fin_ult1[Training_df$ind_ctpp_fin_ult1== 0] <- "FALSE"
Training_df$ind_deco_fin_ult1[Training_df$ind_deco_fin_ult1== 1] <- "TRUE"
Training_df$ind_deco_fin_ult1[Training_df$ind_deco_fin_ult1== 0] <- "FALSE"
Training_df$ind_dela_fin_ult1[Training_df$ind_dela_fin_ult1== 1] <- "TRUE"
Training_df$ind_dela_fin_ult1[Training_df$ind_dela_fin_ult1== 0] <- "FALSE"
Training_df$ind_deme_fin_ult1[Training_df$ind_deme_fin_ult1== 1] <- "TRUE"
Training_df$ind_deme_fin_ult1[Training_df$ind_deme_fin_ult1== 0] <- "FALSE"
Training_df$ind_ecue_fin_ult1[Training_df$ind_ecue_fin_ult1== 1] <- "TRUE"
Training_df$ind_ecue_fin_ult1[Training_df$ind_ecue_fin_ult1== 0] <- "FALSE"
Training_df$ind_fond_fin_ult1[Training_df$ind_fond_fin_ult1== 1] <- "TRUE"
Training_df$ind_fond_fin_ult1[Training_df$ind_fond_fin_ult1== 0] <- "FALSE"
Training_df$ind_hip_fin_ult1[Training_df$ind_hip_fin_ult1== 1] <- "TRUE"
Training_df$ind_hip_fin_ult1[Training_df$ind_hip_fin_ult1== 0] <- "FALSE"
Training_df$ind_nom_pens_ult1[Training_df$ind_nom_pens_ult1== 1] <- "TRUE"
Training_df$ind_nom_pens_ult1[Training_df$ind_nom_pens_ult1== 0] <- "FALSE"
Training_df$ind_plan_fin_ult1[Training_df$ind_plan_fin_ult1== 1] <- "TRUE"
Training_df$ind_plan_fin_ult1[Training_df$ind_plan_fin_ult1== 0] <- "FALSE"
Training_df$ind_pres_fin_ult1[Training_df$ind_pres_fin_ult1== 1] <- "TRUE"
Training_df$ind_pres_fin_ult1[Training_df$ind_pres_fin_ult1== 0] <- "FALSE"
Training_df$ind_reca_fin_ult1[Training_df$ind_reca_fin_ult1== 1] <- "TRUE"
Training_df$ind_reca_fin_ult1[Training_df$ind_reca_fin_ult1== 0] <- "FALSE"
Training_df$ind_recibo_ult1[Training_df$ind_recibo_ult1== 1] <- "TRUE"
Training_df$ind_recibo_ult1[Training_df$ind_recibo_ult1== 0] <- "FALSE"
Training_df$ind_nomina_ult1[Training_df$ind_nomina_ult1== 1] <- "TRUE"
Training_df$ind_nomina_ult1[Training_df$ind_nomina_ult1== 0] <- "FALSE"
Training_df$ind_tjcr_fin_ult1[Training_df$ind_tjcr_fin_ult1== 1] <- "TRUE"
Training_df$ind_tjcr_fin_ult1[Training_df$ind_tjcr_fin_ult1== 0] <- "FALSE"
Training_df$ind_valo_fin_ult1[Training_df$ind_valo_fin_ult1== 1] <- "TRUE"
Training_df$ind_valo_fin_ult1[Training_df$ind_valo_fin_ult1== 0] <- "FALSE"
Training_df$ind_viv_fin_ult1[Training_df$ind_viv_fin_ult1== 1] <- "TRUE"
Training_df$ind_viv_fin_ult1[Training_df$ind_viv_fin_ult1== 0] <- "FALSE"

sapply(Training_df,function(x)any(is.na(x)))

df.task = makeMultilabelTask(id="multi",data = Training_df,target = labels)
df.task


#I chose the classifier chain approach together with a decision tree for the binary classification problems

binary.learner = makeLearner("classif.rpart")
lrncc = makeMultilabelClassifierChainsWrapper(binary.learner)


#Train & Predict####

n = getTaskSize(df.task)
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)

df.mod.cc = train(lrncc, df.task, subset = train.set)
df.pred.cc = predict(df.mod.cc, task = df.task, subset = test.set)
df.pred.cc


performance(df.pred.cc)
listMeasures("multilabel")

performance(df.pred.cc, measures = list(featperc, multilabel.subset01, multilabel.f1, multilabel.acc, multilabel.ppv,multilabel.tpr,timetrain,timeboth,timepredict),model = df.mod.cc)


#Trying Binary Relevance

lrnbr = makeMultilabelBinaryRelevanceWrapper(binary.learner)

df.mod.br = train(lrnbr, df.task, subset = train.set)
df.pred.br = predict(df.mod.br, task = df.task, subset = test.set)

df.pred.br

performance(df.pred.br, measures = list(featperc,multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc, multilabel.tpr, timeboth),model = df.mod.br)

rdesc = makeResampleDesc("Subsample", iters = 3, split = 2/3)
r = resample(lrncc, df.task, rdesc, measures = multilabel.subset01)




#Data Visualization After Prediction


#Draft
sum(df$month=="1" & df$ind_cco_fin_ult1) - sum(df$month=="2" & df$ind_cco_fin_ult1)
sum(df$month=="2" & df$ind_cco_fin_ult1)
