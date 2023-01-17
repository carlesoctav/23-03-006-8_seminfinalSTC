library(tidyverse)
library(psych)
library(dplyr)

setwd("C:/Users/antonius/Downloads")
df <- read_csv('checkpoint_C.csv')
cmap <- c("#63666B", "#29484D", "#F37C2A", "#F68529", "#ED6732")
df_good <- df %>% filter(`CUSTOMER_TYPE`=='good') %>% select(-CUSTOMER_TYPE)
df_bad <- df %>% filter(`CUSTOMER_TYPE`=='bad') %>% select(-CUSTOMER_TYPE)

######## analisis kategorik ########
table <- table(df$CODE_GENDER)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"), main = "Bagan Jenis Kelamin",col = c("mistyrose", "lightblue"))
legend("topright", c("Wanita","Lelaki"), cex = 0.8,
       fill = c("mistyrose", "lightblue"))

table <- table(df$FLAG_OWN_CAR)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"), main = "Bagan Kepemilikan Mobil",col = c("mistyrose", "lightblue"))
legend("topright", c("Tidak Punya","Punya"), cex = 0.8,
       fill = c("mistyrose", "lightblue"))

table <- table(df$FLAG_OWN_REALTY)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"), main = "Bagan Kepemilikan Real Estate",col = c("mistyrose", "lightblue"))
legend("topright", c("Tidak Punya","Punya"), cex = 0.8,
       fill = c("mistyrose", "lightblue"))


table <- table(df$NAME_INCOME_TYPE)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"), main = "Bagan Tipe Penghasilan",col = c("mistyrose", "lightblue","lightblue1", "black","mistyrose2"))
legend("bottomleft", c("Commercial associate","Pensioner", "State Servant", "Student","working"), cex = 0.6,
       fill = c("mistyrose", "lightblue","lightblue1", "black","mistyrose2"))

table <- table(df$NAME_EDUCATION_TYPE)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"), radius=1,cex = 0.5, main = "Bagan Tipe Edukasi",col = c("mistyrose", "mistyrose1","lightblue4", "lightblue1","lightblue2"))
legend("bottomleft", c("Academic degree","Higher education", "Incomplete higher", "Lower secondary","Secondary / secondary special"), cex = 0.45,
       fill = c("mistyrose", "mistyrose1","lightblue4", "lightblue1","lightblue2"))

table <- table(df$NAME_FAMILY_STATUS)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"), main = "Bagan Status Pernikahan",col = c("mistyrose1", "lightblue","mistyrose2", "lightblue1","mistyrose3"))
legend("bottomleft", c("Civil marriage","Married", "Separated", "Single / not married","Widow"), cex = 0.7,
       fill = c("mistyrose1", "lightblue","mistyrose2", "lightblue1","mistyrose3"))

table <- table(df$NAME_HOUSING_TYPE)
table
piepercent<- round(100*table/sum(table), 1)
pie(table, labels = paste0(piepercent, "%"),radius=1,cex = 0.5,main = "Bagan Tipe Rumah",col = c("mistyrose1", "lightblue","mistyrose2", "lightblue1","mistyrose3","lightblue3"))
legend("bottomleft", c("Co-op apartment","House / apartment", "Municipal apartment", "Office apartment","Rented apartment","With parents"), cex = 0.59,
       fill = c("mistyrose1", "lightblue","mistyrose2", "lightblue1","mistyrose3","lightblue3"))

table <- table(df$FLAG_MOBIL)
table

table <- table(df$FLAG_PHONE)
table

table <- table(df$FLAG_EMAIL)
table

table <- table(df$FLAG_WORK_PHONE)
table

table <- table(df$OCCUPATION_TYPE)
table

chisq.test(df$CODE_GENDER, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$FLAG_OWN_CAR, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$FLAG_OWN_REALTY, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$NAME_INCOME_TYPE, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$NAME_EDUCATION_TYPE, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$NAME_FAMILY_STATUS, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$NAME_HOUSING_TYPE, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$FLAG_MOBIL, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$FLAG_WORK_PHONE, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$FLAG_PHONE, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$FLAG_EMAIL, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$OCCUPATION_TYPE, df$CUSTOMER_TYPE, correct=FALSE)
chisq.test(df$CUSTOMER_TYPE, df$CUSTOMER_TYPE, correct=FALSE)

######## analisis numerik ########
describe(df)
boxplot(df$CNT_CHILDREN,
        main = "Bagan Jumlah anak",
        xlab = "Jumlah Anak",
        col = "mistyrose",
        border = "black",
        horizontal = TRUE
)

hist(df$AMT_INCOME_TOTAL, breaks=40, main="Bagan Penghasilan Total",
     xlab="Penghasilan Total",
     xlim=c(50,1000000),
     col="mistyrose",
     freq=TRUE)

table <- table(df$CNT_FAM_MEMBERS)
table

hist(df$AGE_EMPLOYED, breaks=30, main="Bagan Lama Berkerja",
     xlab="Lama Berkerja",
     col="mistyrose",
     freq=TRUE)

hist(df$AGE_BIRTH, breaks=30, main="Bagan Umur Customer",
     xlab="Umur Customer",
     col="mistyrose",
     freq=TRUE)

mydata.cor = cor(df)

summary(aov(CODE_GENDER~CUSTOMER_TYPE, data=df))
summary(aov(FLAG_OWN_CAR~CUSTOMER_TYPE, data=df))
summary(aov(FLAG_OWN_REALTY~CUSTOMER_TYPE, data=df))
summary(aov(NAME_INCOME_TYPE~CUSTOMER_TYPE, data=df))
summary(aov(NAME_EDUCATION_TYPE~CUSTOMER_TYPE, data=df))
summary(aov(NAME_FAMILY_STATUS~CUSTOMER_TYPE, data=df))
summary(aov(NAME_HOUSING_TYPE~CUSTOMER_TYPE, data=df))
summary(aov(FLAG_MOBIL~CUSTOMER_TYPE, data=df))
summary(aov(FLAG_WORK_PHONE~CUSTOMER_TYPE, data=df))
summary(aov(FLAG_PHONE~CUSTOMER_TYPE, data=df))
summary(aov(CNT_FAM_MEMBERS~CUSTOMER_TYPE, data=df))
summary(aov(AGE_BIRTH~CUSTOMER_TYPE, data=df))
summary(aov(AMT_INCOME_TOTAL~CUSTOMER_TYPE, data=df))

kruskal.test(df$AMT_INCOME_TOTAL, df$AGE_BIRTH)
kruskal.test(df$AGE_BIRTH, df$CUSTOMER_TYPE)
kruskal.test(df$CNT_FAM_MEMBERS, df$CUSTOMER_TYPE)
