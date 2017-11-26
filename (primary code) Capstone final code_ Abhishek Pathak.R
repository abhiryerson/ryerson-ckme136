# Import the dataset and renamed the dataset to 'df1'
# It is important to note that for ceratin arrtibutes, where there were NA's, those have been removed
# Not removing them could have potentially caused a bias
# towards other weather patterns which had zero NAs. 

# Characteristics of the Dataset
library(readxl)
df1<- read_excel("C:/Users/user/Downloads/testset1.xlsx")
str(df1)
names(df1)
unique(df1)
class(df1)
head(df1)
tail(df1)
class(df1)
unique(df1$`_conds`)
unique(df1$`_wdire`)
unique(df1$`_tempm`)
unique(df1$`_hum`)

# Graphs

# Temperature vs dewpoint
plot(df1$`_tempm`,df1$`_dewptm`, col = rainbow (4),main = "Temperature vs Dewpoint",xlab = "Temperature", ylab = "Dewpoint")

# Tempreature vs humidity
plot(df1$`_tempm`, df1$`_hum`, col = rainbow(4), main = "Temperature vs Humidity", xlab = "Temperature", ylab = "Humidity")

# Teperatures in New Delhi
hist(df1$`_tempm`, col = rainbow(9), main = "Temperatures in New Delhi", xlab = "Temperature")

# Check the number of NA's within each attribute

# 72 NA's
sum(is.na(df1$`_conds`))

# 0 NA's
sum(is.na(df1$datetime_utc))

# 621 NA's
sum(is.na(df1$`_dewptm`))

# 0 NA's
sum(is.na(df1$`_fog`))

# 0 NA's
sum(is.na(df1$`_hail`))

# 71835 NA's
sum(is.na(df1$`_heatindexm`))

# 143 NA's
sum(is.na(df1$`_hum`))

# 100990 NA's
sum(is.na(df1$`_precipm`))

# 232 NA's
sum(is.na(df1$`_pressurem`))

# 0 NA's
sum(is.na(df1$`_rain`))

# 0 NA's
sum(is.na(df1$`_snow`))

# 673 NA's
sum(is.na(df1$`_tempm`))

# 0 NA's
sum(is.na(df1$`_thunder`))

# 0 NA's
sum(is.na(df1$`_tornado`))

# 4428 NA's
sum(is.na(df1$`_vism`))

# 14775 NA's
sum(is.na(df1$`_wdird`))

# 14775 NA's
sum(is.na(df1$`_wdire`))

# 99918 NA's
sum(is.na(df1$`_wgustm`))

# 100411 NA's 
sum(is.na(df1$`_windchillm`))

# 2358 NA's
sum(is.na(df1$`_wspdm`))


str(df1)

# removed this column because 
colnames(df1)[8]<- "precipm"
df1<- subset( df1, select = -precipm)

colnames(df1)[18]<- "windcm"
df1<- subset( df1, select = -windcm)

colnames(df1)[6]<- "heat1"
df1<- subset(df1, select = -heat1)


colnames(df1)[16]<- "windgust"
df1<- subset(df1, select = -windgust)

colnames(df1)[15]<- "wdire"
df1<- subset( df1, select = -wdire)

df1


str(df1)

# removed NA's from the dataset
row.has.na <- apply(df1, 1, function(x){any(is.na(x))})
sum(row.has.na) #20099 
final.filtered <- df1[!row.has.na,]

df1<-final.filtered

df1
str(df1)
str(df1$`_conds`)
class(df1$`_conds`)
str(df1)
head(df1$` _conds`)


df1$`_conds`<- as.factor(df1$`_conds`)
df1$`_conds`<- as.numeric(df1$`_conds`)

#NA was introduced vy coercion
sum(is.na(df1$`_conds`))
sum(is.na(df1$`_conds`))
names(df1)

df1$date <- substr(df1$datetime_utc, 0, 8)
#df1

df1$time<- substr(df1$datetime_utc, 10, 14)  
#df1
df1$time <- as.numeric(gsub(":","",df1$time))

df1
str(df1)

df1$date<- as.numeric(as.character(df1$date))
head(df1$date)

df1$time<- as.numeric(as.character(df1$time))

sum(is.na(df1$time))
sum(is.na(df1$date))
sum(is.na(df1))
str(df1)

sum(is.na(df1))


# Correlations 
a<- data.frame(df1$`_tempm`, df1$`_dewptm`)
cor(a)
plot(a, main = "Correlation between Temperature and Dewpoint", ylab = "Dewpoint (Celcius)", xlab = "Temperature (Celcius)")

b<- data.frame(df1$date, df1$`_wdird`)
cor(b)
plot(b, main = "Correlation between Date and Wind speed+windgust", ylab = "Wind speed + wind gust (km)", xlab = "Date (yyyymmdd)")

c<- data.frame(df1$time, df1$`_wdird`, df1$`_dewptm`)
cor(c)

d<- data.frame(df1$`_dewptm`, df1$time, df1$date)
cor(d)

str(df1)
sum(is.na(df1))

df1$`_hum`<- as.character(df1$`_hum`)

###UPDATE Total number of records 80891

#For the training and testing sets 70% of the total records will be training
# While the remaining 30% will be atributted towards testing
df1_train<- df1[1:56624,]
df1_test<- df1[56625:80891,]

# *** MODELS which are hastagged were models which had 2-3 attributes in the model.This would cause bias*** 
# Wanted checkout the Muliple R sqaure of these models,
#but just created predicted one model with all the varaibles in it in order not to cause bias.
#Turned out the one model with all the attributes had the highest multiple R squared.

# Model and Predict # 1
#df1_t_c<- lm(`_tempm` ~ `_conds`, data = df1_train)
#summary(df1_t_c)

#predict_df1_t_c<- predict(df1_t_c, newdata = df1_test, interval = "pred")
#summary(predict_df1_t_c)

# Model and Predict # 2
#df1_t_c_d<- lm(`_tempm` ~ `_conds` + `_dewptm`, data = df1_train)
#summary(df1_t_c_d)

#predict_df1_t_c_d<- predict(df1_t_c_d, newdata = df1_test, interval = "pred")
#summary(predict_df1_t_c_d)

# Model and Predict # 3
#df1_t_c_h<- lm(`_tempm` ~ `_conds`+ `_hum`, data = df1_train)
#summary(df1_t_c_h)

#predict_df1_t_c_h<- predict(df1_t_c_h, newdata = df1_test, interval = "pred")
#summary(predict_df1_t_c_h)

# Model and Predict # 4
#df1_t_d<- lm(`_tempm` ~ `_dewptm`, data = df1_train)
#summary(df1_t_d)

#predict_df1_t_d<- predict(df1_t_d, newdata = df1_test, interval = "pred")
#summary(predict_df1_t_d)

# Model and Predict # 5
#df1_t_h<- lm(`_tempm` ~ `_hum`, data = df1_train)
#summary(df1_t_h)

#predict_df1_t_h<- predict(df1_t_h, newdata = df1_test, interval = "pred")
#summary(predict_df1_t_h)


# Model and Predict # 6
#df1_t_d_h<- lm(`_tempm` ~ `_dewptm` + `_hum`, data = df1_train)
#summary(df1_t_d_h)

#predict_df1_t_d_h<- predict(df1_t_d_h, newdata = df1_test, interval = "pred")
#summary(predict_df1_t_d_h)

# Model 

#df1_t_all<- lm(`_tempm`~ `_dewptm` + `_hum` + `_conds` + `_fog` + `_hail` + `_pressurem` + `_rain` + `_snow` + `_thunder` + `_tornado` + `_vism` + `_wdird` + `_wspdm`, data = df1_train)
#summary(df1_t_all)


str(df1)

# Model with all the attributes

df1_t_all_1<- lm(`_tempm`~ `_dewptm`+ `_hum` +`_conds` + `_fog` + `_hail`+ `_pressurem` + `_rain` + `_snow` + `_thunder` + `_tornado`+ `_vism`+ `_wdird` + `_wspdm`+ date + time, data = df1_train)
summary(df1_t_all_1)

predict_df1_all_1<- predict(df1_t_all_1, newdata = df1_test, interval = "pred")
summary(predict_df1_all_1)
plot(predict_df1_all_1, main = "Prediction Model of all attributes")

#Cross validation

library(DAAG)

cv1<- cv.lm(data=df1, df1_t_all_1, m = 5)

#arima model - Times Series
library(forecast)

df4<- df1$`_tempm`[1:80891]

variable<- ts(df4, start=c(1996,11), end = c(2017,4))
fit1<- auto.arima(variable)
fcast1<- forecast(fit1)
plot(fcast1, xlab = "Year", ylab = "Temperature (Celcius)")









