rm(list = ls())

df_train = read.csv("housing_train.csv",stringsAsFactors = F)
df_test = read.csv("housing_test.csv",stringsAsFactors = F)
df_test$Price = NA

#Labelling them as test, train to identify later.
df_train$data = 'train'
df_test$data = 'test'

df_all = rbind(df_train,df_test)

library(dplyr)
glimpse(df_all)
summary(df_all)

#Back up data
df_all_backup = df_all

#Outliers n continuous cariables
outlier_limits = function(x,k){
  x_q1 = quantile(x,na.rm = T)[2]
  x_q3 = quantile(x,na.rm = T)[4]
  x_iqr = IQR(x,na.rm = T)
  ll = x_q1-k*x_iqr
  ul = x_q3+k*x_iqr
  x_limits = c(ll,ul)
  names(x_limits) = NULL
  return(x_limits)
}
#Outliers in Price
ol_price = outlier_limits(df_all$Price,1.5)
ol_price
hist(df_all$Price,breaks = 50)
boxplot(df_all$Price)
which(df_all$Price<ol_price[1])
sum(df_all$Price>ol_price[2],na.rm = T)
df_all$Price[which(df_all$Price>ol_price[2])] = ol_price[2] # CAPPING

#Outliers in LandSize
ol_Landsize = outlier_limits(df_all$Landsize,1.5)
ol_Landsize
hist(df_all$Landsize,breaks = 50)
boxplot(df_all$Landsize)
which(df_all$Landsize<ol_Landsize[1])
sum(df_all$Landsize>ol_Landsize[2],na.rm = T)
df_all$Landsize[which(df_all$Landsize>ol_Landsize[2])] = ol_Landsize[2] # CAPPING

#Outliers in Building Area
ol_BA = outlier_limits(df_all$BuildingArea,1.5)
ol_BA
hist(df_all$BuildingArea,breaks = 50)
boxplot(df_all$BuildingArea)
which(df_all$BuildingArea<ol_BA[1])
sum(df_all$BuildingArea>ol_BA[2],na.rm = T)
df_all$BuildingArea[which(df_all$BuildingArea>ol_BA[2])] = ol_BA[2] # CAPPING

#Outliers in BedRoom2
table(df_all$Bedroom2)
ol_Bedroom = outlier_limits(df_all$Bedroom2,1.5)
ol_Bedroom
hist(df_all$Bedroom2,breaks = 50)
boxplot(df_all$Bedroom2)
which(df_all$Bedroom2<ol_Bedroom[1])
sum(df_all$Bedroom2>ol_Bedroom[2],na.rm = T)
df_all$Bedroom2[which(df_all$Bedroom2>ol_Bedroom[2])] = 4 # CAPPING

#Outliers in Bathrooms
ol_Bath = outlier_limits(df_all$Bathroom,1.5)
ol_Bath
hist(df_all$Bathroom,breaks = 50)
boxplot(df_all$Bathroom)
which(df_all$Bathroom<ol_Bath[1])
sum(df_all$Bathroom>ol_Bath[2],na.rm = T)
df_all$Bathroom[which(df_all$Bathroom>ol_Bath[2])] = 3 # CAPPING

#outliers in Rooms
ol_Rooms = outlier_limits(df_all$Rooms,1.5)
ol_Rooms
hist(df_all$Rooms,breaks = 50)
boxplot(df_all$Rooms)
which(df_all$Rooms<ol_Rooms[1])
sum(df_all$Rooms>ol_Rooms[2],na.rm = T)
df_all$Rooms[which(df_all$Rooms>ol_Rooms[2])] = 4 # CAPPING

#Outliers in cars
ol_car = outlier_limits(df_all$Car,1.5)
ol_car
hist(df_all$Car,breaks = 50)
boxplot(df_all$Car)
which(df_all$Car<ol_car[1])
sum(df_all$Car>ol_car[2],na.rm = T)
df_all$Car[which(df_all$Car>ol_car[2])] =  3# CAPPING


#checking for missing value
na_df = data.frame(sapply(df_all,function(x) sum(is.na(x))))
na_df

#Imputing missing values

#Imputing bathroom NA using median.
df_all$Bathroom = ifelse(is.na(df_all$Bathroom),median(df_all$Bathroom,na.rm = T),df_all$Bathroom)

#Median/mode imputation for NA values for Bedroom
med_Bedroom = median(df_all$Bedroom2,na.rm = T)

#Bedroom
df_all$Bedroom2 = ifelse(is.na(df_all$Bedroom2),med_Bedroom,df_all$Bedroom2)
df_all[is.na(df_all$Bedroom2),]

#Building Area
# df_all[is.na(df_all$BuildingArea),'BuildingArea']
table(df_all$Rooms)
#mean bulding area for 1 room
avg_ba_1 = mean(df_all[df_all$Rooms==1,"BuildingArea"],na.rm = T)
#imputing area of 1room of BA
df_all[df_all$Rooms==1 & is.na(df_all$BuildingArea),"BuildingArea"] = avg_ba_1
#mean bulding area for 2 room
avg_ba_2 = mean(df_all[df_all$Rooms==2,"BuildingArea"],na.rm = T)

#imputing area of 2room of BA
df_all[df_all$Rooms==2 & is.na(df_all$BuildingArea),"BuildingArea"] = avg_ba_2

#mean bulding area for 3 room
avg_ba_3 = mean(df_all[df_all$Rooms==3,"BuildingArea"],na.rm = T)

#imputing area of 3room of BA
df_all[df_all$Rooms==3 & is.na(df_all$BuildingArea),"BuildingArea"] = avg_ba_3
#mean bulding area for 4 room
avg_ba_4 = mean(df_all[df_all$Rooms==4,"BuildingArea"],na.rm = T)

#imputing area of 4room of BA
df_all[df_all$Rooms==4 & is.na(df_all$BuildingArea),"BuildingArea"] = avg_ba_4


#LANDSIZE
df_all[is.na(df_all$Landsize),]
#mean Land Size for bilding area with 1 room
avg_ls_1 = mean(df_all[df_all$BuildingArea==avg_ba_1,"Landsize"],na.rm = T)
#imputing land size of BA having 1 room
df_all[df_all$BuildingArea==avg_ba_1 & is.na(df_all$Landsize),"Landsize"] = avg_ls_1


#mean Land Size for bilding area with 2 room
avg_ls_2 = mean(df_all[df_all$BuildingArea==avg_ba_2,"Landsize"],na.rm = T)
#imputing land size of BA having 2 room
df_all[df_all$BuildingArea==avg_ba_2 & is.na(df_all$Landsize),"Landsize"] = avg_ls_2

#mean Land Size for bilding area with 3 room
avg_ls_3 = mean(df_all[df_all$BuildingArea==avg_ba_3,"Landsize"],na.rm = T)
#imputing land size of BA having 3 room
df_all[df_all$BuildingArea==avg_ba_3 & is.na(df_all$Landsize),"Landsize"] = avg_ls_2

#mean Land Size for bilding area with 4 room
avg_ls_4 = mean(df_all[df_all$BuildingArea==avg_ba_4,"Landsize"],na.rm = T)
#imputing land size of BA having 4 room
df_all[df_all$BuildingArea==avg_ba_4 & is.na(df_all$Landsize),"Landsize"] = avg_ls_4


#CARS
table(df_all$Car)
median_car = median(df_all$Car,na.rm = T)

df_all$Car = ifelse(is.na(df_all$Car),median_car,df_all$Car)

#YearBuilt

median_yearbuilt = median(df_all$YearBuilt,na.rm = T)

df_all$YearBuilt = ifelse(is.na(df_all$YearBuilt),median_yearbuilt,df_all$YearBuilt)

#Coding dummy variables 
glimpse(df_all)

#Binning the Categorical variables
#Encoding 'suburb' as Low,medium,high price areas 
unique(df_all$Suburb)

df_suburb = data.frame(sort(tapply(df_all$Price, df_all$Suburb, mean,na.rm=T)))
suburb_names = rownames(df_suburb)

df_all$Suburb = case_when(df_all$Suburb %in% (suburb_names[1:48]) ~ 'Low Price Suburb',
          df_all$Suburb %in% (suburb_names[49:97]) ~ 'Medium Price Suburb',
          TRUE ~ 'High Price Suburb')

#Encoding 'SellerG' as Low, Medium, High price sellers

df_sellers = data.frame(sort(tapply(df_all$Price,df_all$SellerG,mean,na.rm=T)))
seller_names = row.names(df_sellers) 

df_all$SellerG = case_when(df_all$SellerG %in% seller_names[1:60] ~ 'Low Price Seller',
          df_all$SellerG %in% seller_names[61:120] ~ 'Mediun Price Seller',
          TRUE ~ 'High Price Seller')

#Encoding CouncilArea in Low, Medium, High, Extreme Price CA's.
unique(df_all$CouncilArea)
df_all[df_all$CouncilArea=='','CouncilArea'] = 'Random Area'
df_councilArea = data.frame(sort(tapply(df_all$Price, df_all$CouncilArea, mean, na.rm=T)))
CouncilArea_names = row.names(df_councilArea)

df_all$CouncilArea = case_when(df_all$CouncilArea %in% CouncilArea_names[1:5] ~ 'Low price CA',
          df_all$CouncilArea %in% CouncilArea_names[6:10] ~ 'Medium Price CA',
          df_all$CouncilArea %in% CouncilArea_names[11:15] ~ 'High price CA',
          TRUE ~ 'Extreme Price CA')

#Converting postcode into Categorical Variable then Binning into Low, Medium, High PC
unique(df_all$Postcode)
df_all$Postcode = as.character(df_all$Postcode)
df_postcode = data.frame(sort(tapply(df_all$Price,df_all$Postcode,mean,na.rm=T)))
post_codes = row.names(df_postcode)

df_all$Postcode = case_when(df_all$Postcode %in% post_codes[1:30] ~ 'Low PC',
          df_all$Postcode %in% post_codes[31:60] ~ 'Medium PC',
          TRUE ~ 'High PC')

#Dummy Coding the Categorical variables:

CreateDummies1 = function(data,var,freq_cutoff = 100){
  t = table(data[,var])
  t = t[t > freq_cutoff]
  t = sort(t)
  categories = names(t)[-1]
  
  for(cat in categories){
    name = paste(var,cat,sep='_')
    name = gsub(" ","",name)
    name = gsub("-","_",name)
    name = gsub("\\?","Q",name)
    name = gsub("<","LT_",name)
    name = gsub("\\+","",name)
    name = gsub(">","GT_",name)
    name = gsub("=","EQ_",name)
    name = gsub(",","",name)
    name = gsub("/","_",name)
    
    data[,name] = as.numeric(data[,var]==cat)
  }
  data[,var] = NULL
  return(data)
}

#Extracting all the charecrer variables
cat_cols = names(df_all)[sapply(df_all, is.character)]
#removing data & Address as they are not needed.
cat_cols = cat_cols[!cat_cols %in% c('data','Address')]

#Using the Create Dummies function to apply to these columns.

for(i in cat_cols){
  df_all = CreateDummies1(df_all,i,50)
}
glimpse(df_all)

#Deleting unwanted variables
ls()
rm(avg_ba_1,avg_ba_2,avg_ba_3,avg_ba_4,avg_ls_1,avg_ls_2,avg_ls_3,avg_ls_4,
   cat_cols,CouncilArea_names,df_councilArea,df_postcode,df_sellers,df_suburb,
   df_test,df_train,i,med_Bedroom,median_car,median_yearbuilt,post_codes,seller_names,
   suburb_names,ol_BA,ol_Bath,ol_Bedroom,ol_car,ol_dist,ol_Landsize,ol_price,ol_Rooms)


#Separating our data sets, now that the pre-processing is done.
df_all_new = df_all[-1]
r_df_train = df_all_new %>% filter(data=='train') %>% select(-data)
r_df_test = df_all_new %>% filter(data=='test') %>% select(-data,-Price)  

#Sampling to create validation data.
set.seed(123)
s = sample(1:nrow(r_df_train),0.7*nrow(r_df_train))
train1 = r_df_train[s,]
train2 = r_df_train[-s,]

#Pre-Applying the model
for_vif = lm(Price~. ,data=train1)
summary(for_vif)

#VIF Test
library(car)
sort(vif(for_vif),decreasing = T)

for_vif = lm(Price~.-Postcode_HighPC,data=train1)
sort(vif(for_vif),decreasing = T)

for_vif = lm(Price~.-Postcode_HighPC-Rooms,data=train1)
sort(vif(for_vif),decreasing = T)

#After finding the redundant information, building the model

rm(for_vif)

fit = lm(Price~.-Postcode_HighPC-Rooms,data=train1)
step(fit)
#?step()
summary(fit)

fit = lm(Price~.-Postcode_HighPC-Rooms-Method_SP,data=train1)
summary(fit)

fit = lm(Price~.-Postcode_HighPC-Method_SP-Postcode_MediumPC,data=train1)
summary(fit)

fit = lm(Price~.-Postcode_HighPC-Method_SP-Postcode_MediumPC-Method_PI,data=train1)
summary(fit)

fit = lm(Price~.-Postcode_HighPC-Method_SP-Postcode_MediumPC-Method_PI-Bedroom2,data=train1)
summary(fit)


plot(fit,where=4)

hist(fit$residuals,breaks =50) # Should look close to being normal
?hist()
## For homoskedasticity check (Assumption: Constant Variance)
plot(fit$fitted.values, fit$residuals)
abline(h=0) # Add a horizontal line at y = 0


#Prediction and testing of our model

val_pred = predict(fit,train2)

# Lets put Actual, Predicted and Errors (of testset) in a dataframe
d=data.frame(Actual=train2$Price, Predicted=val_pred, Error=train2$Price-val_pred)
View(d)

## Plot Actual vs. Predicted values 
plot(d$Actual,d$Predicted)

# # Mean Absolute Percentage Error (MAPE)
MAPE_test = mean(abs(d$Error/d$Actual))*100
MAPE_test # Lower the better

## Root Mean Square Error (RMSE)
RMSE_test=sqrt(mean(d$Error^2))
RMSE_test # Lower the better

Score_LR =212467/RMSE_test
Score_LR

# RMSE and MAPE Calculation using forecast package
# install.packages('forecast')
library(forecast)
accuracy(d$Predicted, d$Actual)

#DECISION TREES
library(tree)
dt =  tree(Price ~ ., data=train1)

plot(dt)
text(dt,pretty=0)
dt
summary(dt)

cv_dt = cv.tree(dt)
plot(cv_dt$size,cv_dt$dev,type='b')

prne_dt = prune.tree(dt,best = 8)
plot(prne_dt)
text(prne_dt,pretty = NULL)

pred_prune = predict(prne_dt,newdata = train2)

Pruned_MAPE_test_dt = mean(abs((train2$Price-pred_prune)/train2$Price))*100
Pruned_MAPE_test_dt #

Pruned_rmse_dt=((train2$Price)-(pred_prune))^2 %>% mean() %>% sqrt()
Pruned_rmse_dt

#Prediction
tree.pred=predict(dt,newdata=train2)

MAPE_test_dt = mean(abs((train2$Price-tree.pred)/train2$Price))*100
MAPE_test_dt

rmse_dt=((train2$Price)-(tree.pred))^2 %>% mean() %>% sqrt()
rmse_dt

Score_DT =212467/rmse_dt
Score_DT


#Random Forest 
library(randomForest)

set.seed(1)
rf = randomForest(Price~.,data = train1)
rf
#?randomForest
varImpPlot(rf)

pred_rf = predict(rf,newdata = train2)

MAPE_test_rf = mean(abs((train2$Price-pred_rf)/train2$Price))*100
MAPE_test_rf 

rmse_rf=((train2$Price)-(pred_rf))^2 %>% mean() %>% sqrt()
rmse_rf

Score_RF =212467/rmse_rf
Score_RF

d=data.frame(Actual=train2$Price, Predicted=pred_rf, Error=train2$Price-pred_rf)
View(d)

## Plot Actual vs. Predicted values 
plot(d$Actual,d$Predicted)

#GBM
library(gbm)
set.seed(123)
boost = gbm(Price~.,data=train1,distribution = 'gaussian',n.trees = 5000,interaction.depth = 4,shrinkage = 0.02)
boost
summary(boost)

pred_boost = predict(boost,newdata = train2,n.trees = 5000)

plot(train2$Price,pred_boost)

MAPE_test_gbm = mean(abs((train2$Price-pred_boost)/train2$Price))*100
MAPE_test_gbm

rmse_gbm=((train2$Price)-(pred_boost))^2 %>% mean() %>% sqrt()
rmse_gbm

Score_gbm =212467/rmse_gbm
Score_gbm

#Final predictions
pred_boost_test = predict(boost,newdata = r_df_test,n.trees = 5000)
