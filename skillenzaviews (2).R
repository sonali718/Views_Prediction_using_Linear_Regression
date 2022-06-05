views = read.csv("C:/Users/lenovo/Desktop/viewstrain.csv")
names(views)
summary(views)
str(views)
views = views[,c(-5,-6,-7,-8,-9,-18,-15,-20,-21)]
str(views)
names(views)
library(lubridate)
views$publish_date = dmy(views$publish_date)
views$trending_date =dmy(views$trending_date)
views$dislike = as.integer(views$dislike)
views$Video_id = as.integer(views$Video_id)
views$views = as.integer(views$views)
str(views)
views$Tag_count = as.integer(views$Tag_count)
views$Trend_tag_count = as.integer(views$Trend_tag_count)
str(views)
views$category_id = as.integer(views$category_id)
str(views)
summary(views)
library(VIM)
imputed = kNN(views)
summary(imputed)
imputed = imputed[,c(1:12)]
str(imputed)
views$Tag_count = ifelse(views$Tag_count == " ",NA,views$Tag_count)
views$Trend_tag_count = ifelse(views$Trend_tag_count == " ",NA,views$Tag_count)
views$comment_count = ifelse(views$comment_count == " ",NA,views$comment_count)
views$likes = ifelse(views$likes == " ",NA,views$likes)
impute_num = imputed[sapply(imputed,is.numeric)]
impute_char = imputed[sapply(imputed,is.factor)]
varnames = names(impute_num)
for (i in 2:9)
{
  
  impute_num[,i+8] <- ifelse(impute_num[,i] > quantile((impute_num[,i]),c(0.95)),
                                           quantile(impute_num[,i],c(0.95)),impute_num[,i])
}
impute_num = impute_num[,c(1,10:17)]
names(impute_num) = varnames
library(caret)
impute_num_nzv = nearZeroVar(impute_num)
impute_num_nzv
views_final = cbind(impute_num,impute_char)
views_final$publish_date = views$publish_date
views_final$trending_date = views$trending_date
summary(views_final)
table(views_final$views)
library(caTools)
set.seed(100)
sample = sample.split(views_final$views,SplitRatio = 0.8)
train = subset(views_final,sample == T)
test = subset(views_final,sample == F)
names(train)
model = lm(views~.,data = train)
summary(model)
predict_test = predict(model,newdata = test)
head(predict_test)
test$predicted = predict_test
test$error = test$views - test$predicted
install.packages("Metrics")
library(Metrics)
rmse(test$views,test$predicted)

#_______________________________decision-tree--------------------------------------

names(train)
library(rpart)
mtree = rpart(views~.,data = train[,c(-1)],
 control = rpart.control(minsplit = 500,minbucket = 300,maxdepth = 30,cp=0.001,xval = 10))
plot(mtree)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(mtree,main = "deci_tree for views for a video",sub = "prediction for views",
               caption = TRUE,palettes = "BuGn",type = 2)


prp(mtree,faclen = 1,cex = 0.8,extra = 1)

pred = predict(mtree,test[,c(-1)])
table(train$views)
head(pred,100)
pred = as.data.frame(pred)
library(Metrics)
summary(train)
str(train)
str(test)
summary(pred)
summary(test$views)
rmse(test$views,pred$pred)






#----------------------------validation data----------------------------------

str(train)
validation = read.csv("C:/Users/lenovo/Desktop/viewstest.csv")
str(validation)
library(VIM)
validation = kNN(validation)
summary(validation)
validation = validation[,c(1:18)]
names(validation)
validation=validation[,c(-5,-6,-7,-8,-9,-18)]
summary(validation)
str(validation)
library(lubridate)
validation$publish_date = ymd(validation$publish_date)
validation$trending_date = ymd(validation$trending_date)
validation$comment_disabled=as.factor(validation$comment_disabled)
validation$Video_id = as.integer(validation$Video_id)
names(validation)
pred_valid = predict(model,validation)
head(pred_valid)
pred_final = as.data.frame(pred_valid)
validation$views = pred_final
names(validation)
validation = validation[,c(1,13)]
write.csv(validation,"submission1.csv")
getwd()

