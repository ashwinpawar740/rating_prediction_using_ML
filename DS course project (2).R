
#title = predict the ratings of app on play store

setwd("D:/SYCS Sem-4/Data Science/course_project")
gplay_store<-read.csv("DS_CP_google_play_store.csv")
head(gplay_store)

library(predict3d)  #To plot LM model
library(ggplot2)  
library(caret)  #dummy variables
library(e1071)  #SVM model



#data preprocessing ----

#converting each category into particular number
category<-gplay_store$Category
category_uni<-unique(category)
category_uni
len<-length(category_uni)
category_n<-c()
iterations<-1:len
for(i in iterations){
  category_n[i]=i-1
}
new_category<-c()
category_int<-cbind(category_uni,category_n)
for (i in gplay_store$Category) {
  for(j in iterations){
    if(i==category_uni[j]){
      new_category<-c(new_category,category_n[j])
    }
  }
}

# fact<-factor(gplay_store$Category)
# fact<-as.numeric(fact)
# fact


#new_category #(variable 1)





#cleaning of size and also filling missing values 

new_size<-c()
class(gplay_store$Size)
for (i in gplay_store$Size) {
  
  len<-nchar(i)
  if(substr(i,len,len)=="M"){ 
    t<-substr(i,1,len-1)
    t<-as.double(t)
    new_size<-c(new_size,t*1024*1024)

  }else if(substr(i,len,len)=="k"){
    t<-substr(i,1,len-1)
    t<-as.numeric(t)
    new_size<-c(new_size,t*1024)
    
  }else{
    new_size<-c(new_size,mean(new_size,na.rm = TRUE))
  }
  
}
#print(new_size[1]+1)

#new_size #(variable 2)





# cleaning number of installations

class(gplay_store$Installs)
new_installation<-c()
new_installation<-gsub(",","",gplay_store$Installs)     # 10,100+

len<-1:length(new_installation)

for (i in len) {
  new_installation[i]<-substr(new_installation[i],1,nchar(new_installation[i])-1)
}


new_installation<-as.integer(new_installation)

which(is.na(new_installation))


#new_installation (variable 3)





# converting type classification into binary

new_type<-c()

for(i in gplay_store$Type){
  if(i=="Free"){
    new_type<-c(new_type,0)
  }else{
    new_type<-c(new_type,1)
  }
}

#new_type (variable 4)





# cleaning price variable or column 

new_price<-c()
temp_price<-gplay_store$Price
temp_price<-gsub(",","",gplay_store$Price)  # $3.99
for (i in temp_price) {
  
  if(i=="0"){
    new_price<-c(new_price,0)
  }
  else{
    t<-substr(i,2,nchar(i))  
    t<-as.double(t)
    new_price<-c(new_price,t)
  }
}

#new_price (variable 5)







# convert reviews to numeric


class(gplay_store$Reviews)


#new_review<-as.integer(gplay_store$Reviews)
new_review<-c()

for(i in gplay_store$Reviews){
  if(substr(i,nchar(i),nchar(i))=="M"){   # 6M
    t<-substr(i,1,nchar(i)-1)
    t<-as.integer(t)
    t<-t*1000000
    new_review<-c(new_review,t)
  }else if(i=="0"){
    
    new_review<-c(new_review,mean(new_review))
    
  }else{
    t<-i
    t<-as.integer(t)
    new_review<-c(new_review,t)
  }
}

#new_review

# new_review (variable 6)






# cleaning ratings
rating<-gplay_store$Rating
class(rating)
temp_rating<-c()



j<-1
for (i in rating) {
  if(i=="NaN"){
    temp_rating<-c(temp_rating,mean(temp_rating))
  }else{
    temp_rating<-c(temp_rating,i)
  }
  j<-j+1
  
}






#new_content_rating  (variable 7)

new_content_rating<-c()
temp_content<-gplay_store$Content.Rating


content_rating_uni<-unique(temp_content)
content_rating_uni
len<-length(content_rating_uni)

content_rating_n<-c()
iterations<-1:len
for(i in iterations){
  content_rating_n[i]=i-1
}

#print(content_rating_uni)

rating_int<-cbind(content_rating_uni,content_rating_n)
for (i in temp_content) {
  for(j in iterations){
    if(i==content_rating_uni[j]){
      new_content_rating<-c(new_content_rating,content_rating_n[j])
    }
  }
  
}


fact<-factor(gplay_store$Content.Rating)
fact<-as.numeric(fact)
new_content_rating<-fact


#print(new_content_rating)



# cleaning genres (variable 8)



genres<-gplay_store$Genres
genres_uni<-unique(genres)

len<-length(genres_uni)
genres_n<-c()
iterations<-1:len
for(i in iterations){
  genres_n[i]=i
}
new_genres<-c()
genres_int<-cbind(genres_uni,genres_n)
for (i in gplay_store$Genres) {
  for(j in iterations){
    if(i==genres_uni[j]){
      new_genres<-c(new_genres,genres_n[j])
    }
  }
}

#print(new_genres)
# new_genres (variable 9)




# dummy variable encoding for category

category<-as.data.frame(category)

dmy<-dummyVars("~.",category)
dummy_cat<-data.frame(predict(dmy, newdata = category))


dummy_cat[1:8,]







# binding all new values together
new_gplay<-cbind(temp_rating,category,new_review,new_size,new_installation,new_type,new_price,new_content_rating,new_category,genres,new_genres)
head(new_gplay)
new_gplay<-as.data.frame(new_gplay)
colnames(new_gplay)<-c("rating","category","review","size","installation","type","price","content_rating","category_c","genres","genres_c")
head(new_gplay)



new_gplay$rating<-as.double(new_gplay$rating)
new_gplay$review<-as.integer(new_gplay$review)
new_gplay$size<-as.integer(new_gplay$size)
new_gplay$installation<-as.integer(new_gplay$installation)
new_gplay$type<-as.integer(new_gplay$type)
new_gplay$price<-as.double(new_gplay$price)
new_gplay$content_rating<-as.integer(new_gplay$content_rating)
new_gplay$category_c<-as.integer(new_gplay$category_c)
new_gplay$genres_c<-as.integer(new_gplay$genres_c)

head(new_gplay)
class(new_gplay)
#print(new_gplay$category) 
#print(new_gplay$size)
#print(new_gplay$installation)
#print(new_gplay$type)
#print(new_gplay$rating)
#print(new_gplay$price)
#print(new_gplay$review)
#print(new_gplay$category_c)
#print(new_gplay$genres_c)




# new data frame including dummy variables



dummy_new_gplay<-cbind(new_gplay,dummy_cat)
class(dummy_new_gplay)
class(dummy_new_gplay)

head(dummy_new_gplay)


#class(dummy_new_gplay$categoryFAMILY)







# splitting data into training and testing ----


# x_train<-new_gplay[1:8000,3:9]
# x_test<-new_gplay[8001:nrow(new_gplay),3:9]
# head(new_gplay)
# 
# x_train<-data.frame(x_train)
# x_test<-data.frame(x_test)
# 
# nrow(new_gplay)
# 
# 
# y_train<-new_gplay[1:8000,1]
# y_test<-new_gplay[8001:10841,1]
# 
# 
# y_train<-data.frame(y_train)
# y_test<-data.frame(y_test)
# 
# y_train[1,1]
# class(y_train)
# length(y_train)
# length(y_test)
# 
# # y_train<-unlist(y_train)
# # y_test<-unlist(y_test)
# # 
# 
# 
# class(y_train)

v<-data.frame(new_gplay$review[8001:10841])



#----

cat("Excluding categorical variables")

# linear model 1 ----
lm_1<-lm(new_gplay$rating~new_gplay$review + new_gplay$size +new_gplay$installation + new_gplay$type + new_gplay$price +new_gplay$content_rating ,new_gplay)


summary(lm_1)
pa<-predict(lm_1)
pa

#x<-predict(lm_1,x_test)

pa<-format(round(pa, 2), nsmall = 2)
pa<-as.numeric(pa)

actual<-format(round(new_gplay$rating, 2), nsmall = 2)
actual<-as.numeric(actual)
pa

er<-abs(actual[1:10840]-pa[1:10840])
t<-cbind(actual=actual[1:10840],predicted=pa,error=er[1:10840])
t
mean(er)
cat("\nmean of all errors excluding categorical of LR model : ",mean(er))

plot_1<-ggplot(new_gplay[1:10840,],aes(x=actual[1:10840],y=pa[1:10840]))+geom_point(color="red",size=1)+geom_smooth(formula = y ~ x,method = "lm")+coord_cartesian(xlim =c(3.5, 5), ylim = c(3.5,5)) +
  labs(title = "actual VS predicted , excluding categorical",x="actual",y="predicted")
print(plot_1)


# linear model 2 ----

lm_2<-lm(new_gplay$rating~new_gplay$review + new_gplay$size +new_gplay$type + new_gplay$price,new_gplay)


lm_2
summary(lm_2)
pa<-predict(lm_2)
pa

pa<-format(round(pa, 2), nsmall = 2)
pa<-as.numeric(pa)

actual<-format(round(new_gplay$rating, 2), nsmall = 2)
actual<-as.numeric(actual)
length(actual)
length(pa)

er<-abs(actual[1:10840]-pa[1:10840])
t<-cbind(actual=actual[1:10840],predicted=pa[1:10840],error=er[1:10840])
t
mean(er)
cat("\nmean of all errors excluding categorical of LR model : ",mean(er))

plot_2<-ggplot(new_gplay[1:10840,],aes(x=actual[1:10840],y=pa[1:10840]))+geom_point(color="red",size=1)+geom_smooth(formula = y ~ x,method = "lm")+coord_cartesian(xlim =c(3.5, 5), ylim = c(3.5,5)) +
  labs(title = "actual VS predicted , excluding categorical",x="actual",y="predicted")
print(plot_2)



# including categorical variable 

# linear model 3 ----
lm_3<-lm(dummy_new_gplay$rating ~ . -dummy_new_gplay$rating -dummy_new_gplay$category -dummy_new_gplay$genres -dummy_new_gplay$genres_c -dummy_new_gplay$category_c ,dummy_new_gplay)

#lm_3<-lm(rating~ . -rating -category - genres - genres_c - category_c,dummy_new_gplay)

summary(lm_3)

pa<-predict(lm_3)

pa<-format(round(pa, 2), nsmall = 2)
pa<-as.numeric(pa)

actual<-format(round(dummy_new_gplay$rating, 2), nsmall = 2)
actual<-as.numeric(actual)



er<-abs(actual[1:10840]-pa[1:10840])
t<-cbind(actual=actual[1:10840],predicted=pa[1:10840],error=er[1:10840])
t
mean(er)
cat("\nmean of all errors including categorical of LR model : ",mean(er))

plot_3<-ggplot(dummy_new_gplay[1:10840,],aes(x=actual[1:10840],y=pa[1:10840]))+geom_point(color="red",size=1)+geom_smooth(formula = y ~ x,method = "lm")+coord_cartesian(xlim =c(3.5, 5), ylim = c(3.5,5)) + 
  labs(title = "actual VS predicted , including categorical",x="actual",y="predicted")
print(plot_3)



# support vector regression 

# support vector machine ----

svr_1<-svm(rating~. -rating -category -genres,dummy_new_gplay)
svr_1
summary(svr_1)
pa<-predict(svr_1)


pa<-format(round(pa, 2), nsmall = 2)
pa<-as.numeric(pa)

actual<-format(round(dummy_new_gplay$rating, 2), nsmall = 2)
actual<-as.numeric(actual)

er<-abs(actual[1:10840]-pa[1:10840])
t<-cbind(actual=actual[1:10840],predicted=pa[1:10840],error=er[1:10840])
t
mean(er)
cat("\nmean of all errors including categorical of SVR model : ",mean(er))

plot_3<-ggplot(new_gplay[1:10840,],aes(x=actual[1:10840],y=pa[1:10840]))+geom_point(color="red",size=1)+geom_smooth(method = "svm")+coord_cartesian(xlim =c(3.5, 5), ylim = c(3.5,5)) + 
  labs(title = "actual VS predicted , including categorical",x="actual",y="predicted")
plot_3















# lm1<-lm(unlist(y_train)~x_train$size,new_gplay)
# summary(lm1)
# p<-predict(lm1)
# p
# p2<-ggplot(new_gplay[1:8000,],aes(x=t,y=y_train))+geom_point()+geom_smooth(method="lm")
# p2


# 
# lm1<-lm(unlist(y_train)~x_train$review + x_train$size + x_train$installation + x_train$type + x_train$price,new_gplay[1:8000,])
# print(summary(lm1))
# class(x_test)
# p<-predict(lm1,v)
# p
# x_test
# length(p)
# length(x_test)
# 
# # p1<-plot(p,unlist(y_test))+abline(a=0,b=1)
# # p1
# 
# 
# 
# 
# 
# 
# lm2<-lm(y_train~x_train$review + x_train$size  + x_train$type + x_train$price ,new_gplay)
# print(summary(lm2))
# 
# p<-predict(lm2,x_test)
# p
# length(p)
# #p<-predict(lm1,x_test,interval = 'confidence')
# 
# pl<-ggplot(x=y_test,y=p)+geom_point()
# 
# print(pl)




#----











