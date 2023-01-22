setwd("C:/Users/milind/Documents/Aashish Redkar/Project")
getwd()
brc=read.csv("train_BRCpofr.csv",stringsAsFactors = FALSE)
brc_test=read.csv("test_koRSKBP.csv",stringsAsFactors = FALSE)


library(tree)
library(ISLR)
library(dplyr)
library(car)
View (brc)
glimpse(brc)

table(brc$gender)
table(brc$area)
table(brc$qualification)

brc=brc %>%
  mutate(gender_M=as.numeric(gender=="Male")) %>%
  select(-gender)

brc_test=brc_test %>%
  mutate(gender_M=as.numeric(gender=="Male")) %>%
  select(-gender)


brc=brc %>%
  mutate(area_U=as.numeric(area=="Urban")) %>%
  select(-area)


brc_test=brc_test %>%
  mutate(area_U=as.numeric(area=="Urban")) %>%
  select(-area)

brc=brc %>%
  mutate(Qual_High=as.numeric(qualification=="High School"),
         Qual_Bach=as.numeric(qualification=="Bachelor")) %>%
  select(-qualification)

brc_test=brc_test %>%
  mutate(Qual_High=as.numeric(qualification=="High School"),
         Qual_Bach=as.numeric(qualification=="Bachelor")) %>%
  select(-qualification)


table(brc$income)

brc=brc %>%
  mutate(Inc_5to10=as.numeric(income=="5L-10L"),
         Inc_10=as.numeric(income=="More than 10L"),
         Inc_2to5=as.numeric(income=="2L-5L"),
  ) %>%
  select(-income)

brc_test=brc_test %>%
  mutate(Inc_5to10=as.numeric(income=="5L-10L"),
         Inc_10=as.numeric(income=="More than 10L"),
         Inc_2to5=as.numeric(income=="2L-5L"),
  ) %>%
  select(-income)


table(brc$num_policies)
brc=brc %>%
  mutate(Pol_1plus=as.numeric(num_policies=="More than 1")) %>%
  select(-num_policies)


brc_test=brc_test %>%
  mutate(Pol_1plus=as.numeric(num_policies=="More than 1")) %>%
  select(-num_policies)

table(brc$policy)
brc=brc %>%
  mutate(Pol_A=as.numeric(policy=="A"),
         Pol_B=as.numeric(policy=="B")) %>%
  select(-policy)

brc_test=brc_test %>%
  mutate(Pol_A=as.numeric(policy=="A"),
         Pol_B=as.numeric(policy=="B")) %>%
  select(-policy)

table(brc$type_of_policy)
brc=brc %>%
  mutate(Pol_plat=as.numeric(type_of_policy=="Platinum"),
         Pol_gold=as.numeric(type_of_policy=="Gold")) %>%
  select(-type_of_policy)

brc_test=brc_test %>%
  mutate(Pol_plat=as.numeric(type_of_policy=="Platinum"),
         Pol_gold=as.numeric(type_of_policy=="Gold")) %>%
  select(-type_of_policy)

View(brc)

brc$log_claim_amount=log(brc$claim_amount)
brc$log_cltv=log(brc$cltv)
brc$log_vintage=log(brc$vintage)

brc=brc%>%
  mutate(log_claim_amount=as.numeric(gsub("-Inf","0",log_claim_amount)),
         log_vintage=as.numeric(gsub("-Inf","0",log_vintage))) %>%
  select(-claim_amount, -cltv, -vintage)

brc_test$log_claim_amount=log(brc_test$claim_amount)
brc_test$log_cltv=log(brc_test$cltv)
brc_test$log_vintage=log(brc_test$vintage)

brc_test=brc_test%>%
  mutate(log_claim_amount=as.numeric(gsub("-Inf","0",log_claim_amount)),
         log_vintage=as.numeric(gsub("-Inf","0",log_vintage))) %>%
  select(-claim_amount, -cltv, -vintage)

apply(brc,2,function(x) sum(is.na(x)))
apply(brc_test,2,function(x) sum(is.na(x)))

View(cor(brc))
### simple linear model
set.seed(2)
s=sample(1:nrow(brc),0.70*nrow(brc))
ld_train=brc[s,]
ld_test=brc[-s,]

l.fit = lm(log_cltv~.-id ,data=ld_train)
summary(l.fit)

library(car)
vcheck=vif(l.fit)
sort(vcheck,decreasing = T)

l.fit=lm(log_cltv~. -id -Inc_5to10 ,data=ld_train)
summary(l.fit)

t=vif(l.fit)
sort(t,decreasing = T)
t
l.fit=lm(log_cltv~. -id -Inc_5to10 -Inc_10 -Qual_High ,data=ld_train)
summary(l.fit)

t=vif(l.fit)
sort(t,decreasing = T)

###### Residual standard error: 0.5391 on 62561 degrees of freedom
###### Multiple R-squared:  0.3135,	Adjusted R-squared:  0.3133 
###### F-statistic:  2380 on 12 and 62561 DF,  p-value: < 2.2e-16



train_res = cbind.data.frame(Actual=ld_train$log_cltv, Fitted=fitted(l.fit), Error=residuals(l.fit))
View(train_res)

rmse_train=sqrt(mean(train_res$Error^2))
rmse_train

# RMSE  - 0.5390364  -  Stable Model

#Avctual vs predicted
library(ggplot2)
ggplot(train_res,aes(x=Actual,y=Fitted))+geom_point()

#Error~N(0,sigma2)

ggplot(train_res,aes(Error))+geom_histogram()


#Predicted vs error - Homoscedasticity/Independence of errors

ggplot(train_res,aes(x=Fitted,y=Error))+geom_point()

# checking performance

ir_predict=predict(l.fit,newdata=ld_test)
TestRes = cbind.data.frame(Act=ld_test, Pred=ir_predict)
TestRes =cbind.data.frame(ld_test, di=ld_test$log_cltv-ir_predict)
View(TestRes)


#rmse = sqrt(summation(1/n(yi-ypred)^2))
rmse_test=sqrt(mean(TestRes$di^2))
rmse_test
# RMSE  - 0.5427006  -  Stable Model
# visualising

d=data.frame(real=ld_test$log_cltv,predicted=ir_predict, Res=ld_test$log_cltv-ir_predict)
# library(ggplot2)

#Actual vs predicted
ggplot(d,aes(x=real,y=predicted))+geom_point()

#Error~N(0,sigma2)
ggplot(d,aes(Res))+geom_histogram()


#Predicted vs error - Homoscedasticity/Independence of errors
ggplot(d,aes(x=predicted,y=Res))+geom_point()

### Final Answer
ir_fin_predict=predict(l.fit,newdata=brc_test)
TestRes_fin = cbind.data.frame(Act=brc_test, Pred=ir_fin_predict)

View(TestRes_fin)
write.csv(TestRes_fin, "C:/Users/milind/Documents/Aashish Redkar/Test.csv",row.names = F)
write.csv(TestRes_fin, "C:\\Users\\milind\\Documents\\Aashish Redkar\\Eduvancer\\R\\R2\\RExport.csv",row.names = F)

### Decision Tree

brc.tree=tree(log_cltv~.-id,data=ld_train)

brc.tree

cv.brc.tree=cv.tree(brc.tree)
plot(cv.brc.tree$size,cv.brc.tree$dev,type='b')

sum((ld_train$log_cltv-predict(brc.tree,newdata=ld_train))**2) %>%
  sqrt()
### error - 134.8995
sum((ld_test$log_cltv-predict(brc.tree,newdata=ld_test))**2) %>%
  sqrt()
## error - 88.98283
View(brc_test)

prune.brc.tree=prune.tree(brc.tree,best=3)

plot(prune.brc.tree)
text(prune.brc.tree,pretty=0)
prune.brc.tree

## ------------------------------------------------------------------------
brc_pred_train=predict(prune.brc.tree,newdata =brc)
brc_pred_test=predict(prune.brc.tree,newdata=brc_test)


### Random Forest

library(randomForest)



class_rf=randomForest(log_cltv~.,data=ld_train,ntree =50, do.trace=T)
#### Var - Type of random forest: regression
# Number of trees: 50
# No. of variables tried at each split: 5

# Mean of squared residuals: 0.3019814
# % Var explained: 28.65


class_rf
rf=class_rf

forest.pred=predict(class_rf,newdata=ld_test)
table(ld_test$high,forest.pred)

#### Var - 
forest.pred=predict(class_rf,newdata=brc_test)
table(brc_test$high,forest.pred)

class_rftest.predicted=predict(class_rf,newdata = brc_test)
(class_rftest.predicted-brc$cltv)**2 %>% mean() %>% sqrt()

rftest.predicted=predict(rf,newdata=brc_test)
(rftest.predicted-brc_test$cnt)**2 %>% mean() %>% sqrt()

forest.pred=predict(class_rf,newdata=brc_test)
table(brc_test$cltv,forest.pred)

imp = data.frame(importance(class_rf))
imp$Vars = rownames(imp)
imp[order(imp[,1], decreasing = T),]

varImpPlot(class_rf)


#### GBM
library(gbm)

gbm.fit=gbm(log_cltv~.,
            data=ld_train,
            distribution = "gaussian",
            n.trees = 100,interaction.depth = 3)

test.predicted_train=predict.gbm(gbm.fit,newdata=ld_train,n.trees=100)
(test.predicted_train-ld_train$log_cltv)**2 %>% mean() %>% sqrt()
#### Ans - 0.5330023


test.predicted=predict.gbm(gbm.fit,newdata=ld_test,n.trees=100)
(test.predicted-ld_test$log_cltv)**2 %>% mean() %>% sqrt()
### Ans - 0.5381137


# xgboost
library(xgboost)


x_train=ld_train %>% select(-log_cltv)
y_train=ld_train$log_cltv
x_test=brc_test 
xgb.fit=xgboost(data=data.matrix(x_train),
                label = y_train,
                objective='reg:linear',
                verbose=1,
                nrounds = 10)


## train-rmse:0.612056 

test.predicted=predict(xgb.fit,data.matrix(x_test))
(test.predicted-ld_test$cltv)**2 %>% mean() %>% sqrt()

