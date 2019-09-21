data <- read.csv("data2.csv")
summary(data)
str(data)
data$sex <- ifelse(data$sex=="남", "M","F")
data$sex<-as.factor(data$sex)

neck <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","neck")]
arm <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","arm")]
armhole <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","armhole")]
top_length <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","top_length")]
wrist <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","wrist")]
arm_round <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","arm_round")]
bottom_leng <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","bottom_leng")]
ankle <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","ankle")]

neck <-neck[neck$neck > mean(neck$neck) - 2*sd(neck$neck) &
            neck$neck < mean(neck$neck) + 2*sd(neck$neck),]
arm <-arm[arm$arm > mean(arm$arm) - 2*sd(arm$arm) &
              arm$arm < mean(arm$arm) + 2*sd(arm$arm),]
armhole <-armhole[armhole$armhole > mean(armhole$armhole) - 2*sd(armhole$armhole) &
              armhole$armhole < mean(armhole$armhole) + 2*sd(armhole$armhole),]
top_length <-top_length[top_length$top_length > mean(top_length$top_length) - 2*sd(top_length$top_length) &
              top_length$top_length < mean(top_length$top_length) + 2*sd(top_length$top_length),]
wrist <-wrist[wrist$wrist > mean(wrist$wrist) - 2*sd(wrist$wrist) &
              wrist$wrist < mean(wrist$wrist) + 2*sd(wrist$wrist),]
arm_round <-arm_round[arm_round$arm_round > mean(arm_round$arm_round) - 2*sd(arm_round$arm_round) &
              arm_round$arm_round < mean(arm_round$arm_round) + 2*sd(arm_round$arm_round),]
bottom_leng <-bottom_leng[bottom_leng$bottom_leng > mean(bottom_leng$bottom_leng) - 2*sd(bottom_leng$bottom_leng) &
              bottom_leng$bottom_leng < mean(bottom_leng$bottom_leng) + 2*sd(bottom_leng$bottom_leng),]
ankle <-ankle[ankle$ankle > mean(ankle$ankle) - 2*sd(ankle$ankle) &
              ankle$ankle < mean(ankle$ankle) + 2*sd(ankle$ankle),]





# test하기 위한 데이터분할
library(caret)
idx_neck <-createDataPartition(neck$neck, p=.8, list = F)
train_neck <-neck[idx_neck,]
test_neck <-neck[-idx_neck,]

idx_arm <-createDataPartition(arm$arm, p=.8, list = F)
train_arm <-arm[idx_arm,]
test_arm <-arm[-idx_arm,]

idx_armhole <-createDataPartition(armhole$armhole, p=.8, list = F)
train_armhole <-armhole[idx_armhole,]
test_armhole <-armhole[-idx_armhole,]

idx_top_length <-createDataPartition(top_length$top_length, p=.8, list = F)
train_top_length <-top_length[idx_top_length,]
test_top_length <-top_length[-idx_top_length,]

idx_wrist <-createDataPartition(wrist$wrist, p=.8, list = F)
train_wrist <-wrist[idx_wrist,]
test_wrist <-wrist[-idx_wrist,]

idx_arm_round <-createDataPartition(arm_round$arm_round, p=.8, list = F)
train_arm_round <-arm_round[idx_arm_round,]
test_arm_round <-arm_round[-idx_arm_round,]

idx_bottom_leng <-createDataPartition(bottom_leng$bottom_leng, p=.8, list = F)
train_bottom_leng <-bottom_leng[idx_bottom_leng,]
test_bottom_leng <-bottom_leng[-idx_bottom_leng,]

idx_ankle <-createDataPartition(ankle$ankle, p=.8, list = F)
train_ankle <-ankle[idx_ankle,]
test_ankle <-ankle[-idx_ankle,]

control = trainControl(method='repeatedcv', search='random', number=3, verbose = TRUE, repeats = 3)

##neck
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_neck <- train(
  neck~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_neck,trControl = control,method = 'xgbTree')
neck.xgb = predict(xgb.model_neck,test_neck)
R2(neck.xgb,test_neck$neck)#0.83
#Fitting nrounds = 483, max_depth = 2, eta = 0.102, gamma = 9.35, colsample_bytree = 0.656, min_child_weight = 9, subsample = 0.438 on full training set

##arm
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_arm <- train(
  arm~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_arm,trControl = control,method = 'xgbTree')
arm.xgb = predict(xgb.model_arm,test_arm) # 0.67
R2(arm.xgb,test_arm$arm)
#Fitting nrounds = 839, max_depth = 9, eta = 0.0911, gamma = 5.57, colsample_bytree = 0.433, min_child_weight = 6, subsample = 0.425 on full training set

##armhole
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_armhole <- train(
  armhole~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_armhole,trControl = control,method = 'xgbTree')
armhole.xgb = predict(xgb.model_armhole,test_armhole)
R2(armhole.xgb,test_armhole$armhole)#0.7
#Fitting nrounds = 620, max_depth = 10, eta = 0.232, gamma = 4.38, colsample_bytree = 0.472, min_child_weight = 13, subsample = 0.991 on full training set

##top_length
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_top_length <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_top_length,trControl = control,method = 'xgbTree')
top_length.xgb = predict(xgb.model_top_length,test_top_length)
R2(top_length.xgb,test_top_length$top_length)  #0.6597816
#Fitting nrounds = 229, max_depth = 1, eta = 0.479, gamma = 0.8, colsample_bytree = 0.409, min_child_weight = 10, subsample = 0.275 on full training set

train_top_length$bottom_leng <-predict(xgb.model_bottom_leng, train_top_length)
##wrist
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_wrist <- train(
  wrist~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_wrist,trControl = control,method = 'xgbTree')
wrist.xgb = predict(xgb.model_wrist,test_wrist)
R2(wrist.xgb,test_wrist$wrist)  #0.7297816
#Fitting nrounds = 553, max_depth = 1, eta = 0.189, gamma = 5.04, colsample_bytree = 0.501, min_child_weight = 18, subsample = 0.913 on full training set

##arm_round
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_arm_round <- train(
  arm_round~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_arm_round,trControl = control,method = 'xgbTree')
arm_round.xgb = predict(xgb.model_arm_round,test_arm_round)
R2(arm_round.xgb,test_arm_round$arm_round)  #0.84497816
#Fitting nrounds = 784, max_depth = 2, eta = 0.122, gamma = 6.91, colsample_bytree = 0.681, min_child_weight = 12, subsample = 0.894 on full training set

##bottom
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_bottom_leng <- train(
  bottom_leng~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_bottom_leng,trControl = control,method = 'xgbTree')
bottom_leng.xgb = predict(xgb.model_bottom_leng,test_bottom_leng)
R2(bottom_leng.xgb,test_bottom_leng$bottom_leng)  #0.8638056
#최적모델은 nrounds = 499, max_depth = 2, eta = 0.05,colsample_bytree = 0.5
#Fitting nrounds = 376, max_depth = 5, eta = 0.0759, gamma = 8.61, colsample_bytree = 0.387, min_child_weight = 19, subsample = 0.673 on full training set

##ankle
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10),
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_ankle <- train(
  ankle~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=train_ankle,trControl = control,method = 'xgbTree')
ankle.xgb = predict(xgb.model_ankle,test_ankle)
R2(ankle.xgb,test_ankle$ankle)  #0.7197816
#Fitting nrounds = 251, max_depth = 1, eta = 0.308, gamma = 7.6, colsample_bytree = 0.683, min_child_weight = 18, subsample = 0.959 on full training set
#최적모델은 nrounds = 357, max_depth = 3

size <-read.csv('C:\\Users\\KOS\\Documents\\카카오톡 받은 파일/size3.csv')
size$sex <-factor(size$sex, levels = c("남","여"))

#stacking 작업

train_top_length$bottom_leng <-predict(xgb.model_bottom_leng, train_top_length)
train_top_length$arm <-predict(xgb.model_arm, train_top_length)

xgb.model_top_length2 <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh+bottom_leng,
  data=train_top_length,trControl = control,method = 'xgbTree')
test_top_length$bottom_leng <-predict(xgb.model_bottom_leng, test_top_length)
#Fitting nrounds = 312, max_depth = 7, eta = 0.0477, gamma = 8.43, colsample_bytree = 0.404, min_child_weight = 11, subsample = 0.844 on full training set

xgb.model_top_length3 <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh+bottom_leng+arm,
  data=train_top_length,trControl = control,method = 'xgbTree')
test_top_length$arm <-predict(xgb.model_arm, test_top_length)

top_length.xgb2 <-predict(xgb.model_top_length2, test_top_length)
R2(top_length.xgb2, test_top_length$top_length) # 67.43%

top_length.xgb3 <-predict(xgb.model_top_length3, test_top_length)
R2(top_length.xgb3, test_top_length$top_length)

saveRDS(xgb.model_ankle, "xgb_model_ankle.rds")
saveRDS(xgb.model_arm, "xgb_model_arm.rds")
saveRDS(xgb.model_arm_round, "xgb_model_arm_round.rds")
saveRDS(xgb.model_armhole, "xgb_model_armhole.rds")
saveRDS(xgb.model_bottom_leng, "xgb_model_bottom_leng.rds")
saveRDS(xgb.model_neck, "xgb_model_neck.rds")
saveRDS(xgb.model_top_length2, "xgb_model_top_length.rds")
saveRDS(xgb.model_wrist, "xgb_model_wrist.rds")

train_top_length$bottom_leng <-predict(xgb.model_bottom_leng, train_top_length)
xgb.model_top_length <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh+bottom_leng,
  data=train_top_length,trControl = control,method = 'xgbTree')
top_length.xgb <-predict(xgb.model_top_length, test_top_length)

#####################################################
#서과장님께 보내드릴 최종 모델링
#####################################################
##neck
control = trainControl(method='repeatedcv', search='random', number=3, verbose = TRUE, repeats = 3)

xgb.model_neck_final <- train(
  neck~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data = neck,trControl = control,method = 'xgbTree')

neck.xgb = predict(xgb.model_neck_final,test_neck)
R2(neck.xgb,test_neck$neck)#0.84
#Fitting nrounds = 271, max_depth = 2, eta = 0.405, gamma = 2.21, colsample_bytree = 0.321, min_child_weight = 1, subsample = 0.472

##arm
xgb.model_arm_final <- train(
  arm~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data = arm,trControl = control,method = 'xgbTree')

arm.xgb = predict(xgb.model_arm_final,test_arm) # 0.7758
R2(arm.xgb,test_arm$arm)
#Fitting nrounds = 382, max_depth = 3, eta = 0.325, gamma = 4.15, colsample_bytree = 0.42, min_child_weight = 13, subsample = 0.564 on full training set

##armhole
xgb.model_armhole_final <- train(
  armhole~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=armhole,trControl = control,method = 'xgbTree')

armhole.xgb = predict(xgb.model_armhole_final,test_armhole)
R2(armhole.xgb,test_armhole$armhole)#0.7
#Fitting nrounds = 279, max_depth = 6, eta = 0.21, gamma = 5.37, colsample_bytree = 0.361, min_child_weight = 0, subsample = 0.531 on full training set

##top_length
xgb.model_top_length_final <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=top_length,trControl = control,method = 'xgbTree')

top_length.xgb = predict(xgb.model_top_length_final,test_top_length)
R2(top_length.xgb,test_top_length$top_length)  #0.6597816
#


##wrist
xgb.model_wrist_final <- train(
  wrist~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=wrist,trControl = control,method = 'xgbTree')
wrist.xgb = predict(xgb.model_wrist,test_wrist)
R2(wrist.xgb,test_wrist$wrist)  #0.7297816
#

##arm_round
xgb.model_arm_round_final <- train(
  arm_round~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=arm_round,trControl = control,method = 'xgbTree')

arm_round.xgb = predict(xgb.model_arm_round,test_arm_round)
R2(arm_round.xgb,test_arm_round$arm_round)  #0.84497816
#

##bottom
xgb.model_bottom_leng_final <- train(
  bottom_leng~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=bottom_leng,trControl = control,method = 'xgbTree')

bottom_leng.xgb = predict(xgb.model_bottom_leng,test_bottom_leng)
R2(bottom_leng.xgb,test_bottom_leng$bottom_leng)  #0.8638056
#최적모델은 nrounds = 499, max_depth = 2, eta = 0.05,colsample_bytree = 0.5
#

##ankle
xgb.model_ankle_final <- train(
  ankle~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=ankle,trControl = control,method = 'xgbTree')

ankle.xgb = predict(xgb.model_ankle_final,test_ankle)
R2(ankle.xgb,test_ankle$ankle)  #0.7197816

#stacking 부분 코드 
#bottom_leng를 이용한 top_leng의 정확도 향상
top_length$bottom_leng<-predict(xgb.model_bottom_leng_final,top_length)

xgb.model_top_length_final2 <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh+bottom_leng,
  data=top_length,trControl = control,method = 'xgbTree')
#Fitting nrounds = 926, max_depth = 6, eta = 0.0861, gamma = 9.99, colsample_bytree = 0.477, min_child_weight = 18, subsample = 0.325 on full training set
#save model
saveRDS(xgb.model_ankle_final, "xgb_model_ankle.rds")
saveRDS(xgb.model_arm_final, "xgb_model_arm.rds")
saveRDS(xgb.model_arm_round_final, "xgb_model_arm_round.rds")
saveRDS(xgb.model_armhole_final, "xgb_model_armhole.rds")
saveRDS(xgb.model_bottom_leng_final, "xgb_model_bottom_leng.rds")
saveRDS(xgb.model_neck_final, "xgb_model_neck.rds")
saveRDS(xgb.model_top_length_final2, "xgb_model_top_length.rds")
saveRDS(xgb.model_wrist_final, "xgb_model_wrist.rds")


