str(total2)
total3<-total2[,-c(22,21,20,10)]
summary(total3)

mom04_n<-mom04_n[,-11]

#이름다시
final6<-total3
str(final6)
colnames(final6)<-c("sex","age","height","weight",
                    "shoulder","chest","belly","arm",
                    "armhole","bottom_leng","wrist",
                    "arm_round","waist","hip",
                    "thigh","ankle","top_length",
                    "neck")

final6<-total2
final6<-final6[,-c(11,20)]
str(final6)
colnames(final6)<-c("sex","age","height","weight",
                    "shoulder","chest","belly","neck","arm",
                    "armhole","bottom_leng","wrist",
                    "arm_round","waist","hip",
                    "thigh","ankle","top_length"
                    )
str(mom04_n)
colnames(mom04_n)<-c("sex","age","height","weight",
                    "shoulder","chest","belly","neck","arm",
                    "armhole","bottom_leng","wrist",
                    "arm_round","waist","hip",
                    "thigh","ankle","top_length"
)
str(final6)

summary(final6)

final6$label<-1:nrow(final6)

#어깨
sort(unique(final6$shoulder))
shoulder<-subset(final6,shoulder>=mean(final6$shoulder)-2*sd(final6$shoulder) & shoulder<=mean(final6$shoulder)+2*sd(final6$shoulder))
str(shoulder)

#chest
sort(unique(final6$bottom_leng))
chest<-subset(final6,chest>=mean(final6$chest)-2*sd(final6$chest) & chest<=mean(final6$chest)+2*sd(final6$chest))
str(chest)


#belly 배
sort(unique(fi))
belly<-subset(final6,belly>=mean(final6$belly)-2*sd(final6$belly) & belly<=mean(final6$belly)+2*sd(final6$belly))
str(belly)

#neck
neck<-subset(final6,neck>=mean(final6$neck)-2*sd(final6$neck) & neck<=mean(final6$neck)+2*sd(final6$neck))
str(neck)

#팔길이 arm
arm<-subset(final6,arm>=mean(final6$arm)-2*sd(final6$arm) & arm<=mean(final6$arm)+2*sd(final6$arm))
str(arm)

#겨드랑이둘레 armhole
armhole<-subset(final6,armhole>=mean(final6$armhole)-2*sd(final6$armhole) & armhole<=mean(final6$armhole)+2*sd(final6$armhole))
str(armhole)

#상체총길이
top_length<-subset(final6,top_length>=mean(final6$top_length)-2*sd(final6$top_length) & top_length<=mean(final6$top_length)+2*sd(final6$top_length))
str(top_length)

#손목둘레
wrist<-subset(final6,wrist>=mean(final6$wrist)-2*sd(final6$wrist) & wrist<=mean(final6$wrist)+2*sd(final6$wrist))
str(wrist)

#손목둘레 모델링


#팔둘레
arm_round<-subset(final6,arm_round>=mean(final6$arm_round)-2*sd(final6$arm_round) & arm_round<=mean(final6$arm_round)+2*sd(final6$arm_round))
str(arm_round)

#허리둘레
waist<-subset(final6,waist>=mean(final6$waist)-2*sd(final6$waist) & waist<=mean(final6$waist)+2*sd(final6$waist))
str(waist)

#엉덩이둘레
hip<-subset(final6,hip>=mean(final6$hip)-2*sd(final6$hip) & hip<=mean(final6$hip)+2*sd(final6$hip))
str(hip)

#넙다리둘레
thigh<-subset(final6,thigh>=mean(final6$thigh)-2*sd(final6$thigh) & thigh<=mean(final6$thigh)+2*sd(final6$thigh))
str(thigh)

#엉덩뼈높이
bottom_leng<-subset(final6,bottom_leng>=mean(final6$bottom_leng)-2*sd(final6$bottom_leng) & bottom_leng<=mean(final6$bottom_leng)+2*sd(final6$bottom_leng))
str(bottom_leng)

#발목둘레
ankle<-subset(final6,ankle>=mean(final6$ankle)-2*sd(final6$ankle) & ankle<=mean(final6$ankle)+2*sd(final6$ankle))
str(ankle)
str(final6)
final6[,c(1,2,3,4,19)]
str(data_total)

data_total = final6[,c(1,2,3,4,19)] %>% 
  inner_join(shoulder[,c(5,19)], by="label") %>%
  inner_join(chest[,c(6,19)], by="label") %>%
  inner_join(belly[,c(7,19)], by="label") %>%
  inner_join(neck[,c(8,19)], by="label") %>%
  inner_join(arm[,c(9,19)], by="label") %>%
  inner_join(armhole[,c(10,19)], by="label") %>%
  inner_join(top_length[,c(11,19)], by="label") %>%
  inner_join(wrist[,c(12,19)], by="label") %>%
  inner_join(arm_round[,c(13,19)], by="label") %>%
  inner_join(waist[,c(14,19)], by="label") %>%
  inner_join(hip[,c(15,19)], by="label") %>%
  inner_join(thigh[,c(16,19)], by="label") %>%
  inner_join(bottom_leng[,c(17,19)], by="label") %>%
  inner_join(ankle[,c(18,19)], by="label")
str(data_total)
summary(data_total)
#모델링
data_total_w<-data_total_w %>% select("sex","age",
                                      "height","weight",
                                      "belly","arm",
                                      "arm_round","waist",
                                      "thigh","ankle",
                                      "neck","label",
                                      "shoulder","chest",
                                      "armhole","top_length"
                                      ,"wrist","hip",
                                      "bottom_leng")

plztotal<-rbind(data_total_m,data_total_w)
str(plztotal)
summary(plztotal)
plztotal<-plztotal[,-12]

write.csv(plztotal,"변수별직접이상치제거.csv",row.names=F)

cor(total[,-1])
install.packages("psych")
library(psych)
pairs.panels(total[,-1])
  
total=read.csv("total.csv")
library(caret) #필요한 패키지를 깔아야한다. 
library(xgboost)
total<-final6
str(total)
summary(total)
sort(unique(total$bottom_leng))
##neck
#데이터중 80%를 학습용, 20%를 테스트 데이터로 분할.
total<-plztotal
str(total)
set.seed(123) #분할할때 언제든 똑같이 분류해서 정확히 비교하기 위해서 

idx = createDataPartition(total$neck, p=.8, list=F)
data.train = total[idx, ]
data.test = total[-idx, ]

#교차검증은 5-fold CV로 한다.
#훈련용 데이터를 무작위로 5개로 나누어 4개로 학습 후 1개로 검증하는 과정을 총 5회 반복.
control = trainControl(method='cv', search='grid', number=5,verbose = TRUE) 
#하이퍼파라미터를 여러가지로 주어 최적모델 찾기. 
xgGrid <- expand.grid(eta=0.03, colsample_bytree=0.5, max_depth=c(2:10), 
                      nrounds=c(400:700), gamma=1, min_child_weight=1, subsample =1)
#실제 학습시키기.
xgb.model_neck <- train(
  neck~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
#테스트 데이터로 예측성능 보기.
neck.xgb = predict(xgb.model_neck,data.test)
R2(neck.xgb,data.test$neck)  #r-square값은 0.8476775로 나옴. 

#랜덤탐색
control = trainControl(method='repeatedcv', search='random', number=5,repeats = 2,verbose = TRUE)
xgb.model <- train(
  n ~ .,
  data = train[-1],
  tuneLength = 3,
  trControl = control,
  method="xgbTree")

xgb.model


#최적모델은 nrounds = 276, max_depth = 3
#위에서 찾은 최적파라미터로 전체 데이처 학습시키기.
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=3, 
                      nrounds=276, gamma=1, min_child_weight=1, subsample =1)
xgb.model_neck <- train(
  neck~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=total,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
saveRDS(xgb.model_neck, "xgb.model_neck.rds")



##arm
set.seed(123) 
idx = createDataPartition(total$arm, p=.8, list=F)
data.train = total[idx, ]
data.test = total[-idx, ]
control = trainControl(method='cv', search='grid', number=5) 
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10), 
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_arm <- train(
  arm~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
arm.xgb = predict(xgb.model_arm,size)  
R2(arm.xgb,data.test$arm) #0.7575649  

arm.xgb

size<-read.csv("size2.csv")
str(size)


#최적모델은 nrounds = 392, max_depth = 2
#위에서 찾은 최적파라미터로 전체 데이처 학습시키기.
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=2, 
                      nrounds=392, gamma=1, min_child_weight=1, subsample =1)
xgb.model_arm <- train(
  arm~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=total,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
saveRDS(xgb.model_arm, "xgb.model_arm.rds")


##armhole
set.seed(123)
idx = createDataPartition(total$armhole, p=.8, list=F)
data.train = total[idx, ]
data.test = total[-idx, ]
control = trainControl(method='cv', search='grid', number=5) 
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10), 
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_armhole <- train(
  armhole~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
xgb.model_armhole <- train(
  armhole~.,
  data=data.train,tuneGrid = xgGrid,trControl = control,method = 'xgbTree') #0.812626
armhole.xgb = predict(xgb.model_armhole,data.test)  
R2(armhole.xgb,data.test$armhole) #0.7496948
#최적모델은 nrounds = 143, max_depth = 4

#위에서 찾은 최적파라미터로 전체 데이처 학습시키기.
xgGrid <- expand.grid(eta=0.05, colsample_bytree=c(0.5:1), max_depth=4, 
                      nrounds=143, gamma=1, min_child_weight=1, subsample =1)
xgb.model_armhole <- train(
  armhole~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=total,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
saveRDS(xgb.model_armhole, "xgb.model_armhole.rds")


##top_length
set.seed(123)
idx = createDataPartition(total$top_length, p=.8, list=F)
data.train = total[idx, ]
data.test = total[-idx, ]
control = trainControl(method='cv', search='grid', number=5) 
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10), 
                      nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
xgb.model_top_length <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')  
top_length.xgb = predict(xgb.model_top_length,data.test)  
R2(top_length.xgb,data.test$top_length) #0.6319521  
#최적모델은 nrounds = 280, max_depth = 2

#위에서 찾은 최적파라미터로 전체 데이처 학습시키기.
xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=2, 
                      nrounds=280, gamma=1, min_child_weight=1, subsample =1)
xgb.model_top_length <- train(
  top_length~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=total,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')
saveRDS(xgb.model_top_length, "xgb.model_top_length.rds")












