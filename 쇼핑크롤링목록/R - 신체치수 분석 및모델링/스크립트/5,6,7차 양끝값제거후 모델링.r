#성별 전처리 -> rbind -> 부위별 모델링
total_t2<-rbind(data_total_m2,data_total_w2)
summary(data_total_m)
summary(data_total_w)
View(total_t)
View(data_total_w)
summary(data_total_m2)
aa<-data.frame(a=c(1:7),b=c(8:14))
bb<-data.frame(b=c(1:7),a=c(8:14))
ccc<-rbind(aa,bb)
ccc
##################
##################
str(total_t2)
summary(total_t)
summary(total)
str(data_total_m)
str(data_total_w)
str(woman_model)
str(man_model)

data_total_m<-data_total_m[,-17]
data_total_w<-data_total_w[,-8]

total_m<-data_total_m
total_w<-data_total_w

#어깨 - 성별처리
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

shoulder_m<-subset(total_m,shoulder>=mean(total_m$shoulder)-2*sd(total_m$shoulder) & shoulder<=mean(total_m$shoulder)+2*sd(total_m$shoulder))
shoulder_w<-subset(total_w,shoulder>=mean(total_w$shoulder)-2*sd(total_w$shoulder) & shoulder<=mean(total_w$shoulder)+2*sd(total_w$shoulder))

shoulder_mw<-rbind(shoulder_m,shoulder_w)
str(shoulder_mw)

#어깨-모델링

#chest - 성별처리
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

chest_m<-subset(total_m,chest>=mean(total_m$chest)-2*sd(total_m$chest) & chest<=mean(total_m$chest)+2*sd(total_m$chest))
chest_w<-subset(total_w,chest>=mean(total_w$chest)-2*sd(total_w$chest) & chest<=mean(total_w$chest)+2*sd(total_w$chest))

chest_mw<-rbind(chest_m,chest_w)

#chest - modeling

#belly 배
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

belly_m<-subset(total_m,belly>=mean(total_m$belly)-2*sd(total_m$belly) & belly<=mean(total_m$belly)+2*sd(total_m$belly))
belly_w<-subset(total_w,belly>=mean(total_w$belly)-2*sd(total_w$belly) & belly<=mean(total_w$belly)+2*sd(total_w$belly))

belly_mw<-rbind(belly_m,belly_w)

#belly - modeling


#neck
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

neck_m<-subset(total_m,neck>=mean(total_m$neck)-2*sd(total_m$neck) & neck<=mean(total_m$neck)+2*sd(total_m$neck))
neck_w<-subset(total_w,neck>=mean(total_w$neck)-2*sd(total_w$neck) & neck<=mean(total_w$neck)+2*sd(total_w$neck))

neck_mw<-rbind(neck_m,neck_w)

#neck - modeling

#팔길이 arm
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

arm_m<-subset(total_m,arm>=mean(total_m$arm)-2*sd(total_m$arm) & arm<=mean(total_m$arm)+2*sd(total_m$arm))
arm_w<-subset(total_w,arm>=mean(total_w$arm)-2*sd(total_w$arm) & arm<=mean(total_w$arm)+2*sd(total_w$arm))

arm_mw<-rbind(arm_m,arm_w)

#arm - modeling


#겨드랑이둘레 armhole
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

armhole_m<-subset(total_m,armhole>=mean(total_m$armhole)-2*sd(total_m$armhole) & armhole<=mean(total_m$armhole)+2*sd(total_m$armhole))
armhole_w<-subset(total_w,armhole>=mean(total_w$armhole)-2*sd(total_w$armhole) & armhole<=mean(total_w$armhole)+2*sd(total_w$armhole))

armhole_mw<-rbind(armhole_m,armhole_w)

#armhole - modeling

#상체총길이
total_m<-subset(total,sex=="남")
total_w<-subset(total,sex=="여")

top_length_m<-subset(total_m,top_length>=mean(total_m$top_length)-2*sd(total_m$top_length) & top_length<=mean(total_m$top_length)+2*sd(total_m$top_length))
top_length_w<-subset(total_w,top_length>=mean(total_w$top_length)-2*sd(total_w$top_length) & top_length<=mean(total_w$top_length)+2*sd(total_w$top_length))

top_length_mw<-rbind(top_length_m,top_length_w)

#top_length - modeling

#손목둘레
wrist_m<-subset(total_m,wrist>=mean(total_m$wrist)-2*sd(total_m$wrist) & wrist<=mean(total_m$wrist)+2*sd(total_m$wrist))
wrist_w<-subset(total_w,wrist>=mean(total_w$wrist)-2*sd(total_w$wrist) & wrist<=mean(total_w$wrist)+2*sd(total_w$wrist))

str(wrist_m)
wrist_w<- wrist_w %>% select("sex","age","height","weight","shoulder",
                                         "chest","belly","neck","arm",
                                         "armhole","top_length","wrist",
                                         "arm_round","waist","hip",
                                         "thigh","bottom_leng","ankle")

wrist_mw<-rbind(wrist_m,wrist_w)

set.seed(123)
idx = createDataPartition(wrist_mw$wrist, p=.8, list=F)
data.train = wrist_mw[idx, ]
data.test = wrist_mw[-idx, ]

#control = trainControl(method='cv', search='grid', number=5) 
#xgGrid <- expand.grid(eta=0.05, colsample_bytree=0.5, max_depth=c(2:10), 
                      #nrounds=c(100:500), gamma=1, min_child_weight=1, subsample =1)
control = trainControl(method='repeatedcv', search='random', number=5,repeats = 2,verbose = TRUE)

xgb.model_wrist <- train(
  wrist~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,trControl = control,method = 'xgbTree')

wrist.xgb = predict(xgb.model_wrist,data.test)  
R2(wrist.xgb,data.test$wrist)  #0.7363663


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
total_m<-total_m[,-15]
total_w<-total_w[,-5]

thigh_m<-subset(total_m,thigh>=mean(total_m$thigh)-2*sd(total_m$thigh) & thigh<=mean(total_m$thigh)+2*sd(total_m$thigh))
thigh_w<-subset(total_w,thigh>=mean(total_w$thigh)-2*sd(total_w$thigh) & thigh<=mean(total_w$thigh)+2*sd(total_w$thigh))

thigh_mw<-rbind(thigh_m,thigh_w)

thigh_mw$sex<-as.factor(thigh_mw$sex)
str(thigh_mw)
set.seed(123)
idx = createDataPartition(thigh_mw$thigh, p=.8, list=F)
data.train = thigh_mw[idx, ]
data.test = thigh_mw[-idx, ]
#control = trainControl(method='cv', search='grid', number=5,verbose=TRUE) 
#xgGrid <- expand.grid(eta=0.03, colsample_bytree=0.5, max_depth=c(2:10), 
                      #nrounds=c(400:700), gamma=1, min_child_weight=1, subsample =1)

control = trainControl(method='repeatedcv', search='random', number=5,repeats = 2,verbose = TRUE)

xgb.model_thigh <- train(
  thigh~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,trControl = control,method = 'xgbTree')

#tuneGrid = xgGrid

thigh.xgb = predict(xgb.model_thigh,data.test)  
R2(thigh.xgb,data.test$thigh)
summary(bottom_leng_mw)


#엉덩뼈높이
str(data_total_m2)
str(data_total_w2)
total_m<-data_total_m2
total_w<-data_total_w2
total_m<-total_m[,-15]
total_w<-total_w[,-5]

#5,6,7차
total_m<-data_total_m
total_w<-data_total_w
str(total_m)


bottom_leng_m<-subset(total_m,bottom_leng>=mean(total_m$bottom_leng)-2*sd(total_m$bottom_leng) & bottom_leng<=mean(total_m$bottom_leng)+2*sd(total_m$bottom_leng))
bottom_leng_w<-subset(total_w,bottom_leng>=mean(total_w$bottom_leng)-2*sd(total_w$bottom_leng) & bottom_leng<=mean(total_w$bottom_leng)+2*sd(total_w$bottom_leng))

str(bottom_leng_m)
colnames(bottom_leng_w)<-c("sex","age","height","weight",
                     "chest","belly","neck","arm",
                     "armhole","wrist",
                     "arm_round","waist",
                     "thigh","ankle","shoulder","bottom_leng","hip","top_length")

bottom_leng_w<- bottom_leng_w %>% select("sex","age","height","weight",
                           "chest","belly","neck","arm",
                           "armhole","wrist",
                           "arm_round","waist",
                           "thigh","ankle","shoulder","bottom_leng","hip","top_length")

str(bottom_leng_w)

bottom_leng_mw<-rbind(bottom_leng_m,bottom_leng_w)

bottom_leng_mw$sex<-as.factor(bottom_leng_mw$sex)
str(bottom_leng_mw)
summary(bottom_leng_mw)

#다리길이 (엉덩뼈높이)모델링
total<-read.csv("total.csv")
str(total)
summary(total)
str(bottom_leng_mw)
bottom_leng_mw<-total

set.seed(123)
idx = createDataPartition(bottom_leng_mw$bottom_leng, p=.8, list=F)
data.train = bottom_leng_mw[idx, ]
data.test = bottom_leng_mw[-idx, ]
control = trainControl(method='cv', search='grid', number=5,verbose=TRUE) 
xgGrid <- expand.grid(eta=0.03, colsample_bytree=0.5, max_depth=c(2:10), 
                      nrounds=c(400:700), gamma=1, min_child_weight=1, subsample =1)

control = trainControl(method='repeatedcv', search='random', number=5,repeats = 2,verbose = TRUE)

xgb.model_bottom_leng <- train(
  bottom_leng~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,trControl = control,method = 'xgbTree')

#tuneGrid = xgGrid

bottom_leng.xgb = predict(xgb.model_bottom_leng,data.test)  
R2(bottom_leng.xgb,data.test$bottom_leng)
summary(bottom_leng_mw)

#발목둘레
total<-final6

#total_m<-subset(total,sex=="남")
#total_w<-subset(total,sex=="여")

ankle_m<-subset(total_m,ankle>=mean(total_m$ankle)-2*sd(total_m$ankle) & ankle<=mean(total_m$ankle)+2*sd(total_m$ankle))
ankle_w<-subset(total_w,ankle>=mean(total_w$ankle)-2*sd(total_w$ankle) & ankle<=mean(total_w$ankle)+2*sd(total_w$ankle))
str(ankle_m)
ankle_mw<-rbind(ankle_m,ankle_w)

ankle_mw$sex<-as.factor(ankle_mw$sex)
str(ankle_mw)
summary(ankle_mw)

#발목모델링
##ankle
ankle_w<- ankle_w %>% select("sex","age","height","weight",
                                         "chest","belly","neck","arm",
                                         "armhole","wrist",
                                         "arm_round","waist",
                                         "thigh","ankle","shoulder","bottom_leng","hip","top_length")

ankle_mw<-rbind(ankle_m,ankle_w)

ankle_mw$sex<-as.factor(ankle_mw$sex)


set.seed(123)
idx = createDataPartition(ankle_mw$ankle, p=.8, list=F)
data.train = ankle_mw[idx, ]
data.test = ankle_mw[-idx, ]
#control = trainControl(method='cv', search='grid', number=5,verbose=TRUE) 
#xgGrid <- expand.grid(eta=0.03, colsample_bytree=0.5, max_depth=c(2:10), 
                      #nrounds=c(400:700), gamma=1, min_child_weight=1, subsample =1)

control = trainControl(method='repeatedcv', search='random', number=5,repeats = 3,verbose = TRUE)

xgb.model_ankle <- train(
  ankle~sex+age+height+weight+shoulder+chest+belly+waist+hip+thigh,
  data=data.train,trControl = control,method = 'xgbTree')

#tuneGrid = xgGrid

xgb.model_ankle <- train(
  ankle~.,
  data=data.train,tuneGrid = xgGrid,trControl = control,method = 'xgbTree')#0.7780316 #nrounds = 486, max_depth = 3
ankle.xgb = predict(xgb.model_ankle,data.test)  
R2(ankle.xgb,data.test$ankle)  #0.7097816
#최적모델은 nrounds = 357, max_depth = 3
summary(ankle_mw)

str(data)











