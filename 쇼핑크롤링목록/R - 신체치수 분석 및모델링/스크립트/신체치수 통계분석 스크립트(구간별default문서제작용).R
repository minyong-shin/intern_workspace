setwd("C:\\Users\\KOS\\Desktop\\heronation")
library(reshape2)

data_6 <-read.csv("data_6.csv")
data_7 <-read.csv("data_7.csv")

data <-rbind(data_6, data_7)

summary(data) # 통계요약

# delete na
sum(is.na(data))

data <-na.omit(data) # na가 포함된 행을 제거

sum(is.na(data))

data$sex <-trimws(data$sex, which = "both") # 공백제거

summary(data)

# 15세 이상데이터만
data1 <-data[data$age >=15,]
summary(data1)

# 키
plot(data1$height[data1$sex == "남"], main = "남자 키 분포도")
plot(data1$height[data1$sex == "여"], main = "여자 키 분포도")

# 몸무게
plot(data1$weight[data1$sex == "남"], main = "남자 몸무게 분포도")
plot(data1$weight[data1$sex == "여"], main = "여자 몸무게 분포도")

# 몸무게 120kg 이하 데이터만
data2 <-data1[!data1$weight >=120,]
summary(data2)

data_m <-data2[data2$sex == '남',]
data_w <-data2[data2$sex == '여',]
summary(data_m)
summary(data_w)

# 남자 키 140cm, 여자 키 135cm 이상 데이터만
data_m <-total_m
data_w <-total_w

summary(data_m)
summary(data_w)

##################### 남성
# 키/몸무게 데이터 구간 나누기
data_m <-transform(data_m,
                   hlev = cut(height, breaks = seq(1000,2000,by=50),
                              include.lowest = T,
                              right = F,
                              labels = c("100~104cm","105~109cm","110~114cm","115~119cm","120~124cm","125~129cm",
                                         "130~134cm","135~139cm","140~144cm","145~149cm","150~154cm","155~159cm",
                                         "160~164cm","165~169cm","170~174cm","175~179cm","180~184cm","185~189cm",
                                         "190~194cm","195~199cm")))

data_m <-transform(data_m,
                   wlev = cut(weight, breaks = seq(10,155,by=5),
                              include.lowest = T,
                              right = F,
                              labels = c("10~14kg","15~19kg","20~24kg","25~29kg","30~34kg","35~39kg","40~44kg","45~49kg",
                                         "50~54kg","55~59kg","60~64kg","65~69kg","70~74kg","75~79kg","80~84kg","85~89kg",
                                         "90~94kg","95~99kg","100~104kg","105~109kg","110~114kg","115~119kg","120~124kg","125~129kg",
                                         "130~134kg","135~139kg","140~144kg","145~149kg","150~154kg")))

#################
#키구간,몸무게구간별 데이터의 수
library(dplyr)
num_m<-data_m %>% group_by(hlev,wlev) %>% tally()
num_m<-as.data.frame(num_m)
str(num_m)
View(num_m)

##############################################
##############################################
##############################################
#구간산정
##############################################
# 신체치수별 5가지 구간으로 나누기(-1sd / -0.33sd / 0.33sd / 1sd)

m_shoulder <-data_m[data_m$shoulder > mean(data_m$shoulder)-2*sd(data_m$shoulder)&
                      data_m$shoulder < mean(data_m$shoulder + 2*sd(data_m$shoulder)),]
## shoulder
library(dplyr)
library(reshape2)

a1 <-m_shoulder

#a2 <-transmute(a1, hlev = hlev, wlev = wlev, mean = mean(shoulder), sd = sd(shoulder), sigma = (shoulder - mean(shoulder))/sd(shoulder))
#a3 <-a2

#어깨구간
a1$slev <-ifelse(a1$shoulder <= mean(a1$shoulder)-sd(a1$shoulder), "XS",
                 ifelse(a1$shoulder >= mean(a1$shoulder)-sd(a1$shoulder) & a1$shoulder <= mean(a1$shoulder)-0.33*sd(a1$shoulder),"S",
                        ifelse(a1$shoulder >=mean(a1$shoulder)-0.33*sd(a1$shoulder) & a1$shoulder <= mean(a1$shoulder)+0.33*sd(a1$shoulder), "M",
                               ifelse(a1$shoulder >= mean(a1$shoulder)+0.33*sd(a1$shoulder) & a1$shoulder <= mean(a1$shoulder)+sd(a1$shoulder), "L", "XL"))))

str(a1)
a1$slev <-factor(a1$slev, levels = c("XS","S","M","L","XL"))

#성별 구간별 빈도
a4 <-dcast(a1, hlev + wlev ~ slev, length)
str(a4)

## chest
m_chest <-data_m[data_m$chest > mean(data_m$chest)-2*sd(data_m$chest)&
                   data_m$chest < mean(data_m$chest + 2*sd(data_m$chest)),]
library(dplyr)
b1 <-m_chest
#b2 <-transmute(b1, hlev = hlev, wlev = wlev, mean = mean(chest), sd = sd(chest), sigma = (chest - mean(chest))/sd(chest))
#b3 <-b2

#가슴구간
b1$slev <-ifelse(b1$chest <= mean(b1$chest)-sd(b1$chest), "XS",
                 ifelse(b1$chest >= mean(b1$chest)-sd(b1$chest) & b1$chest <= mean(b1$chest)-0.33*sd(b1$chest),"S",
                        ifelse(b1$chest >=mean(b1$chest)-0.33*sd(b1$chest) & b1$chest <= mean(b1$chest)+0.33*sd(b1$chest), "M",
                               ifelse(b1$chest >= mean(b1$chest)+0.33*sd(b1$chest) & b1$chest <= mean(b1$chest)+sd(b1$chest), "L", "XL"))))

str(b1)

b1$slev <-factor(b1$slev, levels = c("XS","S","M","L","XL"))

#성별 구간별 빈도
b4 <-dcast(b1, hlev + wlev ~ slev, length)
str(b4)
View(b4)

## belly
m_belly <-data_m[data_m$belly > mean(data_m$belly)-2*sd(data_m$belly)&
                   data_m$belly < mean(data_m$belly + 2*sd(data_m$belly)),]
library(dplyr)
#어깨구간
c1<-m_belly

c1$slev <-ifelse(c1$belly <= mean(c1$belly)-sd(c1$belly), "XS",
                 ifelse(c1$belly >= mean(c1$belly)-sd(c1$belly) & c1$belly <= mean(c1$belly)-0.33*sd(c1$belly),"S",
                        ifelse(c1$belly >=mean(c1$belly)-0.33*sd(c1$belly) & c1$belly <= mean(c1$belly)+0.33*sd(c1$belly), "M",
                               ifelse(c1$belly >= mean(c1$belly)+0.33*sd(c1$belly) & c1$belly <= mean(c1$belly)+sd(c1$belly), "L", "XL"))))

str(c1)
c1$slev <-factor(c1$slev, levels = c("XS","S","M","L","XL"))

#성별 구간별 빈도
c4 <-dcast(c1, hlev + wlev ~ slev, length)
str(c4)

## waist
m_waist <-data_m[data_m$waist > mean(data_m$waist)-2*sd(data_m$waist)&
                   data_m$waist < mean(data_m$waist + 2*sd(data_m$waist)),]

library(dplyr)
d1 <-m_waist
d1$slev <-ifelse(d1$waist <= mean(d1$waist)-sd(d1$waist), "XS",
                 ifelse(d1$waist >= mean(d1$waist)-sd(d1$waist) & d1$waist <= mean(d1$waist)-0.33*sd(d1$waist),"S",
                        ifelse(d1$waist >=mean(d1$waist)-0.33*sd(d1$waist) & d1$waist <= mean(d1$waist)+0.33*sd(d1$waist), "M",
                               ifelse(d1$waist >= mean(d1$waist)+0.33*sd(d1$waist) & d1$waist <= mean(d1$waist)+sd(d1$waist), "L", "XL"))))
str(d1)
d1$slev <-factor(d1$slev, levels = c("XS","S","M","L","XL"))
#성별 구간별 빈도
d4 <-dcast(d1, hlev + wlev ~ slev, length)
str(d4)

## hip
m_hip <-data_m[data_m$hip > mean(data_m$hip)-2*sd(data_m$hip)&
                 data_m$hip < mean(data_m$hip + 2*sd(data_m$hip)),]
library(dplyr)
e1 <-m_hip
e1$slev <-ifelse(e1$hip <= mean(e1$hip)-sd(e1$hip), "XS",
                 ifelse(e1$hip >= mean(e1$hip)-sd(e1$hip) & e1$hip <= mean(e1$hip)-0.33*sd(e1$hip),"S",
                        ifelse(e1$hip >=mean(e1$hip)-0.33*sd(e1$hip) & e1$hip <= mean(e1$hip)+0.33*sd(e1$hip), "M",
                               ifelse(e1$hip >= mean(e1$hip)+0.33*sd(e1$hip) & e1$hip <= mean(e1$hip)+sd(e1$hip), "L", "XL"))))
str(e1)
e1$slev <-factor(e1$slev, levels = c("XS","S","M","L","XL"))
#성별 구간별 빈도
e4 <-dcast(e1, hlev + wlev ~ slev, length)
str(e4)

## thigh
m_thigh <-data_m[data_m$thigh > mean(data_m$thigh)-2*sd(data_m$thigh)&
                   data_m$thigh < mean(data_m$thigh + 2*sd(data_m$thigh)),]
library(dplyr)
f1 <-m_thigh
f1$slev <-ifelse(f1$thigh <= mean(f1$thigh)-sd(f1$thigh), "XS",
                 ifelse(f1$thigh >= mean(f1$thigh)-sd(f1$thigh) & f1$thigh <= mean(f1$thigh)-0.33*sd(f1$thigh),"S",
                        ifelse(f1$thigh >=mean(f1$thigh)-0.33*sd(f1$thigh) & f1$thigh <= mean(f1$thigh)+0.33*sd(f1$thigh), "M",
                               ifelse(f1$thigh >= mean(f1$thigh)+0.33*sd(f1$thigh) & f1$thigh <= mean(f1$thigh)+sd(f1$thigh), "L", "XL"))))
str(f1)
f1$slev <-factor(f1$slev, levels = c("XS","S","M","L","XL"))
#성별 구간별 빈도
f4 <-dcast(f1, hlev + wlev ~ slev, length)
str(f4)

View(a4)
View(b4)
View(c4)
View(d4)
View(e4)
View(f4)

# csv파일형식으로 저장
write.csv(a4, "shoulder_m_v1.0.csv", row.names = F)
write.csv(b4, "chest_m_v1.0.csv", row.names = F)
write.csv(c4, "belly_m_v1.0.csv", row.names = F)
write.csv(d4, "waist_m_v1.0.csv", row.names = F)
write.csv(e4, "hip_m_v1.0.csv", row.names = F)
write.csv(f4, "thigh_m_v1.0.csv", row.names = F)

############################################
##########################################
#단순선형모델로 예측하기엔 한계가 있음.
#키별 몸무게별로 사람들의 구간을 모델링으로 예측하려면
#xgb써야하지만 너무 오래걸림
#그러면, 있는 사람 즉 있는 데이터만 가지고 일일히 산정해야할 듯
# 어깨 모델
library(caret)
library(xgboost)
str(m_shoulder)
str(data_m)
a_1 <-m_shoulder[,c(3,4,5)]
summary(a_1)
fit1 <-lm(shoulder ~ . + height*weight, data = a_1)

#xgb로 예측하기
control = trainControl(method='repeatedcv', search='random', number=5,repeats = 5,verbose = TRUE)

fit1 <- train(
  shoulder~. + height*weight,
  data=a_1,trControl = control,method = 'xgbTree')


# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(145,190, by=5)){
  for (j in seq(35,115, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

str(dat)
View(dat)
# 빈도수 샘플링
dd = c()
for (i in seq(145,190,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_m$shoulder)-1*sd(data_m$shoulder)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_m$shoulder)-0.33*sd(data_m$shoulder)) & da$mean > round(mean(data_m$shoulder)-1*sd(data_m$shoulder)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_m$shoulder)+0.33*sd(data_m$shoulder)) & da$mean > round(mean(data_m$shoulder)-0.33*sd(data_m$shoulder)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_m$shoulder)+1*sd(data_m$shoulder)) & da$mean > round(mean(data_m$shoulder)+0.33*sd(data_m$shoulder)),"l","xl"))))))
}
str(dd)
View(dd)
write.csv(dd, "a.csv", row.names = F)

# 가슴 모델
str(data_m)
a_2 <-m_chest[,c(3,4,6)]
summary(a_2)
fit1 <-lm(chest ~ . + height*weight, data = a_2)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(145,190, by=5)){
  for (j in seq(35,115, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}
View(dat)
# 빈도수 샘플링
dd = c()
for (i in seq(145,190,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_m$chest)-1*sd(data_m$chest)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_m$chest)-0.33*sd(data_m$chest)) & da$mean > round(mean(data_m$chest)-1*sd(data_m$chest)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_m$chest)+0.33*sd(data_m$chest)) & da$mean > round(mean(data_m$chest)-0.33*sd(data_m$chest)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_m$chest)+1*sd(data_m$chest)) & da$mean > round(mean(data_m$chest)+0.33*sd(data_m$chest)),"l","xl"))))))
}
write.csv(dd, "b.csv", row.names = F)

# 배둘레 모델
a_3 <-data_m[,c(2,3,6)]
summary(a_3)
fit1 <-lm(belly ~ . + height*weight, data = a_3)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(145,190, by=5)){
  for (j in seq(35,115, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(145,190,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_m$belly)-1*sd(data_m$belly)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_m$belly)-0.33*sd(data_m$belly)) & da$mean > round(mean(data_m$belly)-1*sd(data_m$belly)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_m$belly)+0.33*sd(data_m$belly)) & da$mean > round(mean(data_m$belly)-0.33*sd(data_m$belly)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_m$belly)+1*sd(data_m$belly)) & da$mean > round(mean(data_m$belly)+0.33*sd(data_m$belly)),"l","xl"))))))
}
write.csv(dd, "c.csv", row.names = F)

# 허리둘레 모델
a_4 <-data_m[,c(2,3,13)]
summary(a_4)
fit1 <-lm(waist ~ . + height*weight, data = a_4)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(145,190, by=5)){
  for (j in seq(35,115, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(145,190,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_m$waist)-1*sd(data_m$waist)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_m$waist)-0.33*sd(data_m$waist)) & da$mean > round(mean(data_m$waist)-1*sd(data_m$waist)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_m$waist)+0.33*sd(data_m$waist)) & da$mean > round(mean(data_m$waist)-0.33*sd(data_m$waist)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_m$waist)+1*sd(data_m$waist)) & da$mean > round(mean(data_m$waist)+0.33*sd(data_m$waist)),"l","xl"))))))
}
write.csv(dd, "d.csv", row.names = F)

# 힙둘레 모델
a_5 <-data_m[,c(2,3,14)]
summary(a_5)
fit1 <-lm(hip ~ . + height*weight, data = a_5)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(145,190, by=5)){
  for (j in seq(35,115, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(145,190,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_m$hip)-1*sd(data_m$hip)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_m$hip)-0.33*sd(data_m$hip)) & da$mean > round(mean(data_m$hip)-1*sd(data_m$hip)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_m$hip)+0.33*sd(data_m$hip)) & da$mean > round(mean(data_m$hip)-0.33*sd(data_m$hip)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_m$hip)+1*sd(data_m$hip)) & da$mean > round(mean(data_m$hip)+0.33*sd(data_m$hip)),"l","xl"))))))
}
write.csv(dd, "e.csv", row.names = F)

# 넙다리둘레 모델
a_6 <-data_m[,c(2,3,15)]
summary(a_6)
fit1 <-lm(thigh ~ . + height*weight, data = a_6)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(145,190, by=5)){
  for (j in seq(35,115, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(145,190,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_m$thigh)-1*sd(data_m$thigh)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_m$thigh)-0.33*sd(data_m$thigh)) & da$mean > round(mean(data_m$thigh)-1*sd(data_m$thigh)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_m$thigh)+0.33*sd(data_m$thigh)) & da$mean > round(mean(data_m$thigh)-0.33*sd(data_m$thigh)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_m$thigh)+1*sd(data_m$thigh)) & da$mean > round(mean(data_m$thigh)+0.33*sd(data_m$thigh)),"l","xl"))))))
}
write.csv(dd, "f.csv", row.names = F)

##################### 여성
# 키/몸무게 데이터 구간 나누기
str(data_w)
summary(data_w)
data_w <-transform(data_w,
                   hlev = cut(height, breaks = seq(1000,2000,by=50),
                              include.lowest = T,
                              right = F,
                              labels = c("100~104cm","105~109cm","110~114cm","115~119cm","120~124cm","125~129cm",
                                         "130~134cm","135~139cm","140~144cm","145~149cm","150~154cm","155~159cm",
                                         "160~164cm","165~169cm","170~174cm","175~179cm","180~184cm","185~189cm",
                                         "190~194cm","195~199cm")))

data_w <-transform(data_w,
                   wlev = cut(weight, breaks = seq(10,155,by=5),
                              include.lowest = T,
                              right = F,
                              labels = c("10~14kg","15~19kg","20~24kg","25~29kg","30~34kg","35~39kg","40~44kg","45~49kg",
                                         "50~54kg","55~59kg","60~64kg","65~69kg","70~74kg","75~79kg","80~84kg","85~89kg",
                                         "90~94kg","95~99kg","100~104kg","105~109kg","110~114kg","115~119kg","120~124kg","125~129kg",
                                         "130~134kg","135~139kg","140~144kg","145~149kg","150~154kg")))

#################
#키구간,몸무게구간별 데이터의 수
num_w<-data_w %>% group_by(hlev,wlev) %>% tally()
num_w<-as.data.frame(num_w)
str(num_w)
View(num_w)

##############################################
##############################################
##############################################
#구간산정
##############################################
# 신체치수별 5가지 구간으로 나누기(-1sd / -0.33sd / 0.33sd / 1sd)
w_shoulder <-data_w[data_w$shoulder > mean(data_w$shoulder)-2*sd(data_w$shoulder)&
                      data_w$shoulder < mean(data_w$shoulder + 2*sd(data_w$shoulder)),]

## shoulder
library(dplyr)
library(reshape2)

a1_w <-w_shoulder

#a2 <-transmute(a1_w, hlev = hlev, wlev = wlev, mean = mean(shoulder), sd = sd(shoulder), sigma = (shoulder - mean(shoulder))/sd(shoulder))
#a3 <-a2

#어깨구간
a1_w$slev <-ifelse(a1_w$shoulder <= mean(a1_w$shoulder)-sd(a1_w$shoulder), "XS",
                 ifelse(a1_w$shoulder >= mean(a1_w$shoulder)-sd(a1_w$shoulder) & a1_w$shoulder <= mean(a1_w$shoulder)-0.33*sd(a1_w$shoulder),"S",
                        ifelse(a1_w$shoulder >=mean(a1_w$shoulder)-0.33*sd(a1_w$shoulder) & a1_w$shoulder <= mean(a1_w$shoulder)+0.33*sd(a1_w$shoulder), "M",
                               ifelse(a1_w$shoulder >= mean(a1_w$shoulder)+0.33*sd(a1_w$shoulder) & a1_w$shoulder <= mean(a1_w$shoulder)+sd(a1_w$shoulder), "L", "XL"))))

str(a1_w)
a1_w$slev <-factor(a1_w$slev, levels = c("XS","S","M","L","XL"))

#성별 구간별 빈도
a4_w <-dcast(a1_w, hlev + wlev ~ slev, length)
str(a4_w)

## chest
w_chest <-data_w[data_w$chest > mean(data_w$chest)-2*sd(data_w$chest)&
                   data_w$chest < mean(data_w$chest + 2*sd(data_w$chest)),]
library(dplyr)
b1_w <-w_chest
#b2 <-transmute(b1_w, hlev = hlev, wlev = wlev, mean = mean(chest), sd = sd(chest), sigma = (chest - mean(chest))/sd(chest))
#b3 <-b2

#가슴구간
b1_w$slev <-ifelse(b1_w$chest <= mean(b1_w$chest)-sd(b1_w$chest), "XS",
                 ifelse(b1_w$chest >= mean(b1_w$chest)-sd(b1_w$chest) & b1_w$chest <= mean(b1_w$chest)-0.33*sd(b1_w$chest),"S",
                        ifelse(b1_w$chest >=mean(b1_w$chest)-0.33*sd(b1_w$chest) & b1_w$chest <= mean(b1_w$chest)+0.33*sd(b1_w$chest), "M",
                               ifelse(b1_w$chest >= mean(b1_w$chest)+0.33*sd(b1_w$chest) & b1_w$chest <= mean(b1_w$chest)+sd(b1_w$chest), "L", "XL"))))

str(b1_w)

b1_w$slev <-factor(b1_w$slev, levels = c("XS","S","M","L","XL"))

#성별 구간별 빈도
b4_w <-dcast(b1_w, hlev + wlev ~ slev, length)
str(b4)
View(b4)

## belly
w_belly <-data_w[data_w$belly > mean(data_w$belly)-2*sd(data_w$belly)&
                   data_w$belly < mean(data_w$belly + 2*sd(data_w$belly)),]
library(dplyr)
#어깨구간
c1_w<-w_belly

c1_w$slev <-ifelse(c1_w$belly <= mean(c1_w$belly)-sd(c1_w$belly), "XS",
                 ifelse(c1_w$belly >= mean(c1_w$belly)-sd(c1_w$belly) & c1_w$belly <= mean(c1_w$belly)-0.33*sd(c1_w$belly),"S",
                        ifelse(c1_w$belly >=mean(c1_w$belly)-0.33*sd(c1_w$belly) & c1_w$belly <= mean(c1_w$belly)+0.33*sd(c1_w$belly), "M",
                               ifelse(c1_w$belly >= mean(c1_w$belly)+0.33*sd(c1_w$belly) & c1_w$belly <= mean(c1_w$belly)+sd(c1_w$belly), "L", "XL"))))

str(c1_w)
c1_w$slev <-factor(c1_w$slev, levels = c("XS","S","M","L","XL"))

#성별 구간별 빈도
c4_w <-dcast(c1_w, hlev + wlev ~ slev, length)
str(c4)

## waist
w_waist <-data_w[data_w$waist > mean(data_w$waist)-2*sd(data_w$waist)&
                   data_w$waist < mean(data_w$waist + 2*sd(data_w$waist)),]

library(dplyr)
d1_w <-w_waist
d1_w$slev <-ifelse(d1_w$waist <= mean(d1_w$waist)-sd(d1_w$waist), "XS",
                 ifelse(d1_w$waist >= mean(d1_w$waist)-sd(d1_w$waist) & d1_w$waist <= mean(d1_w$waist)-0.33*sd(d1_w$waist),"S",
                        ifelse(d1_w$waist >=mean(d1_w$waist)-0.33*sd(d1_w$waist) & d1_w$waist <= mean(d1_w$waist)+0.33*sd(d1_w$waist), "M",
                               ifelse(d1_w$waist >= mean(d1_w$waist)+0.33*sd(d1_w$waist) & d1_w$waist <= mean(d1_w$waist)+sd(d1_w$waist), "L", "XL"))))
str(d1_w)
d1_w$slev <-factor(d1_w$slev, levels = c("XS","S","M","L","XL"))
#성별 구간별 빈도
d4_w <-dcast(d1_w, hlev + wlev ~ slev, length)
str(d4)

## hip
w_hip <-data_w[data_w$hip > mean(data_w$hip)-2*sd(data_w$hip)&
                 data_w$hip < mean(data_w$hip + 2*sd(data_w$hip)),]
library(dplyr)
e1_w <-w_hip
e1_w$slev <-ifelse(e1_w$hip <= mean(e1_w$hip)-sd(e1_w$hip), "XS",
                 ifelse(e1_w$hip >= mean(e1_w$hip)-sd(e1_w$hip) & e1_w$hip <= mean(e1_w$hip)-0.33*sd(e1_w$hip),"S",
                        ifelse(e1_w$hip >=mean(e1_w$hip)-0.33*sd(e1_w$hip) & e1_w$hip <= mean(e1_w$hip)+0.33*sd(e1_w$hip), "M",
                               ifelse(e1_w$hip >= mean(e1_w$hip)+0.33*sd(e1_w$hip) & e1_w$hip <= mean(e1_w$hip)+sd(e1_w$hip), "L", "XL"))))
str(e1_w)
e1_w$slev <-factor(e1_w$slev, levels = c("XS","S","M","L","XL"))
#성별 구간별 빈도
e4_w <-dcast(e1_w, hlev + wlev ~ slev, length)
str(e4)

## thigh
w_thigh <-data_w[data_w$thigh > mean(data_w$thigh)-2*sd(data_w$thigh)&
                   data_w$thigh < mean(data_w$thigh + 2*sd(data_w$thigh)),]
library(dplyr)
f1_w <-w_thigh
f1_w$slev <-ifelse(f1_w$thigh <= mean(f1_w$thigh)-sd(f1_w$thigh), "XS",
                 ifelse(f1_w$thigh >= mean(f1_w$thigh)-sd(f1_w$thigh) & f1_w$thigh <= mean(f1_w$thigh)-0.33*sd(f1_w$thigh),"S",
                        ifelse(f1_w$thigh >=mean(f1_w$thigh)-0.33*sd(f1_w$thigh) & f1_w$thigh <= mean(f1_w$thigh)+0.33*sd(f1_w$thigh), "M",
                               ifelse(f1_w$thigh >= mean(f1_w$thigh)+0.33*sd(f1_w$thigh) & f1_w$thigh <= mean(f1_w$thigh)+sd(f1_w$thigh), "L", "XL"))))
str(f1_w)
f1_w$slev <-factor(f1_w$slev, levels = c("XS","S","M","L","XL"))
#성별 구간별 빈도
f4_w <-dcast(f1_w, hlev + wlev ~ slev, length)
str(f4)

View(a4_w)
View(b4_w)
View(c4_w)
View(d4_w)
View(e4_w)
View(f4_w)
# csv파일형식으로 저장
write.csv(a4, "shoulder_m_v1.0.csv", row.names = F)
write.csv(b4, "chest_m_v1.0.csv", row.names = F)
write.csv(c4, "belly_m_v1.0.csv", row.names = F)
write.csv(d4, "waist_m_v1.0.csv", row.names = F)
write.csv(e4, "hip_m_v1.0.csv", row.names = F)
write.csv(f4, "thigh_m_v1.0.csv", row.names = F)

# 어깨 범위
round(mean(data_w$shoulder) -1 * sd(data_w$shoulder),1)
round(mean(data_w$shoulder) -0.33 * sd(data_w$shoulder),1)
round(mean(data_w$shoulder) +0.33 * sd(data_w$shoulder),1)
round(mean(data_w$shoulder) +1 * sd(data_w$shoulder),1)

# 가슴 범위
round(mean(data_w$chest) -1 * sd(data_w$chest),1)
round(mean(data_w$chest) -0.33 * sd(data_w$chest),1)
round(mean(data_w$chest) +0.33 * sd(data_w$chest),1)
round(mean(data_w$chest) +1 * sd(data_w$chest),1)

# 배둘레 범위
round(mean(data_w$belly) -1 * sd(data_w$belly),1)
round(mean(data_w$belly) -0.33 * sd(data_w$belly),1)
round(mean(data_w$belly) +0.33 * sd(data_w$belly),1)
round(mean(data_w$belly) +1 * sd(data_w$belly),1)

# 허리 범위
round(mean(data_w$waist) -1 * sd(data_w$waist) ,1)
round(mean(data_w$waist) -0.33 * sd(data_w$waist),1)
round(mean(data_w$waist) +0.33 * sd(data_w$waist),1)
round(mean(data_w$waist) +1 * sd(data_w$waist),1)

# 엉덩이 범위
round(mean(data_w$hip) -1 * sd(data_w$hip),1)
round(mean(data_w$hip) -0.33 * sd(data_w$hip),1)
round(mean(data_w$hip) +0.33 * sd(data_w$hip),1)
round(mean(data_w$hip) +1 * sd(data_w$hip),1)

# 넙다리 범위
round(mean(data_w$thigh) -1 * sd(data_w$thigh),1)
round(mean(data_w$thigh) -0.33 * sd(data_w$thigh),1)
round(mean(data_w$thigh) +0.33 * sd(data_w$thigh),1)
round(mean(data_w$thigh) +1 * sd(data_w$thigh),1)

# 어깨 모델
a_1 <-data_w[,2:4]
summary(a_1)
fit1 <-lm(shoulder ~ . + height*weight, data = a_1)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(135,175, by=5)){
  for (j in seq(30,110, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(135,175,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_w$shoulder)-1*sd(data_w$shoulder)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_w$shoulder)-0.33*sd(data_w$shoulder)) & da$mean > round(mean(data_w$shoulder)-1*sd(data_w$shoulder)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_w$shoulder)+0.33*sd(data_w$shoulder)) & da$mean > round(mean(data_w$shoulder)-0.33*sd(data_w$shoulder)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_w$shoulder)+1*sd(data_w$shoulder)) & da$mean > round(mean(data_w$shoulder)+0.33*sd(data_w$shoulder)),"l","xl"))))))
}
write.csv(dd, "a.csv", row.names = F)

# 가슴 모델
a_2 <-data_w[,c(2,3,5)]
summary(a_2)
fit1 <-lm(chest ~ . + height*weight, data = a_2)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(135,175, by=5)){
  for (j in seq(30,110, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(135,175,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_w$chest)-1*sd(data_w$chest)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_w$chest)-0.33*sd(data_w$chest)) & da$mean > round(mean(data_w$chest)-1*sd(data_w$chest)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_w$chest)+0.33*sd(data_w$chest)) & da$mean > round(mean(data_w$chest)-0.33*sd(data_w$chest)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_w$chest)+1*sd(data_w$chest)) & da$mean > round(mean(data_w$chest)+0.33*sd(data_w$chest)),"l","xl"))))))
}
write.csv(dd, "b.csv", row.names = F)

# 배둘레 모델
a_3 <-data_w[,c(2,3,6)]
summary(a_3)
fit1 <-lm(belly ~ . + height*weight, data = a_3)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(135,175, by=5)){
  for (j in seq(30,110, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(135,175,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_w$belly)-1*sd(data_w$belly)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_w$belly)-0.33*sd(data_w$belly)) & da$mean > round(mean(data_w$belly)-1*sd(data_w$belly)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_w$belly)+0.33*sd(data_w$belly)) & da$mean > round(mean(data_w$belly)-0.33*sd(data_w$belly)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_w$belly)+1*sd(data_w$belly)) & da$mean > round(mean(data_w$belly)+0.33*sd(data_w$belly)),"l","xl"))))))
}
write.csv(dd, "c.csv", row.names = F)

# 허리둘레 모델
a_4 <-data_w[,c(2,3,13)]
summary(a_4)
fit1 <-lm(waist ~ . + height*weight, data = a_4)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(135,175, by=5)){
  for (j in seq(30,110, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(135,175,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_w$waist)-1*sd(data_w$waist)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_w$waist)-0.33*sd(data_w$waist)) & da$mean > round(mean(data_w$waist)-1*sd(data_w$waist)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_w$waist)+0.33*sd(data_w$waist)) & da$mean > round(mean(data_w$waist)-0.33*sd(data_w$waist)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_w$waist)+1*sd(data_w$waist)) & da$mean > round(mean(data_w$waist)+0.33*sd(data_w$waist)),"l","xl"))))))
}
write.csv(dd, "d.csv", row.names = F)

# 힙둘레 모델
a_5 <-data_w[,c(2,3,14)]
summary(a_5)
fit1 <-lm(hip ~ . + height*weight, data = a_5)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(135,175, by=5)){
  for (j in seq(30,110, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(135,175,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_w$hip)-1*sd(data_w$hip)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_w$hip)-0.33*sd(data_w$hip)) & da$mean > round(mean(data_w$hip)-1*sd(data_w$hip)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_w$hip)+0.33*sd(data_w$hip)) & da$mean > round(mean(data_w$hip)-0.33*sd(data_w$hip)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_w$hip)+1*sd(data_w$hip)) & da$mean > round(mean(data_w$hip)+0.33*sd(data_w$hip)),"l","xl"))))))
}
write.csv(dd, "e.csv", row.names = F)

# 넙다리둘레 모델
a_6 <-data_w[,c(2,3,15)]
summary(a_6)
fit1 <-lm(thigh ~ . + height*weight, data = a_6)

# 예측 평균
pred <-function(start_h, start_w){
  df = data.frame(height = sample(start_h:(start_h+5), 1000000, replace = T),
                  weight = sample(start_w:(start_w+5), 1000000, replace = T))
  round(mean(predict(fit1, df)),1)
}

dat <- c()
for (i in seq(135,175, by=5)){
  for (j in seq(30,110, by =5)){
    dat = rbind(dat,data.frame(height = i, weight = j, mean = pred(i,j)))
  }
}

# 빈도수 샘플링
dd = c()
for (i in seq(135,175,by=5)){
  da = dat[dat$height == i,]
  dd = rbind(dd, data.frame(hei = da$height, wei = da$weight, size = ifelse(da$height == i & da$mean <= round(mean(data_w$thigh)-1*sd(data_w$thigh)),"xs",
                                                                            ifelse(da$height == i & da$mean <=round(mean(data_w$thigh)-0.33*sd(data_w$thigh)) & da$mean > round(mean(data_w$thigh)-1*sd(data_w$thigh)),"s",
                                                                                   ifelse(da$height == i & da$mean <=round(mean(data_w$thigh)+0.33*sd(data_w$thigh)) & da$mean > round(mean(data_w$thigh)-0.33*sd(data_w$thigh)),"m",
                                                                                          ifelse(da$height == i & da$mean <=round(mean(data_w$thigh)+1*sd(data_w$thigh)) & da$mean > round(mean(data_w$thigh)+0.33*sd(data_w$thigh)),"l","xl"))))))
}
write.csv(dd, "f.csv", row.names = F)