#GBM 
install.packages("AmesHousing")
library(AmesHousing)

library(rsample)      # data splitting 
install.packages(c("gbm","h2o","pdp",'lime'))
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)


ah <- AmesHousing::make_ames()
str(ah)

set.seed(123)
idx = createDataPartition(ah$Sale_Price,p = 0.7,list = F)
ah_train = ah[idx,]
ah_test =ah[-idx,]

## gbm(formula = Sale_Price ~ ., distribution = "gaussian", data = ames_train, 
##     n.trees = 10000, interaction.depth = 1, shrinkage = 0.001, 
##     cv.folds = 5, verbose = FALSE, n.cores = NULL)
## A gradient boosted model with gaussian loss function.
## 10000 iterations were performed.
## The best cross-validation iteration was 10000.
## There were 80 predictors of which 45 had non-zero influence.
gbm.fit <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ah_train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  

ah.gbm = predict(gbm.fit,ah_test)
R2(ah.gbm,ah_test$Sale_Price)프

#RMSE
sqrt(min(gbm.fit$cv.error))
#손실함수 그래프 ntrees added to the ensemble
#손실함수의 설명 
#https://ratsgo.github.io/machine%20learning/2017/10/12/terms/
gbm.perf(gbm.fit,method = "cv")#교차검증방법으로 피팅모델의 손실함수 그래프

#parameter tuning
gbm.fit <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ah_train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  
min_mse <- which.min(gbm.fit$cv.error)
min_mse
sqrt(min(gbm.fit$cv.error[min_mse]))
#ntree의 어느 부분에서 가장 최적일지
gbm.perf(gbm.fit,method="cv")
#rmse가 줄어든 것을 확인할 수 있다.
#gbm random search가 있을 것 같은데 일단 현재 문서에서는 grid search를 하므로
#grid search를 통해서 최적의 hyper parameter을 찾는다
# modify hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)


# total number of combinations
nrow(hyper_grid) #grid경우의수

#caret 패키지의 train함수처럼 자동으로 grid.search를 진행해주는 형식이 아니라
#직접 반복문을 통해서 입력해야한다.
# grid search 
#random sampling를 하는데 여기선 똑같은 train데이터를 추출하는데
#random으로 섞어서 추추
random_index <- sample(1:nrow(ah_train), nrow(ah_train))
random_ames_train <- ah_train[random_index, ]

for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Sale_Price ~ .,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
#따라서 해당 결과의 하이퍼 파라미터를 확인하면 아래와 같다ㅏ
# train GBM model
gbm.fit.final <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ah_train,
  n.trees = 483,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

#변수별로 종속변수에 관련이 높은 변수를 추출해준다 
#즉,rmse에 영향을 미치는 변수를 추출 method는 두개로 사용형태가 다름
par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
#값도 출력됨
#vip패키지를 사용하여 ggplot처럼 나타나게 할 수 있음

#lime 라임 알고리즘
model_type.gbm <- function(x, ...) {
  return("regression")
}

predict_model.gbm <- function(x, newdata, ...) {
  pred <- predict(x, newdata, n.trees = x$n.trees)
  return(as.data.frame(pred))
}

local_obs <- ah_test[1:3, ]
explainer <- lime(ah_train, gbm.fit.final)
explanation <- explain(local_obs, explainer, n_features = 5)
plot_features(explanation)
#라임알고리즘은 군집으로 분석할 때 좋을 것 같음 왜냐하면 케이스별로 나눠주므로
#즉, 행별로 나눠주므로 군집분석시 군집별로 속성값의 영향을 미치는 정도를 확인할 때

#예측
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, ah_test)
RMSE(pred,ah_test$Sale_Price)

################################################################
################################################################
#신체치수데이터를 이용한 gbm
data <- read.csv("data2.csv")
summary(data)
str(data)
data$sex <- ifelse(data$sex=="남", "M","F")
data$sex<-as.factor(data$sex)

#치수별로 모델분할
neck <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","neck")]
arm <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","arm")]
armhole <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","armhole")]
top_length <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","top_length")]
wrist <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","wrist")]
arm_round <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","arm_round")]
bottom_leng <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","bottom_leng")]
ankle <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","ankle")]

#데이터 분할
idx_neck <-createDataPartition(neck$neck, p=.8, list = F)
train_neck <-neck[idx_neck,]
test_neck <-neck[-idx_neck,]

#neck_gbm
neck_gbm_model <- gbm(
  formula = neck ~ .,
  distribution = "gaussian",
  data = train_neck,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
)  
sqrt(min(neck_gbm_model$cv.error))
#ntree가 적을수록 깊이가 깊을수록 shrinkage가 커질수록 정확도 높아짐.
neck_pred<-predict(neck_gbm_model,test_neck)
R2(neck_pred,test_neck$neck)

mean_neck <- (neck_pred+neck.xgb)/2
R2(mean_neck,test_neck$neck) #앙상블을 통해서 증가되는 결정계수는 약 0.5%임
#그렇게 높은 성능은 기대하기 힘들지만 결정계수가 오르고 또한 오차를 잘 잡는 면
#에서 유용할 수 있음
#현재 위의 모델은 xgboost와 gbm을 사용함
#이후에 회귀예측을 하는 모델인 rf, svm도 같이 묶으면 좋을 것 같음


#Lightgbm 
#https://bluediary8.tistory.com/25



#데이터셋의 단위와 스케일이 비슷한 형태에서 svm이용
# svm은 로지스틱회귀나 분류rf 등 두개 이상으로 나누어진 집단을 분류하는데
#사용하기도 하며 연속형 변수를 예측하는 회귀에서도 사용한다.(단, 이름이 svr임)
#svm은 분류 알고리즘에서 정확도가 높은 편이며 이상치의 영향도 적게 받음
#기본 방법은 데이터를 나누는 "최적의 경계"를 만드는 방식임
#경계선에서 가장 가까운 벡터를 support vecto라하고 데이터와 경계 사이의 거리를
#마진이라고 하고 이 margin에서 가장 가까운 데이터를 support vecto라함

#svm에서는 커널 트릭이라는 기법으로 초평면을 3차원으로 그릴 수 있음
#이 기법을 이용하여 2차원으로 분리하기 어려운 데이터도 분리
#roc curve, auc를 사용
library(e1071)
install.packages("Epi")
library(Epi)
library(caret)

data <- read.csv("data2.csv")

neck <-data[,c("sex","age","height","weight","shoulder","chest","belly","waist","hip","thigh","neck")]

idx_neck <-createDataPartition(neck$neck, p=.8, list = F)
train_neck <-neck[idx_neck,]
test_neck <-neck[-idx_neck,]

#tune.svm()함수를 통해서 최적의 gamma, cost의 값을 구한다
#svm은 gamma와 cost를 찾고서 그다음 svm()으로 진행해야 함
tune.svm(neck ~ ., data = neck, 
         gamma = 2^(-1:1), cost=2^(2:4),verbose = TRUE)

#그래프
# 그래프
plot(x=test$c_thickness, y=yhat_test, main="SVR")

#최적의 gamma, cost를 찾았으면 모델에 적용
m<- svm(neck~.,data = train_neck,gamma = 1,cost=16)






