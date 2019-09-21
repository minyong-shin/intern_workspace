#5,6,7차 신체치수의 병합 후 이상치 제거

final<-rbind(version04,version10,version15)
str(final)
#
aa<-rbind(mom04,cha6,version15)
###################################

sum(is.na(final))
View(final)

#
str(aa)
sum(is.na(aa))
aa<-na.omit(aa)

sna123 <-c()
for (i in 1:ncol(aa)){
  sna123 <-c(sna123, sum(is.na(aa[,i])))
}
sna123 <-as.matrix(sna123)
rownames(sna123) <-colnames(aa)
sna123

#결측치채우기
final2<-final
final2$lable<-1:nrow(final)


#라벨링 떼기
fea<-final2[,c(8,20)]
str(fea)

fea2<-final2[,-c(8)]
str(fea2)

nap<-na.omit(fea2)
str(nap)

final3<-merge(nap,fea,by="lable",all=F)
str(final3)
sum(is.na(final3))

str(final3)
final3$성별<-as.factor(final3$성별)
final4<-final3[,-1]
str(final4)

write.csv(final4,"신체치수데이터.csv",row.names=F)

#다중대치 mice로 결측치 채우기
install.packages("mice")
library(mice)

final4_mice<-mice(final4,pred=quickpred(final4),m=5,method="rf",ntree=3)
str(final4_mice)

final4_mice1<-complete(final4_mice,1)
sum(is.na(final4_mice1))
str(final4_mice1)

final4_mice2<-complete(final4_mice,2)
str(final4_mice2)

final4_mice3<-complete(final4_mice,3)

final4_mice4<-complete(final4_mice,4)

final4_mice5<-complete(final4_mice,5)
str(final4)

final5<-final4[,-19]
str(final5)

final5$목둘레 <- (final4_mice1$목둘레+final4_mice2$목둘레+final4_mice3$목둘레+final4_mice4$목둘레+final4_mice5$목둘레)/5
str(final5)
sum(is.na(final5))

#이상치처리 - 남자,여자 몸무게
man<-subset(aa,성별=="남")
woman<-subset(aa,성별=="여")

#################################
#################################
man$label<-1:nrow(man)
str(man)
sort(unique(man$나이))

woman$label<-1:nrow(woman)
sort(unique(woman$나이))

windows()
#남자
ggplot(man,aes(label,몸무게,col=성별))+
  geom_point()+
  scale_y_continuous(name = "몸무게",
                     breaks = seq(0,160, 10),
                     limits=c(0, 160))+
  geom_hline(yintercept = 110,col="blue",cex=0.5)+
  geom_hline(yintercept = 30,col="blue",cex=0.5)



#여자
ggplot(woman,aes(label,몸무게,col=성별))+
  geom_point()+
  scale_y_continuous(name = "몸무게",
                     breaks = seq(0,140, 10),
                     limits=c(0, 140))+
  geom_hline(yintercept = 90,col="blue",cex=0.5)+
  geom_hline(yintercept = 30,col="blue",cex=0.5)

#이상치처리 - 남자,여자 - 키
ggplot(man,aes(label,키,col=성별))+
  geom_point()+
  scale_y_continuous(name = "키",
                     breaks = seq(1200,2000, 100),
                     limits=c(1200, 2000))+
  geom_hline(yintercept = 1400,col="blue",cex=0.5)
  
ggplot(woman,aes(label,키,col=성별))+
  geom_point()+
  scale_y_continuous(name = "키",
                     breaks = seq(1200,1800, 100),
                     limits=c(1200, 1800))+
  geom_hline(yintercept = 1350,col="blue",cex=0.5)


#남/여키와 몸무게 이상치처리 정리
data_m<-subset(man,몸무게>=30 & 몸무게<=110)
str(data_m)
data_m2<-subset(data_m,키>=1400)
str(data_m2)

data_w<-subset(woman,몸무게>=30 & 몸무게<=90)
str(data_w)
data_w2<-subset(data_w,키>=1350)
str(data_w2)

#여기서 다시 데이터 통합
total2<-rbind(data_w2,data_m2)
str(total2)
#키와 몸무게의 데이터 구간 나누기
data_m2 <-transform(data_m2,
                   hlev = cut(키, breaks = seq(1000,2000,by=50),
                              include.lowest = T,
                              right = F,
                              labels = c("100~105cm","105~110cm","110~115cm","115~120cm","120~125cm","125~130cm",
                                         "130~135cm","135~140cm","140~145cm","145~150cm","150~155cm","155~160cm",
                                         "160~165cm","165~170cm","170~175cm","175~180cm","180~185cm","185~190cm",
                                         "190~195cm","195~200cm")))

data_m2 <-transform(data_m2,
                   wlev = cut(몸무게, breaks = seq(10,155,by=5),
                              include.lowest = T,
                              right = F,
                              labels = c("10~15kg","15~20kg","20~25kg","25~30kg","30~35kg","35~40kg","40~45kg","45~50kg",
                                         "50~55kg","55~60kg","60~65kg","65~70kg","70~75kg","75~80kg","80~85kg","85~90kg",
                                         "90~95kg","95~100kg","100~105kg","105~110kg","110~115kg","115~120kg","120~125kg","125~130kg",
                                         "130~135kg","135~140kg","140~145kg","145~150kg","150~155kg")))
str(data_m2)

data_w2 <-transform(data_w2,
                    hlev = cut(키, breaks = seq(1000,2000,by=50),
                                include.lowest = T,
                                right = F,
                                labels = c("100~105cm","105~110cm","110~115cm","115~120cm","120~125cm","125~130cm",
                                           "130~135cm","135~140cm","140~145cm","145~150cm","150~155cm","155~160cm",
                                           "160~165cm","165~170cm","170~175cm","175~180cm","180~185cm","185~190cm",
                                           "190~195cm","195~200cm")))

data_w2 <-transform(data_w2,
                    wlev = cut(몸무게, breaks = seq(10,155,by=5),
                                  include.lowest = T,
                                  right = F,
                                  labels = c("10~15kg","15~20kg","20~25kg","25~30kg","30~35kg","35~40kg","40~45kg","45~50kg",
                                             "50~55kg","55~60kg","60~65kg","65~70kg","70~75kg","75~80kg","80~85kg","85~90kg",
                                             "90~95kg","95~100kg","100~105kg","105~110kg","110~115kg","115~120kg","120~125kg","125~130kg",
                                             "130~135kg","135~140kg","140~145kg","145~150kg","150~155kg")))
str(data_w2)


#각 신체치수별 사이즈 측정을 위한 구간산정
#어깨 -남
mean(data_m2$어깨가쪽사이길이)+sd(data_m2$어깨가쪽사이길이)

library(RColorBrewer)
shol1<-ggplot(data_m2, aes(x=어깨가쪽사이길이)) + 
  geom_histogram(fill="#FF99FF",col="white")+
  scale_x_continuous(name = "어깨",
                     breaks = seq(300,550, 50),
                     limits=c(300, 550))+
  geom_vline(aes(xintercept=mean(어깨가쪽사이길이)),
             color="blue", linetype="dashed", size=1)+
  geom_vline(xintercept = mean(data_m2$어깨가쪽사이길이)+sd(data_m2$어깨가쪽사이길이),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$어깨가쪽사이길이)-sd(data_m2$어깨가쪽사이길이),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$어깨가쪽사이길이)+0.33*sd(data_m2$어깨가쪽사이길이),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_m2$어깨가쪽사이길이)-0.33*sd(data_m2$어깨가쪽사이길이),col="#003300",size=1)

shol1

shol2<-ggplot(data_w2, aes(x=어깨가쪽사이길이)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  scale_x_continuous(name = "어깨",
                     breaks = seq(250,500, 50),
                     limits=c(250, 500))+
  geom_vline(aes(xintercept=mean(어깨가쪽사이길이)),
             color="blue", linetype="dashed", size=1)+
  geom_vline(xintercept = mean(data_w2$어깨가쪽사이길이)+sd(data_w2$어깨가쪽사이길이),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$어깨가쪽사이길이)-sd(data_w2$어깨가쪽사이길이),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$어깨가쪽사이길이)+0.33*sd(data_w2$어깨가쪽사이길이),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_w2$어깨가쪽사이길이)-0.33*sd(data_w2$어깨가쪽사이길이),col="#003300",size=1)
install.packages("ggpubr")
library(ggpubr)
ggarrange(shol1,shol2, 
          labels = c("남자어깨", "여자어깨"),
          ncol = 2, nrow = 1)

#가슴##########################################

min(data_m2$가슴둘레)
max(data_m2$가슴둘레)
che1<-ggplot(data_m2, aes(x=가슴둘레)) + 
  geom_histogram(fill="#FF99FF",col="white")+
  geom_vline(aes(xintercept=mean(가슴둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "가슴둘레",
                     breaks = seq(400,1300, 50),
                     limits=c(400, 1300))+
  geom_vline(xintercept = mean(data_m2$가슴둘레)+sd(data_m2$가슴둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$가슴둘레)-sd(data_m2$가슴둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$가슴둘레)+0.33*sd(data_m2$가슴둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_m2$가슴둘레)-0.33*sd(data_m2$가슴둘레),col="#003300",size=1)

min(data_w2$가슴둘레)
max(data_w2$가슴둘레)

data_w2<-subset(data_w2,!가슴둘레==0)

che2<-ggplot(data_w2, aes(x=가슴둘레)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(가슴둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "가슴둘레",
                     breaks = seq(345,1200, 50),
                     limits=c(345, 1200))+
  geom_vline(xintercept = mean(data_w2$가슴둘레)+sd(data_w2$가슴둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$가슴둘레)-sd(data_w2$가슴둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$가슴둘레)+0.33*sd(data_w2$가슴둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_w2$가슴둘레)-0.33*sd(data_w2$가슴둘레),col="#003300",size=1)


windows()
ggarrange(che1,che2, 
          labels = c("남자가슴둘레", "여자가슴둘레"),
          ncol = 2, nrow = 1)
#배
min(data_m2$배둘레)
max(data_m2$배둘레)
belly1<-ggplot(data_m2, aes(x=배둘레)) + 
  geom_histogram(fill="#FF99FF",col="white")+
  geom_vline(aes(xintercept=mean(배둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "배둘레",
                     breaks = seq(550,1300, 50),
                     limits=c(550, 1300))+
  geom_vline(xintercept = mean(data_m2$배둘레)+sd(data_m2$배둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$배둘레)-sd(data_m2$배둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$배둘레)+0.33*sd(data_m2$배둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_m2$배둘레)-0.33*sd(data_m2$배둘레),col="#003300",size=1)


min(data_w2$배둘레)
max(data_w2$배둘레)

belly2<-ggplot(data_w2, aes(x=배둘레)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(배둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "배둘레",
                     breaks = seq(500,1250, 50),
                     limits=c(500, 1250))+
  geom_vline(xintercept = mean(data_w2$배둘레)+sd(data_w2$배둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$배둘레)-sd(data_w2$배둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$배둘레)+0.33*sd(data_w2$배둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_w2$배둘레)-0.33*sd(data_w2$배둘레),col="#003300",size=1)


windows()
ggarrange(belly1,belly2, 
          labels = c("남자배둘레", "여자배둘레"),
          ncol = 2, nrow = 1)

#허리
min(data_m2$허리둘레)
max(data_m2$허리둘레)
waist1<-ggplot(data_m2, aes(x=허리둘레)) + 
  geom_histogram(fill="#FF99FF",col="white")+
  geom_vline(aes(xintercept=mean(허리둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "허리둘레",
                     breaks = seq(500,1200, 50),
                     limits=c(500, 1200))+
  geom_vline(xintercept = mean(data_m2$허리둘레)+sd(data_m2$허리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$허리둘레)-sd(data_m2$허리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$허리둘레)+0.33*sd(data_m2$허리둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_m2$허리둘레)-0.33*sd(data_m2$허리둘레),col="#003300",size=1)


min(data_w2$허리둘레)
max(data_w2$허리둘레)

waist2<-ggplot(data_w2, aes(x=허리둘레)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(허리둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "허리둘레",
                     breaks = seq(500,1200, 50),
                     limits=c(500, 1200))+
  geom_vline(xintercept = mean(data_w2$허리둘레)+sd(data_w2$허리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$허리둘레)-sd(data_w2$허리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$허리둘레)+0.33*sd(data_w2$허리둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_w2$허리둘레)-0.33*sd(data_w2$허리둘레),col="#003300",size=1)


windows()
ggarrange(waist1,waist2, 
          labels = c("남자허리둘레", "여자허리둘레"),
          ncol = 2, nrow = 1)

#엉덩이
min(data_m2$엉덩이둘레)
max(data_m2$엉덩이둘레)
hip1<-ggplot(data_m2, aes(x=엉덩이둘레)) + 
  geom_histogram(fill="#FF99FF",col="white")+
  geom_vline(aes(xintercept=mean(엉덩이둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "엉덩이둘레",
                     breaks = seq(350,1250, 50),
                     limits=c(350, 1250))+
  geom_vline(xintercept = mean(data_m2$엉덩이둘레)+sd(data_m2$엉덩이둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$엉덩이둘레)-sd(data_m2$엉덩이둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$엉덩이둘레)+0.33*sd(data_m2$엉덩이둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_m2$엉덩이둘레)-0.33*sd(data_m2$엉덩이둘레),col="#003300",size=1)


min(data_w2$엉덩이둘레)
max(data_w2$엉덩이둘레)


hip2<-ggplot(data_w2, aes(x=엉덩이둘레)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(엉덩이둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "엉덩이둘레",
                     breaks = seq(300,1200, 50),
                     limits=c(300, 1200))+
  geom_vline(xintercept = mean(data_w2$엉덩이둘레)+sd(data_w2$엉덩이둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$엉덩이둘레)-sd(data_w2$엉덩이둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$엉덩이둘레)+0.33*sd(data_w2$엉덩이둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_w2$엉덩이둘레)-0.33*sd(data_w2$엉덩이둘레),col="#003300",size=1)


windows()
ggarrange(hip1,hip2, 
          labels = c("남자엉덩이둘레", "여자엉덩이둘레"),
          ncol = 2, nrow = 1)

#넙다리
min(data_m2$넙다리둘레)
max(data_m2$넙다리둘레)
thigh1<-ggplot(data_m2, aes(x=넙다리둘레)) + 
  geom_histogram(fill="#FF99FF",col="white")+
  geom_vline(aes(xintercept=mean(넙다리둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "넙다리둘레",
                     breaks = seq(350,800, 50),
                     limits=c(350, 800))+
  geom_vline(xintercept = mean(data_m2$넙다리둘레)+sd(data_m2$넙다리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$넙다리둘레)-sd(data_m2$넙다리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_m2$넙다리둘레)+0.33*sd(data_m2$넙다리둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_m2$넙다리둘레)-0.33*sd(data_m2$넙다리둘레),col="#003300",size=1)+
  geom_text(aes(round(mean(data_m2$넙다리둘레)-0.33*sd(data_m2$넙다리둘레),0),y=2400),label=round(mean(data_m2$넙다리둘레)-0.33*sd(data_m2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_m2$넙다리둘레)+0.33*sd(data_m2$넙다리둘레),0),y=2400),label=round(mean(data_m2$넙다리둘레)+0.33*sd(data_m2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_m2$넙다리둘레)-sd(data_m2$넙다리둘레),0),y=2400),label=round(mean(data_m2$넙다리둘레)-sd(data_m2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_m2$넙다리둘레)+sd(data_m2$넙다리둘레),0),y=2400),label=round(mean(data_m2$넙다리둘레)+sd(data_m2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_m2$넙다리둘레),0),y=2500),label=round(mean(data_m2$넙다리둘레),0))
  

man_model[man_model$thigh==367,]
data_w2<-subset(data_w2,!넙다리둘레 ==0)

min(data_w2$넙다리둘레)
max(data_w2$넙다리둘레)

thigh2<-ggplot(data_w2, aes(x=넙다리둘레)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(넙다리둘레)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "넙다리둘레",
                     breaks = seq(350,800, 50),
                     limits=c(350, 800))+
  geom_vline(xintercept = mean(data_w2$넙다리둘레)+sd(data_w2$넙다리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$넙다리둘레)-sd(data_w2$넙다리둘레),col="red",size=1)+
  geom_vline(xintercept = mean(data_w2$넙다리둘레)+0.33*sd(data_w2$넙다리둘레),col="#003300",size=1)+
  geom_vline(xintercept = mean(data_w2$넙다리둘레)-0.33*sd(data_w2$넙다리둘레),col="#003300",size=1)+
  geom_text(aes(round(mean(data_w2$넙다리둘레)-0.33*sd(data_w2$넙다리둘레),0),y=2400),label=round(mean(data_w2$넙다리둘레)-0.33*sd(data_w2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_w2$넙다리둘레)+0.33*sd(data_w2$넙다리둘레),0),y=2400),label=round(mean(data_w2$넙다리둘레)+0.33*sd(data_w2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_w2$넙다리둘레)-sd(data_w2$넙다리둘레),0),y=2400),label=round(mean(data_w2$넙다리둘레)-sd(data_w2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_w2$넙다리둘레)+sd(data_w2$넙다리둘레),0),y=2400),label=round(mean(data_w2$넙다리둘레)+sd(data_w2$넙다리둘레),0))+
  geom_text(aes(round(mean(data_w2$넙다리둘레),0),y=2500),label=round(mean(data_w2$넙다리둘레),0))

thigh2

windows()
man_model[max(man_model$thigh),]

ggarrange(thigh1,thigh2, 
          labels = c("남자넙다리둘레", "여자넙다리둘레"),
          ncol = 2, nrow = 1)


