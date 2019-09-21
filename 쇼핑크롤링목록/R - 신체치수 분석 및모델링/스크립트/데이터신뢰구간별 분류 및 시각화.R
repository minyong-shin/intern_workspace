data<-read.csv("data.csv")
str(data)
total_m<-subset(data,sex=="남")
total_w<-subset(data,sex=="여")
#5%자른 데이터로 구간 산정 그래프
#어깨
shoulder_m<-subset(total_m,shoulder>=mean(total_m$shoulder)-2*sd(total_m$shoulder) & shoulder<=mean(total_m$shoulder)+2*sd(total_m$shoulder))
shoulder_w<-subset(total_w,shoulder>=mean(total_w$shoulder)-2*sd(total_w$shoulder) & shoulder<=mean(total_w$shoulder)+2*sd(total_w$shoulder))

#각구간별 평균값 - 남자
mean(shoulder_m[shoulder_m$shoulder <= mean(shoulder_m$shoulder)-sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_m[shoulder_m$shoulder <= mean(shoulder_m$shoulder)-0.33*sd(shoulder_m$shoulder) & shoulder_m$shoulder >= mean(shoulder_m$shoulder)-sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_m[shoulder_m$shoulder >= mean(shoulder_m$shoulder)-0.33*sd(shoulder_m$shoulder) & shoulder_m$shoulder <= mean(shoulder_m$shoulder)+0.33*sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_m[shoulder_m$shoulder >= mean(shoulder_m$shoulder)+0.33*sd(shoulder_m$shoulder) & shoulder_m$shoulder <= mean(shoulder_m$shoulder)+sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_m[shoulder_m$shoulder >= mean(shoulder_m$shoulder)+sd(shoulder_m$shoulder),]$shoulder)

#각구간별 평균값 - 여자
mean(shoulder_w[shoulder_w$shoulder <= mean(shoulder_w$shoulder)-sd(shoulder_w$shoulder),]$shoulder)
mean(shoulder_w[shoulder_w$shoulder <= mean(shoulder_w$shoulder)-0.33*sd(shoulder_w$shoulder) & shoulder_w$shoulder >= mean(shoulder_w$shoulder)-sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_w[shoulder_w$shoulder >= mean(shoulder_w$shoulder)-0.33*sd(shoulder_w$shoulder) & shoulder_w$shoulder <= mean(shoulder_w$shoulder)+0.33*sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_w[shoulder_w$shoulder >= mean(shoulder_w$shoulder)+0.33*sd(shoulder_w$shoulder) & shoulder_w$shoulder <= mean(shoulder_w$shoulder)+sd(shoulder_m$shoulder),]$shoulder)
mean(shoulder_w[shoulder_w$shoulder >= mean(shoulder_w$shoulder)+sd(shoulder_w$shoulder),]$shoulder)

library(ggplot2)
shoulder_plot<-ggplot(shoulder_m, aes(x=shoulder)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(shoulder)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "어깨너비",
                     breaks = seq(min(shoulder_m$shoulder),max(shoulder_m$shoulder), 25),
                     limits=c(min(shoulder_m$shoulder),max(shoulder_m$shoulder)))+
  geom_vline(xintercept = mean(shoulder_m$shoulder)+sd(shoulder_m$shoulder),col="red",size=1)+
  geom_vline(xintercept = mean(shoulder_m$shoulder)-sd(shoulder_m$shoulder),col="red",size=1)+
  geom_vline(xintercept = mean(shoulder_m$shoulder)+0.33*sd(shoulder_m$shoulder),col="#003300",size=1)+
  geom_vline(xintercept = mean(shoulder_m$shoulder)-0.33*sd(shoulder_m$shoulder),col="#003300",size=1)+
  geom_text(aes(round(mean(shoulder_m$shoulder)-0.33*sd(shoulder_m$shoulder),0),y=1500),label=round(mean(shoulder_m$shoulder)-0.33*sd(shoulder_m$shoulder),0))+
  geom_text(aes(round(mean(shoulder_m$shoulder)+0.33*sd(shoulder_m$shoulder),0),y=1500),label=round(mean(shoulder_m$shoulder)+0.33*sd(shoulder_m$shoulder),0))+
  geom_text(aes(round(mean(shoulder_m$shoulder)-sd(shoulder_m$shoulder),0),y=1500),label=round(mean(shoulder_m$shoulder)-sd(shoulder_m$shoulder),0))+
  geom_text(aes(round(mean(shoulder_m$shoulder)+sd(shoulder_m$shoulder),0),y=1500),label=round(mean(shoulder_m$shoulder)+sd(shoulder_m$shoulder),0))+
  geom_text(aes(round(mean(shoulder_m$shoulder),0),y=1500),label=round(mean(shoulder_m$shoulder),0))


shoulder_plot2<-ggplot(shoulder_w, aes(x=shoulder)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(shoulder)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "shoulder",
                     breaks = seq(min(shoulder_w$shoulder),max(shoulder_w$shoulder), 25),
                     limits=c(min(shoulder_w$shoulder),max(shoulder_w$shoulder)))+
  geom_vline(xintercept = mean(shoulder_w$shoulder)+sd(shoulder_w$shoulder),col="red",size=1)+
  geom_vline(xintercept = mean(shoulder_w$shoulder)-sd(shoulder_w$shoulder),col="red",size=1)+
  geom_vline(xintercept = mean(shoulder_w$shoulder)+0.33*sd(shoulder_w$shoulder),col="#003300",size=1)+
  geom_vline(xintercept = mean(shoulder_w$shoulder)-0.33*sd(shoulder_w$shoulder),col="#003300",size=1)+
  geom_text(aes(round(mean(shoulder_w$shoulder)-0.33*sd(shoulder_w$shoulder),0),y=1500),label=round(mean(shoulder_w$shoulder)-0.33*sd(shoulder_w$shoulder),0))+
  geom_text(aes(round(mean(shoulder_w$shoulder)+0.33*sd(shoulder_w$shoulder),0),y=1500),label=round(mean(shoulder_w$shoulder)+0.33*sd(shoulder_w$shoulder),0))+
  geom_text(aes(round(mean(shoulder_w$shoulder)-sd(shoulder_w$shoulder),0),y=1500),label=round(mean(shoulder_w$shoulder)-sd(shoulder_w$shoulder),0))+
  geom_text(aes(round(mean(shoulder_w$shoulder)+sd(shoulder_w$shoulder),0),y=1500),label=round(mean(shoulder_w$shoulder)+sd(shoulder_w$shoulder),0))+
  geom_text(aes(round(mean(shoulder_w$shoulder),0),y=1500),label=round(mean(shoulder_w$shoulder),0))

windows()
shoulder_plot
shoulder_plot2

################################################################
#가슴
chest_m<-subset(total_m,chest>=mean(total_m$chest)-2*sd(total_m$chest) & chest<=mean(total_m$chest)+2*sd(total_m$chest))
chest_w<-subset(total_w,chest>=mean(total_w$chest)-2*sd(total_w$chest) & chest<=mean(total_w$chest)+2*sd(total_w$chest))

#각구간별 평균값
mean(chest_m[chest_m$chest <= mean(chest_m$chest)-sd(chest_m$chest),]$chest)
mean(chest_m[chest_m$chest <= mean(chest_m$chest)-0.33*sd(chest_m$chest) & chest_m$chest >= mean(chest_m$chest)-sd(chest_m$chest),]$chest)
mean(chest_m[chest_m$chest >= mean(chest_m$chest)-0.33*sd(chest_m$chest) & chest_m$chest <= mean(chest_m$chest)+0.33*sd(chest_m$chest),]$chest)
mean(chest_m[chest_m$chest >= mean(chest_m$chest)+0.33*sd(chest_m$chest) & chest_m$chest <= mean(chest_m$chest)+sd(chest_m$chest),]$chest)
mean(chest_m[chest_m$chest >= mean(chest_m$chest)+sd(chest_m$chest),]$chest)
#여자


chest_plot<-ggplot(chest_m, aes(x=chest)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(chest)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "어깨너비",
                     breaks = seq(min(chest_m$chest),max(chest_m$chest), 25),
                     limits=c(min(chest_m$chest),max(chest_m$chest)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(chest_m$chest)+sd(chest_m$chest),col="red",size=1)+
  geom_vline(xintercept = mean(chest_m$chest)-sd(chest_m$chest),col="red",size=1)+
  geom_vline(xintercept = mean(chest_m$chest)+0.33*sd(chest_m$chest),col="#003300",size=1)+
  geom_vline(xintercept = mean(chest_m$chest)-0.33*sd(chest_m$chest),col="#003300",size=1)+
  geom_text(aes(round(mean(chest_m$chest)-0.33*sd(chest_m$chest),0),y=800),label=round(mean(chest_m$chest)-0.33*sd(chest_m$chest),0))+
  geom_text(aes(round(mean(chest_m$chest)+0.33*sd(chest_m$chest),0),y=800),label=round(mean(chest_m$chest)+0.33*sd(chest_m$chest),0))+
  geom_text(aes(round(mean(chest_m$chest)-sd(chest_m$chest),0),y=800),label=round(mean(chest_m$chest)-sd(chest_m$chest),0))+
  geom_text(aes(round(mean(chest_m$chest)+sd(chest_m$chest),0),y=800),label=round(mean(chest_m$chest)+sd(chest_m$chest),0))+
  geom_text(aes(round(mean(chest_m$chest),0),y=800),label=round(mean(chest_m$chest),0))


chest_plot2<-ggplot(chest_w, aes(x=chest)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(chest)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "chest",
                     breaks = seq(min(chest_w$chest),max(chest_w$chest), 25),
                     limits=c(min(chest_w$chest),max(chest_w$chest)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(chest_w$chest)+sd(chest_w$chest),col="red",size=1)+
  geom_vline(xintercept = mean(chest_w$chest)-sd(chest_w$chest),col="red",size=1)+
  geom_vline(xintercept = mean(chest_w$chest)+0.33*sd(chest_w$chest),col="#003300",size=1)+
  geom_vline(xintercept = mean(chest_w$chest)-0.33*sd(chest_w$chest),col="#003300",size=1)+
  geom_text(aes(round(mean(chest_w$chest)-0.33*sd(chest_w$chest),0),y=800),label=round(mean(chest_w$chest)-0.33*sd(chest_w$chest),0))+
  geom_text(aes(round(mean(chest_w$chest)+0.33*sd(chest_w$chest),0),y=800),label=round(mean(chest_w$chest)+0.33*sd(chest_w$chest),0))+
  geom_text(aes(round(mean(chest_w$chest)-sd(chest_w$chest),0),y=800),label=round(mean(chest_w$chest)-sd(chest_w$chest),0))+
  geom_text(aes(round(mean(chest_w$chest)+sd(chest_w$chest),0),y=800),label=round(mean(chest_w$chest)+sd(chest_w$chest),0))+
  geom_text(aes(round(mean(chest_w$chest),0),y=800),label=round(mean(chest_w$chest),0))

windows()
chest_plot
chest_plot2

#########################################################
#############3
#배
belly_m<-subset(total_m,belly>=mean(total_m$belly)-2*sd(total_m$belly) & belly<=mean(total_m$belly)+2*sd(total_m$belly))
belly_w<-subset(total_w,belly>=mean(total_w$belly)-2*sd(total_w$belly) & belly<=mean(total_w$belly)+2*sd(total_w$belly))

#각구간별 평균값 - 남자
mean(belly_m[belly_m$belly <= mean(belly_m$belly)-sd(belly_m$belly),]$belly)
mean(belly_m[belly_m$belly <= mean(belly_m$belly)-0.33*sd(belly_m$belly) & belly_m$belly >= mean(belly_m$belly)-sd(belly_m$belly),]$belly)
mean(belly_m[belly_m$belly >= mean(belly_m$belly)-0.33*sd(belly_m$belly) & belly_m$belly <= mean(belly_m$belly)+0.33*sd(belly_m$belly),]$belly)
mean(belly_m[belly_m$belly >= mean(belly_m$belly)+0.33*sd(belly_m$belly) & belly_m$belly <= mean(belly_m$belly)+sd(belly_m$belly),]$belly)
mean(belly_m[belly_m$belly >= mean(belly_m$belly)+sd(belly_m$belly),]$belly)

#각구간별 평균값 - 여자
mean(belly_w[belly_w$belly <= mean(belly_w$belly)-sd(belly_w$belly),]$belly)
mean(belly_w[belly_w$belly <= mean(belly_w$belly)-0.33*sd(belly_w$belly) & belly_w$belly >= mean(belly_w$belly)-sd(belly_m$belly),]$belly)
mean(belly_w[belly_w$belly >= mean(belly_w$belly)-0.33*sd(belly_w$belly) & belly_w$belly <= mean(belly_w$belly)+0.33*sd(belly_m$belly),]$belly)
mean(belly_w[belly_w$belly >= mean(belly_w$belly)+0.33*sd(belly_w$belly) & belly_w$belly <= mean(belly_w$belly)+sd(belly_m$belly),]$belly)
mean(belly_w[belly_w$belly >= mean(belly_w$belly)+sd(belly_w$belly),]$belly)


belly_plot<-ggplot(belly_m, aes(x=belly)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(belly)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "어깨너비",
                     breaks = seq(min(belly_m$belly),max(belly_m$belly), 25),
                     limits=c(min(belly_m$belly),max(belly_m$belly)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(belly_m$belly)+sd(belly_m$belly),col="red",size=1)+
  geom_vline(xintercept = mean(belly_m$belly)-sd(belly_m$belly),col="red",size=1)+
  geom_vline(xintercept = mean(belly_m$belly)+0.33*sd(belly_m$belly),col="#003300",size=1)+
  geom_vline(xintercept = mean(belly_m$belly)-0.33*sd(belly_m$belly),col="#003300",size=1)+
  geom_text(aes(round(mean(belly_m$belly)-0.33*sd(belly_m$belly),0),y=800),label=round(mean(belly_m$belly)-0.33*sd(belly_m$belly),0))+
  geom_text(aes(round(mean(belly_m$belly)+0.33*sd(belly_m$belly),0),y=800),label=round(mean(belly_m$belly)+0.33*sd(belly_m$belly),0))+
  geom_text(aes(round(mean(belly_m$belly)-sd(belly_m$belly),0),y=800),label=round(mean(belly_m$belly)-sd(belly_m$belly),0))+
  geom_text(aes(round(mean(belly_m$belly)+sd(belly_m$belly),0),y=800),label=round(mean(belly_m$belly)+sd(belly_m$belly),0))+
  geom_text(aes(round(mean(belly_m$belly),0),y=800),label=round(mean(belly_m$belly),0))


belly_plot2<-ggplot(belly_w, aes(x=belly)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(belly)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "belly",
                     breaks = seq(min(belly_w$belly),max(belly_w$belly), 25),
                     limits=c(min(belly_w$belly),max(belly_w$belly)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(belly_w$belly)+sd(belly_w$belly),col="red",size=1)+
  geom_vline(xintercept = mean(belly_w$belly)-sd(belly_w$belly),col="red",size=1)+
  geom_vline(xintercept = mean(belly_w$belly)+0.33*sd(belly_w$belly),col="#003300",size=1)+
  geom_vline(xintercept = mean(belly_w$belly)-0.33*sd(belly_w$belly),col="#003300",size=1)+
  geom_text(aes(round(mean(belly_w$belly)-0.33*sd(belly_w$belly),0),y=800),label=round(mean(belly_w$belly)-0.33*sd(belly_w$belly),0))+
  geom_text(aes(round(mean(belly_w$belly)+0.33*sd(belly_w$belly),0),y=800),label=round(mean(belly_w$belly)+0.33*sd(belly_w$belly),0))+
  geom_text(aes(round(mean(belly_w$belly)-sd(belly_w$belly),0),y=800),label=round(mean(belly_w$belly)-sd(belly_w$belly),0))+
  geom_text(aes(round(mean(belly_w$belly)+sd(belly_w$belly),0),y=800),label=round(mean(belly_w$belly)+sd(belly_w$belly),0))+
  geom_text(aes(round(mean(belly_w$belly),0),y=800),label=round(mean(belly_w$belly),0))

windows()
belly_plot
belly_plot2

###################################################
##################################
#허리
waist_m<-subset(total_m,waist>=mean(total_m$waist)-2*sd(total_m$waist) & waist<=mean(total_m$waist)+2*sd(total_m$waist))
waist_w<-subset(total_w,waist>=mean(total_w$waist)-2*sd(total_w$waist) & waist<=mean(total_w$waist)+2*sd(total_w$waist))

#각구간별 평균값 - 남자
mean(waist_m[waist_m$waist <= mean(waist_m$waist)-sd(waist_m$waist),]$waist)
mean(waist_m[waist_m$waist <= mean(waist_m$waist)-0.33*sd(waist_m$waist) & waist_m$waist >= mean(waist_m$waist)-sd(waist_m$waist),]$waist)
mean(waist_m[waist_m$waist >= mean(waist_m$waist)-0.33*sd(waist_m$waist) & waist_m$waist <= mean(waist_m$waist)+0.33*sd(waist_m$waist),]$waist)
mean(waist_m[waist_m$waist >= mean(waist_m$waist)+0.33*sd(waist_m$waist) & waist_m$waist <= mean(waist_m$waist)+sd(waist_m$waist),]$waist)
mean(waist_m[waist_m$waist >= mean(waist_m$waist)+sd(waist_m$waist),]$waist)

#각구간별 평균값 - 여자
mean(waist_w[waist_w$waist <= mean(waist_w$waist)-sd(waist_w$waist),]$waist)
mean(waist_w[waist_w$waist <= mean(waist_w$waist)-0.33*sd(waist_w$waist) & waist_w$waist >= mean(waist_w$waist)-sd(waist_m$waist),]$waist)
mean(waist_w[waist_w$waist >= mean(waist_w$waist)-0.33*sd(waist_w$waist) & waist_w$waist <= mean(waist_w$waist)+0.33*sd(waist_m$waist),]$waist)
mean(waist_w[waist_w$waist >= mean(waist_w$waist)+0.33*sd(waist_w$waist) & waist_w$waist <= mean(waist_w$waist)+sd(waist_m$waist),]$waist)
mean(waist_w[waist_w$waist >= mean(waist_w$waist)+sd(waist_w$waist),]$waist)


waist_plot<-ggplot(waist_m, aes(x=waist)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(waist)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "어깨너비",
                     breaks = seq(min(waist_m$waist),max(waist_m$waist), 25),
                     limits=c(min(waist_m$waist),max(waist_m$waist)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(waist_m$waist)+sd(waist_m$waist),col="red",size=1)+
  geom_vline(xintercept = mean(waist_m$waist)-sd(waist_m$waist),col="red",size=1)+
  geom_vline(xintercept = mean(waist_m$waist)+0.33*sd(waist_m$waist),col="#003300",size=1)+
  geom_vline(xintercept = mean(waist_m$waist)-0.33*sd(waist_m$waist),col="#003300",size=1)+
  geom_text(aes(round(mean(waist_m$waist)-0.33*sd(waist_m$waist),0),y=800),label=round(mean(waist_m$waist)-0.33*sd(waist_m$waist),0))+
  geom_text(aes(round(mean(waist_m$waist)+0.33*sd(waist_m$waist),0),y=800),label=round(mean(waist_m$waist)+0.33*sd(waist_m$waist),0))+
  geom_text(aes(round(mean(waist_m$waist)-sd(waist_m$waist),0),y=800),label=round(mean(waist_m$waist)-sd(waist_m$waist),0))+
  geom_text(aes(round(mean(waist_m$waist)+sd(waist_m$waist),0),y=800),label=round(mean(waist_m$waist)+sd(waist_m$waist),0))+
  geom_text(aes(round(mean(waist_m$waist),0),y=800),label=round(mean(waist_m$waist),0))


waist_plot2<-ggplot(waist_w, aes(x=waist)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(waist)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "waist",
                     breaks = seq(min(waist_w$waist),max(waist_w$waist), 25),
                     limits=c(min(waist_w$waist),max(waist_w$waist)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(waist_w$waist)+sd(waist_w$waist),col="red",size=1)+
  geom_vline(xintercept = mean(waist_w$waist)-sd(waist_w$waist),col="red",size=1)+
  geom_vline(xintercept = mean(waist_w$waist)+0.33*sd(waist_w$waist),col="#003300",size=1)+
  geom_vline(xintercept = mean(waist_w$waist)-0.33*sd(waist_w$waist),col="#003300",size=1)+
  geom_text(aes(round(mean(waist_w$waist)-0.33*sd(waist_w$waist),0),y=800),label=round(mean(waist_w$waist)-0.33*sd(waist_w$waist),0))+
  geom_text(aes(round(mean(waist_w$waist)+0.33*sd(waist_w$waist),0),y=800),label=round(mean(waist_w$waist)+0.33*sd(waist_w$waist),0))+
  geom_text(aes(round(mean(waist_w$waist)-sd(waist_w$waist),0),y=800),label=round(mean(waist_w$waist)-sd(waist_w$waist),0))+
  geom_text(aes(round(mean(waist_w$waist)+sd(waist_w$waist),0),y=800),label=round(mean(waist_w$waist)+sd(waist_w$waist),0))+
  geom_text(aes(round(mean(waist_w$waist),0),y=800),label=round(mean(waist_w$waist),0))

windows()
waist_plot
waist_plot2

#엉덩이
hip_m<-subset(total_m,hip>=mean(total_m$hip)-2*sd(total_m$hip) & hip<=mean(total_m$hip)+2*sd(total_m$hip))
hip_w<-subset(total_w,hip>=mean(total_w$hip)-2*sd(total_w$hip) & hip<=mean(total_w$hip)+2*sd(total_w$hip))

#각구간별 평균값 - 남자
mean(hip_m[hip_m$hip <= mean(hip_m$hip)-sd(hip_m$hip),]$hip)
mean(hip_m[hip_m$hip <= mean(hip_m$hip)-0.33*sd(hip_m$hip) & hip_m$hip >= mean(hip_m$hip)-sd(hip_m$hip),]$hip)
mean(hip_m[hip_m$hip >= mean(hip_m$hip)-0.33*sd(hip_m$hip) & hip_m$hip <= mean(hip_m$hip)+0.33*sd(hip_m$hip),]$hip)
mean(hip_m[hip_m$hip >= mean(hip_m$hip)+0.33*sd(hip_m$hip) & hip_m$hip <= mean(hip_m$hip)+sd(hip_m$hip),]$hip)
mean(hip_m[hip_m$hip >= mean(hip_m$hip)+sd(hip_m$hip),]$hip)

#각구간별 평균값 - 여자
mean(hip_w[hip_w$hip <= mean(hip_w$hip)-sd(hip_w$hip),]$hip)
mean(hip_w[hip_w$hip <= mean(hip_w$hip)-0.33*sd(hip_w$hip) & hip_w$hip >= mean(hip_w$hip)-sd(hip_m$hip),]$hip)
mean(hip_w[hip_w$hip >= mean(hip_w$hip)-0.33*sd(hip_w$hip) & hip_w$hip <= mean(hip_w$hip)+0.33*sd(hip_m$hip),]$hip)
mean(hip_w[hip_w$hip >= mean(hip_w$hip)+0.33*sd(hip_w$hip) & hip_w$hip <= mean(hip_w$hip)+sd(hip_m$hip),]$hip)
mean(hip_w[hip_w$hip >= mean(hip_w$hip)+sd(hip_w$hip),]$hip)


hip_plot<-ggplot(hip_m, aes(x=hip)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(hip)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "어깨너비",
                     breaks = seq(min(hip_m$hip),max(hip_m$hip), 25),
                     limits=c(min(hip_m$hip),max(hip_m$hip)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(hip_m$hip)+sd(hip_m$hip),col="red",size=1)+
  geom_vline(xintercept = mean(hip_m$hip)-sd(hip_m$hip),col="red",size=1)+
  geom_vline(xintercept = mean(hip_m$hip)+0.33*sd(hip_m$hip),col="#003300",size=1)+
  geom_vline(xintercept = mean(hip_m$hip)-0.33*sd(hip_m$hip),col="#003300",size=1)+
  geom_text(aes(round(mean(hip_m$hip)-0.33*sd(hip_m$hip),0),y=800),label=round(mean(hip_m$hip)-0.33*sd(hip_m$hip),0))+
  geom_text(aes(round(mean(hip_m$hip)+0.33*sd(hip_m$hip),0),y=800),label=round(mean(hip_m$hip)+0.33*sd(hip_m$hip),0))+
  geom_text(aes(round(mean(hip_m$hip)-sd(hip_m$hip),0),y=800),label=round(mean(hip_m$hip)-sd(hip_m$hip),0))+
  geom_text(aes(round(mean(hip_m$hip)+sd(hip_m$hip),0),y=800),label=round(mean(hip_m$hip)+sd(hip_m$hip),0))+
  geom_text(aes(round(mean(hip_m$hip),0),y=800),label=round(mean(hip_m$hip),0))


hip_plot2<-ggplot(hip_w, aes(x=hip)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(hip)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "hip",
                     breaks = seq(min(hip_w$hip),max(hip_w$hip), 25),
                     limits=c(min(hip_w$hip),max(hip_w$hip)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(hip_w$hip)+sd(hip_w$hip),col="red",size=1)+
  geom_vline(xintercept = mean(hip_w$hip)-sd(hip_w$hip),col="red",size=1)+
  geom_vline(xintercept = mean(hip_w$hip)+0.33*sd(hip_w$hip),col="#003300",size=1)+
  geom_vline(xintercept = mean(hip_w$hip)-0.33*sd(hip_w$hip),col="#003300",size=1)+
  geom_text(aes(round(mean(hip_w$hip)-0.33*sd(hip_w$hip),0),y=800),label=round(mean(hip_w$hip)-0.33*sd(hip_w$hip),0))+
  geom_text(aes(round(mean(hip_w$hip)+0.33*sd(hip_w$hip),0),y=800),label=round(mean(hip_w$hip)+0.33*sd(hip_w$hip),0))+
  geom_text(aes(round(mean(hip_w$hip)-sd(hip_w$hip),0),y=800),label=round(mean(hip_w$hip)-sd(hip_w$hip),0))+
  geom_text(aes(round(mean(hip_w$hip)+sd(hip_w$hip),0),y=800),label=round(mean(hip_w$hip)+sd(hip_w$hip),0))+
  geom_text(aes(round(mean(hip_w$hip),0),y=800),label=round(mean(hip_w$hip),0))

windows()
hip_plot
hip_plot2

#넙다리
thigh_m<-subset(total_m,thigh>=mean(total_m$thigh)-2*sd(total_m$thigh) & thigh<=mean(total_m$thigh)+2*sd(total_m$thigh))
thigh_w<-subset(total_w,thigh>=mean(total_w$thigh)-2*sd(total_w$thigh) & thigh<=mean(total_w$thigh)+2*sd(total_w$thigh))

#각구간별 평균값 - 남자
mean(thigh_m[thigh_m$thigh <= mean(thigh_m$thigh)-sd(thigh_m$thigh),]$thigh)
mean(thigh_m[thigh_m$thigh <= mean(thigh_m$thigh)-0.33*sd(thigh_m$thigh) & thigh_m$thigh >= mean(thigh_m$thigh)-sd(thigh_m$thigh),]$thigh)
mean(thigh_m[thigh_m$thigh >= mean(thigh_m$thigh)-0.33*sd(thigh_m$thigh) & thigh_m$thigh <= mean(thigh_m$thigh)+0.33*sd(thigh_m$thigh),]$thigh)
mean(thigh_m[thigh_m$thigh >= mean(thigh_m$thigh)+0.33*sd(thigh_m$thigh) & thigh_m$thigh <= mean(thigh_m$thigh)+sd(thigh_m$thigh),]$thigh)
mean(thigh_m[thigh_m$thigh >= mean(thigh_m$thigh)+sd(thigh_m$thigh),]$thigh)

#각구간별 평균값 - 여자
mean(thigh_w[thigh_w$thigh <= mean(thigh_w$thigh)-sd(thigh_w$thigh),]$thigh)
mean(thigh_w[thigh_w$thigh <= mean(thigh_w$thigh)-0.33*sd(thigh_w$thigh) & thigh_w$thigh >= mean(thigh_w$thigh)-sd(thigh_m$thigh),]$thigh)
mean(thigh_w[thigh_w$thigh >= mean(thigh_w$thigh)-0.33*sd(thigh_w$thigh) & thigh_w$thigh <= mean(thigh_w$thigh)+0.33*sd(thigh_m$thigh),]$thigh)
mean(thigh_w[thigh_w$thigh >= mean(thigh_w$thigh)+0.33*sd(thigh_w$thigh) & thigh_w$thigh <= mean(thigh_w$thigh)+sd(thigh_m$thigh),]$thigh)
mean(thigh_w[thigh_w$thigh >= mean(thigh_w$thigh)+sd(thigh_w$thigh),]$thigh)


thigh_plot<-ggplot(thigh_m, aes(x=thigh)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(thigh)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "어깨너비",
                     breaks = seq(min(thigh_m$thigh),max(thigh_m$thigh), 25),
                     limits=c(min(thigh_m$thigh),max(thigh_m$thigh)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(thigh_m$thigh)+sd(thigh_m$thigh),col="red",size=1)+
  geom_vline(xintercept = mean(thigh_m$thigh)-sd(thigh_m$thigh),col="red",size=1)+
  geom_vline(xintercept = mean(thigh_m$thigh)+0.33*sd(thigh_m$thigh),col="#003300",size=1)+
  geom_vline(xintercept = mean(thigh_m$thigh)-0.33*sd(thigh_m$thigh),col="#003300",size=1)+
  geom_text(aes(round(mean(thigh_m$thigh)-0.33*sd(thigh_m$thigh),0),y=800),label=round(mean(thigh_m$thigh)-0.33*sd(thigh_m$thigh),0))+
  geom_text(aes(round(mean(thigh_m$thigh)+0.33*sd(thigh_m$thigh),0),y=800),label=round(mean(thigh_m$thigh)+0.33*sd(thigh_m$thigh),0))+
  geom_text(aes(round(mean(thigh_m$thigh)-sd(thigh_m$thigh),0),y=800),label=round(mean(thigh_m$thigh)-sd(thigh_m$thigh),0))+
  geom_text(aes(round(mean(thigh_m$thigh)+sd(thigh_m$thigh),0),y=800),label=round(mean(thigh_m$thigh)+sd(thigh_m$thigh),0))+
  geom_text(aes(round(mean(thigh_m$thigh),0),y=800),label=round(mean(thigh_m$thigh),0))


thigh_plot2<-ggplot(thigh_w, aes(x=thigh)) + 
  geom_histogram(fill="#66CCFF",col="white")+
  geom_vline(aes(xintercept=mean(thigh)),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name = "thigh",
                     breaks = seq(min(thigh_w$thigh),max(thigh_w$thigh), 25),
                     limits=c(min(thigh_w$thigh),max(thigh_w$thigh)))+
  scale_y_continuous(name="사람빈도",breaks=seq(0,1000,100),limits=c(0,1000))+
  geom_vline(xintercept = mean(thigh_w$thigh)+sd(thigh_w$thigh),col="red",size=1)+
  geom_vline(xintercept = mean(thigh_w$thigh)-sd(thigh_w$thigh),col="red",size=1)+
  geom_vline(xintercept = mean(thigh_w$thigh)+0.33*sd(thigh_w$thigh),col="#003300",size=1)+
  geom_vline(xintercept = mean(thigh_w$thigh)-0.33*sd(thigh_w$thigh),col="#003300",size=1)+
  geom_text(aes(round(mean(thigh_w$thigh)-0.33*sd(thigh_w$thigh),0),y=800),label=round(mean(thigh_w$thigh)-0.33*sd(thigh_w$thigh),0))+
  geom_text(aes(round(mean(thigh_w$thigh)+0.33*sd(thigh_w$thigh),0),y=800),label=round(mean(thigh_w$thigh)+0.33*sd(thigh_w$thigh),0))+
  geom_text(aes(round(mean(thigh_w$thigh)-sd(thigh_w$thigh),0),y=800),label=round(mean(thigh_w$thigh)-sd(thigh_w$thigh),0))+
  geom_text(aes(round(mean(thigh_w$thigh)+sd(thigh_w$thigh),0),y=800),label=round(mean(thigh_w$thigh)+sd(thigh_w$thigh),0))+
  geom_text(aes(round(mean(thigh_w$thigh),0),y=800),label=round(mean(thigh_w$thigh),0))

windows()
thigh_plot
thigh_plot2






