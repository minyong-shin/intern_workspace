test_data <- data.frame(body = c("가슴","어깨","소매길이","암홀","총장"),
                        num = c(3,-2,4,-1,-6))
test_data
test_data$comment <- c("살짝커요","살짝작아요","살짝커요","적당해요","많이 작아요")
library(ggplot2)
windows()
#차등그래프
ggplot(test_data,aes(x=body,y=num,fill = body))+
  geom_bar(stat="identity")+
  scale_y_continuous(name = "사이즈차이",
                     limits=c(-8, 8),
                     breaks = seq(-8,8, 1))+
  xlab("신체치수")+
  geom_text(aes(label=paste(comment,"(",paste(ifelse(num>0,paste("+",num),num),"cm",")"))),
            position=position_dodge(width=0.8), vjust=1.5)

#겹치기1
test_data2 <- data.frame(body = c("가슴","어깨","소매길이","암홀","총장","가슴","어깨","소매길이","암홀","총장"),
                         body_num = c(60,47,51,24,70,63,45,55,23,64),
                         legend = c("body","body","body","body","body","cloth","cloth","cloth","cloth","cloth"))
posn_d <- position_dodge(0.2)
ggplot(test_data2,aes(x=body,y=body_num,fill=legend))+
  geom_bar(position=posn_d,stat="identity",alpha=0.6)+
  geom_text(x=1,y=60,label="살짝커요(+3cm)",size=3,col="black")+
  geom_text(x=2,y=51,label="적당해요(+3cm)",size=3,col="black")+
  geom_text(x=3,y=24,label="살짝커요(+3cm)",size=3,col="black")+
  geom_text(x=4,y=45,label="살짝커요(+3cm)",size=3,col="black")+
  geom_text(x=5,y=64,label="살짝커요(+3cm)",size=3,col="black")
  


#겹치기2
test_data2 <- data.frame(body = c("가슴","어깨","소매길이","암홀","총장"),
                         body_num = c(60,47,51,24,70),
                         cloth_num = c(63,45,55,23,64))

windows()
ggplot(test_data2,aes(x=body))+
  geom_bar(aes(y=body_num,fill= "몸size"),alpha=0.6,stat="identity")+
  geom_bar(aes(y=cloth_num,fill="옷size"),alpha=1,stat="identity")


#겹치기3
#기본정보 데이터를 먼저 만들고 난 뒤 body_num에 넣는 방식으로 하면 자동화 가능
test_data2 <- data.frame(body = c("가슴","어깨","소매길이","암홀","총장","가슴","어깨","소매길이","암홀","총장","가슴","어깨","소매길이","암홀","총장"),
                         body_num = c(60,47,51,24,70,63,45,55,23,64,3,-2,4,-1,-6),
                         legend = c("body","body","body","body","body","cloth","cloth","cloth","cloth","cloth","옷_몸차이","옷_몸차이","옷_몸차이","옷_몸차이","옷_몸차이"))
posn_d <- position_dodge(0.2)
ggplot(test_data2,aes(x=body,y=body_num,fill=legend))+
  geom_bar(position=posn_d,stat="identity",alpha=0.6)+
  geom_text(x=1,y=60,label="살짝커요(+3cm)",size=3,col="black")+
  geom_text(x=2,y=51,label="적당해요(+3cm)",size=3,col="black")+
  geom_text(x=3,y=24,label="살짝커요(+3cm)",size=3,col="black")+
  geom_text(x=4,y=45,label="살짝커요(+3cm)",size=3,col="black")+
  geom_text(x=5,y=64,label="살짝커요(+3cm)",size=3,col="black")




#rader chart
install.packages("radarchart")
library(radarchart)

labs <- c("가슴단면", "어깨너비", "소매길이",
          "암홀",  "총장")

scores <- list(
  "신체사이즈" = c(60,47,51,24,70),
  "옷사이즈" = c(63,45,55,23,64)
)

chartJSRadar(scores = scores, labs = labs, maxScale = 75)
#데이터프레임 형태로 나타낼 수 있음 1번째 열을 label로 설정해 놓았을 때만

#운영솔루션별 점유율
x <- c(672,137,121)
labels <- c("cafe24","Makeshop","기타")

prob <- round(x/sum(x)*100)
lbls <- paste(labels, prob) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x,labels = lbls, col=rainbow(length(lbls)),main="운영솔루션 점유율")

install.packages("plotrix")
library(plotrix)
pie3D(x,labels=lbls,explode=0.05,main="운영솔루션 점유율")

solu <- data.frame(solution = c("cafe24","Makeshop","기타"),per=c(672,137,121))
str(solu)
solu$perc <- solu$per/sum(solu$per)*100
library(ggplot2)
windows()
ggplot(solu, aes(x = "", y=per, fill = solution)) + 
  geom_col(position = 'fill', width=1)+
  coord_polar(theta = "y")+
  geom_label(aes(label = paste0(sprintf("%0.1f", round(perc, digits = 2)), "%")), 
             position = position_fill(vjust = 0.4))+
  ggtitle("운영솔루션 점유율(여성)")+
  theme(plot.title = element_text(size = 35, hjust=0.5, face = "bold"),
        legend.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20,face="bold"))

#남성
solu <- data.frame(solution = c("cafe24","Makeshop","기타"),per=c(155,38,25))
str(solu)
solu$perc <- solu$per/sum(solu$per)*100
library(ggplot2)
windows()
ggplot(solu, aes(x = "", y=per, fill = solution)) + 
  geom_col(position = 'fill', width=1)+
  coord_polar(theta = "y")+
  geom_label(aes(label = paste0(sprintf("%0.1f", round(perc, digits = 2)), "%")), 
             position = position_fill(vjust = 0.4))+
  ggtitle("운영솔루션 점유율(남성)")+
  theme(plot.title = element_text(size = 35, hjust=0.5, face = "bold"),
        legend.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=20,face="bold"))
