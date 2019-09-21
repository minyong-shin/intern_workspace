#5차 신체치수
library(data.table)
dr04<-data.frame(fread("5차_직접측정_2004.csv"))
str(dr04)
dr04_a<-data.frame(fread("3차원_5차_직접측정_2004.csv"))
str(dr04_a)
dr04_b<-data.frame(fread("3차원_5차_형상측정_2004.csv"))
str(dr04_b)



#5차 신체치수 전처리
library(dplyr)
str(dr04_a)
sel <- dr04_a %>% select("ID","성별","나이")
str(sel)
dr04_bb<-merge(sel,dr04_b,by="ID",all.x=T)
View(dr04_bb)

#형상측정
library(ggplot2)
dr04_bb<-subset(dr04_bb,나이>=14)
sort(unique(dr04_bb$나이))
str(dr04_bb)

#데이터를 늘리기 위해서
#형상측정 먼저
mom<-merge(dr04_bb,dr04_aa[,c(1,23)],by="ID",all.x=T)
str(mom)
#형상측정에서 데이터를 뽑아 놓은 것
mom04 <- mom %>% select("ID","성별","나이","키","몸무게",
                            "어깨가쪽사이길이","가슴둘레",
                        "배둘레","목둘레","팔길이",
                        "겨드랑둘레","목뒤높이",
                        "위앞엉덩뼈가시높이","손목둘레",
                        "편위팔둘레","허리둘레",
                        "엉덩이둘레",
                        "넙다리둘레","발목최대둘레")

str(mom04)

#이것만 사용해보자
mom04<-mom04[,-1]
mom04$상체총길이<-(mom04$목뒤높이-mom04$위앞엉덩뼈가시높이)

#직접측정 3d
dr04_aa<-subset(dr04_a,나이>=14)
sort(unique(dr04_aa$나이))
str(dr04_aa)

#그다음 직접측정
mom04_a<-merge(dr04_aa[,c(1,2,3,5,23,9,6,12:17)],mom04[,c(1,9,13,14,15,18,19)],by="ID",all.x=T)
str(mom04_a)
mom04_aa<-mom04_a %>% select("ID","성별","나이","키","몸무게",
                             "어깨가쪽사이길이","가슴둘레",
                             "배둘레","목둘레","팔길이",
                             "겨드랑둘레","목뒤높이",
                             "위앞엉덩뼈가시높이","손목둘레",
                             "편위팔둘레","허리둘레",
                             "엉덩이둘레",
                             "넙다리둘레","발목최대둘레")

str(mom04_aa)

mom04_aa<-mom04_aa[,-1]

#직접측정
dr04_1<-subset(dr04,나이>=14)
sort(unique(dr04_1$나이))
str(dr04_1)

mom05_a<-dr04_1 %>% select("성별","나이","X.102.키",
                           "X.132.몸무게","X.228.어깨사이길이",
                           "X.208.가슴둘레","X.212.배꼽수준허리둘레",
                           "X.206.목둘레","X.236.팔길이",
                           "X.239.겨드랑둘레","X.104.목뒤높이",
                           "X.109.엉덩이높이","X.242.손목둘레",
                           "X.240.위팔둘레","X.211.허리둘레",
                           "X.213.엉덩이둘레","X.214.넙다리둘레",
                           "X.220.발목최대둘레")
colnames(mom05_a)<-c("성별","나이","키","몸무게",
                     "어깨가쪽사이길이","가슴둘레",
                     "배둘레","목둘레","팔길이",
                     "겨드랑둘레","목뒤높이",
                     "위앞엉덩뼈가시높이","손목둘레",
                     "편위팔둘레","허리둘레",
                     "엉덩이둘레",
                     "넙다리둘레","발목최대둘레")
str(mom05_a)

#데이터 rbind
version04<-rbind(mom04,mom04_aa,mom05_a)

#상체총길이구학
str(version04)

version04$상체총길이<-(version04$목뒤높이-version04$위앞엉덩뼈가시높이)
str(version04)







