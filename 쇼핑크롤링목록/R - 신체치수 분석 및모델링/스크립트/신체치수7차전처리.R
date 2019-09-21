#7차 신체치수
dr15<-data.frame(fread("3차원_7차_직접측정_2015.csv"))
str(dr15)

#14세이상
dr15_age<- subset(dr15,ⓞ_06_나이_반올림>=14)
str(dr15_age)

#전처리
str(cha15)
cha15<-dr15_age %>% select("ⓞ_02_성별","ⓞ_06_나이_반올림",
                           "X._003_키","X._031_몸무게","X._055_어깨가쪽사이길이",
                           "X._039_가슴둘레","X._044_배둘레",
                           "X._037_목둘레","X._064_팔길이",
                           "X._067_겨드랑둘레","X._005_목뒤높이",
                           "X._015_위앞엉덩뼈가시높이","X._132_손목둘레",
                           "X._130_위팔둘레","X._042_허리둘레",
                           "X._045_엉덩이둘레","X._121_넙다리둘레",
                           "X._127_발목최대둘레")

colnames(cha15)<-c("성별","나이","키","몸무게",
                   "어깨가쪽사이길이","가슴둘레",
                   "배둘레","목둘레","팔길이",
                   "겨드랑둘레","목뒤높이",
                   "위앞엉덩뼈가시높이","손목둘레",
                   "편위팔둘레","허리둘레",
                   "엉덩이둘레",
                   "넙다리둘레","발목최대둘레")
str(cha15)


version15<-cha15
str(version15)
#문자열처리
version15$목뒤높이<-gsub(",","",version15$목뒤높이)
version15$목뒤높이<-as.numeric(version15$목뒤높이)

version15$키<-gsub(",","",version15$키)
version15$키<-as.numeric(version15$키)

version15$엉덩이둘레<-gsub(",","",version15$엉덩이둘레)
version15$엉덩이둘레<-as.numeric(version15$엉덩이둘레)

version15$허리둘레<-gsub(",","",version15$허리둘레)
version15$허리둘레<-as.numeric(version15$허리둘레)

version15$위앞엉덩뼈가시높이<-gsub(",","",version15$위앞엉덩뼈가시높이)
version15$위앞엉덩뼈가시높이<-as.numeric(version15$위앞엉덩뼈가시높이)

version15$목뒤높이<-gsub(",","",version15$목뒤높이)
version15$목뒤높이<-as.numeric(version15$목뒤높이)

version15$배둘레<-gsub(",","",version15$배둘레)
version15$배둘레<-as.numeric(version15$배둘레)
version15$가슴둘레<-gsub(",","",version15$가슴둘레)
version15$가슴둘레<-as.numeric(version15$가슴둘레)


#상체총길이구하기
version15$상체총길이<-version15$목뒤높이-version15$위앞엉덩뼈가시높이
str(version15)





################################################
###################6,7차데이터 병합
data<-read.csv("data.csv")
str(data)
str(mom04)
summary(mom04)

sum(is.na(mom04))
mom04_n<-na.omit(mom04)
str(mom04_n)
