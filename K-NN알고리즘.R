wbcd <- read.csv("C:\\R2\\dataset\\wisc_bc_data.csv")
str(wbcd)


# 첫번째변수는 id. 무의미한 정보를 제공하므로 제외.
wbcd <- wbcd[-1]
str(wbcd)

# B와 M이 직관적이지 않으니 표기법을 mapvalues()함수를 이용하여 팩터의 라벨명을 변경.
# 각각의 비율을 prop.table을 통해 확인
library(plyr)
wbcd$diagnosis <- mapvalues(wbcd$diagnosis, from=c('M','B'), to=c('악성','양성'))
prop.table(table(wbcd$diagnosis))

# 데이터전처리 - 정규화(0과 1사이의 값)
# lapply()함수를 사용하여 wbcd의 2열부터 31열까지 정규화함수 적용하고 그 값을 df로 반환
normalize <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
} 
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)

# 데이터전처리 - 훈련 및 시험 데이터셋 생성
# 훈련데이터로 처음 469개, 시험데이터로 나머지 100개
# 레이블은 훈련레이블과 시험레이블로 나누어 준비
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
train_labels <- wbcd[1:469,1]
test_labels <- wbcd[470:569,1]

# knn()함수로 K-NN 진행
# k값은 훈련용데이터 개수(469)의 루트값과 유사한 21로.
library(class)
fit <- knn(train=wbcd_train, test=wbcd_test, cl=train_labels, k=21)
summary(fit)

# 모델성능평가
# fit벡터에 있는 예측된 클래스가 test_labels 벡터에 있는 알려진값과 얼마나 잘 일치하는지 확인
# 모델의 정확도는 0.98
library(gmodels)
CrossTable(x=test_labels, y=fit, prop.chisq=FALSE)