# 데이터 전처리
# credit의 default변수는 원래 범주형이므로 바꾸어주자
credit <- read.csv("C:\\R2\\dataset\\credit.csv")
str(credit)
credit$default <- factor(credit$default)

# 대출특징인 대출기간과 금액을 살펴보자
summary(credit$months_loan_duration)
summary(credit$amount)

# default벡터는 채무불이행을 했는지를 알려준다
table(credit$default)

# 수표계좌잔고와 저축계좌잔고는 대출 채무불이행 상태의 중요한 예측변수가 될수 있음
# DM은 독일의 화폐단위
table(credit$checking_balance)
table(credit$savings_balance)



# 데이터준비 : 랜덤한 훈련 및 테스트 데이터셋 생성
# 훈련데이터 90%, 테스트데이터 10%
# set.seed()함수를 사용하면 향후 분석을 반복할때마다 동일한 결과를 얻을 수 있다
set.seed(123)
train_sample <- sample(1000,900) # 1부터 1000까지 랜덤한 900개의 샘플
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

# 각 데이터셋은 약30%의 채무불이행을 가짐 -> 잘 분할됨
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


# 모델훈련 
# credit_train의 17열은 default변수이므로, 제외하고 분류를 위한 목표요인벡터로 제공
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model) # 괄호안에 숫자는 결정기준에 부합하는 개수와 부정확하게 분류된 개수를 나타냄. 트리이후에 혼동행렬을 보여주는데 모델이 11%의 오류율을 가지고 있음을 알려줌

# 모델평가
credit_predict <- predict(credit_model, credit_test)
library(gmodels)              
CrossTable(credit_test$default, credit_predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,  dnn = c('actual default','predicted default'))
           