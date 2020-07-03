# sms스팸 모음 데이터 불러오기
sms <- read.csv("C:\\R2\\dataset\\sms_spam.csv")
str(sms)

# type항목은 현재 문자벡터이다. 하지만 원래 범주형 변수이기 때문에 팩터로 변환
sms$type <- factor(sms$type)
str(sms$type)
table(sms$type)

# 텍스트데이터 처리의 첫단계는 텍스트 문서의 모음인 코퍼스를 생성하는 것
# 예제에서는 SMS 메시지 모음
# 코퍼스를 생성하기위해 휘발성코퍼스를 참조하는 VCorpus()함수를 사용
library(tm)
sms_corpus <- VCorpus(VectorSource(sms$text))
sms_corpus # 데이터에 5,559개의 문서가 포함되어있음

# tm코퍼스는 복합리스트
# 특정 메시지의 요약을 얻으려면 리스트 연산자와 함께 inspect()함수 사용
inspect(sms_corpus[1:2])

# 실제 메시지 텍스트를 보려면 as.character()함수 사용
as.character(sms_corpus[[1]])

# 여러 실제 미시지 텍스트를 lapply()함수를 사용
lapply(sms_corpus[1:3], as.character)

# 메시지내용이 스팸인지 햄인지를 필터링하기 위해서는 단어빈도패턴을 활용한다
# 그러기 위해서는 문장을 컴퓨터가 이해할 수 있도록 불용어(to, and, but, or 등), 구두점 등을 없애어 정제해야한다
# tm_map()함수를 사용하여 코퍼스를 정제한다.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower), lazy=TRUE) #소문자로
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # 숫자제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # 불용어제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # 구두점 제거
lapply(sms_corpus_clean[1:3], as.character)

# SnowballC 패키지는 wordStem()함수를 제공해 문자벡터에 대해 어근 형태의 동일한 용어 벡터를 반환한다
# wordStem()함수를 텍스트 문서의 전체 코퍼스에 적용하기위해 stemDocument()변환을 이욯
# stripWhitespace()변환을 이용해 추가 여백을 제거
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus_clean[1:3], as.character)




# 데이터 준비 : 텍스트 문서를 단어로 나누기
# 코퍼스를 문서-단어 행렬(DTM)으로 변환하는 토큰화 작업이 필요하다
# DocumentTermMatrix()함수는 코퍼스를 가져와서 DTM이라고 하는 데이터 구조를 만든다
# DTM의 행은 문서(sms메시지)를 나타내고 열은 용어(단어)를 나타낸다
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm  # 행(문서 : 5559개), 열(용어 : 6561개)

# 워드클라우드 : 텍스트데이터 가시화
library(wordcloud)
spam <- subset(sms, type=="spam")
ham <- subset(sms, type=="ham")
summary(spam)
summary(ham)
wordcloud(spam$text, max.words=40, scale=c(3,0.5)) # 스팸 메시지는 call, fre, now 같은 단어를 포함
wordcloud(ham$text, max.words=40, scale=c(3,0.5)) # 햄 메시지는 can, will, good 같은 단어를 포함

# 데이터준비 : 훈련 및 시험 데이터셋 생성
# 대략적으로 데리터를 훈련용 75%와 테스 25%로 분리/
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
sms_train_labels <- sms[1:4169,]$type
sms_test_labels <- sms[4170:5559,]$type

# 훈련데이터와 테스트데이터모두 13%스팸을 포함하고 있다.
# 스팸메세지가 두 데이터셋에 균등하게 분할됐음을 보여주고 있다.
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# findFreqTerms()함수는 자주나타나는 단어를 찾아 알려줌
findFreqTerms(sms_dtm_train, 5) # 최소 5번 이상 나타나는 단어
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words) # 최소다섯개의 sms 메시지에 나타나는 용어가 1,136개

# DTM을 필터링해 특정벡터에 나타나는 용어만 포함하게 할 필요가 있다
# DTM의 특정 부분을 요청하려면 데이터프레임의 열이름이 DTM에 포함된 단어를 따른다
# 이제 훈련 및 시험 데이터셋은 1,137개의 특징을 포함하며, 이 특징은 최소 다섯개의 메시지에서 나타나는 단어에 해당
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

# 나이브 베이즈 분류기는 일반적으로 범주형 특징으로 된 데이터에 대해 훈련된다.
# DTM의 셀은 숫자이며, 메시지에 나타나는 단어의 횟수를 측정하기 대문이다
# 셀의 값을 단어가 나타나는지 여부에 따라 단순히 yes or no 를 나타내는 범주형 변수로 바꿀 필요가 있다.
convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "NO")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN=2, convert_counts) # margin=2는 열
sms_test <- apply(sms_dtm_freq_test, MARGIN=2, convert_counts)



# naiveBayes(train, class)
# predict(m, test) # m은 naibeBayes()함수에 의해 훈련된 모델
library(e1071)
sms_naive <- naiveBayes(sms_train, sms_train_labels)
sms_test_predict <- predict(sms_naive, sms_test)

# 모델성능 확인방법1 
# 1,390개의 메시지 중 36개만 부정확하게 분류됨
library(gmodels)
CrossTable(sms_test_predict, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))

# 모델성능 확인방법2
library(caret)
confusionMatrix(sms_test_predict, sms_test_labels)


# 모델 성능개선 : 라플라스 추정량
# 1,390개의 메시지 중 34개만 부정확하게 분류됨
sms_naive2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_predict2 <- predict(sms_naive2, sms_test)
CrossTable(sms_test_predict2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))
