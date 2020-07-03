# sms���� ���� ������ �ҷ�����
sms <- read.csv("C:\\R2\\dataset\\sms_spam.csv")
str(sms)

# type�׸��� ���� ���ں����̴�. ������ ���� ������ �����̱� ������ ���ͷ� ��ȯ
sms$type <- factor(sms$type)
str(sms$type)
table(sms$type)

# �ؽ�Ʈ������ ó���� ù�ܰ�� �ؽ�Ʈ ������ ������ ���۽��� �����ϴ� ��
# ���������� SMS �޽��� ����
# ���۽��� �����ϱ����� �ֹ߼����۽��� �����ϴ� VCorpus()�Լ��� ���
library(tm)
sms_corpus <- VCorpus(VectorSource(sms$text))
sms_corpus # �����Ϳ� 5,559���� ������ ���ԵǾ�����

# tm���۽��� ���ո���Ʈ
# Ư�� �޽����� ����� �������� ����Ʈ �����ڿ� �Բ� inspect()�Լ� ���
inspect(sms_corpus[1:2])

# ���� �޽��� �ؽ�Ʈ�� ������ as.character()�Լ� ���
as.character(sms_corpus[[1]])

# ���� ���� �̽��� �ؽ�Ʈ�� lapply()�Լ��� ���
lapply(sms_corpus[1:3], as.character)

# �޽��������� �������� �������� ���͸��ϱ� ���ؼ��� �ܾ�������� Ȱ���Ѵ�
# �׷��� ���ؼ��� ������ ��ǻ�Ͱ� ������ �� �ֵ��� �ҿ��(to, and, but, or ��), ������ ���� ���־� �����ؾ��Ѵ�
# tm_map()�Լ��� ����Ͽ� ���۽��� �����Ѵ�.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower), lazy=TRUE) #�ҹ��ڷ�
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) # ��������
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # �ҿ������
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) # ������ ����
lapply(sms_corpus_clean[1:3], as.character)

# SnowballC ��Ű���� wordStem()�Լ��� ������ ���ں��Ϳ� ���� ��� ������ ������ ��� ���͸� ��ȯ�Ѵ�
# wordStem()�Լ��� �ؽ�Ʈ ������ ��ü ���۽��� �����ϱ����� stemDocument()��ȯ�� �̟G
# stripWhitespace()��ȯ�� �̿��� �߰� ������ ����
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus_clean[1:3], as.character)




# ������ �غ� : �ؽ�Ʈ ������ �ܾ�� ������
# ���۽��� ����-�ܾ� ���(DTM)���� ��ȯ�ϴ� ��ūȭ �۾��� �ʿ��ϴ�
# DocumentTermMatrix()�Լ��� ���۽��� �����ͼ� DTM�̶�� �ϴ� ������ ������ �����
# DTM�� ���� ����(sms�޽���)�� ��Ÿ���� ���� ���(�ܾ�)�� ��Ÿ����
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm  # ��(���� : 5559��), ��(��� : 6561��)

# ����Ŭ���� : �ؽ�Ʈ������ ����ȭ
library(wordcloud)
spam <- subset(sms, type=="spam")
ham <- subset(sms, type=="ham")
summary(spam)
summary(ham)
wordcloud(spam$text, max.words=40, scale=c(3,0.5)) # ���� �޽����� call, fre, now ���� �ܾ ����
wordcloud(ham$text, max.words=40, scale=c(3,0.5)) # �� �޽����� can, will, good ���� �ܾ ����

# �������غ� : �Ʒ� �� ���� �����ͼ� ����
# �뷫������ �����͸� �Ʒÿ� 75%�� �׽� 25%�� �и�/
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
sms_train_labels <- sms[1:4169,]$type
sms_test_labels <- sms[4170:5559,]$type

# �Ʒõ����Ϳ� �׽�Ʈ�����͸�� 13%������ �����ϰ� �ִ�.
# ���Ը޼����� �� �����ͼ¿� �յ��ϰ� ���ҵ����� �����ְ� �ִ�.
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# findFreqTerms()�Լ��� ���ֳ�Ÿ���� �ܾ ã�� �˷���
findFreqTerms(sms_dtm_train, 5) # �ּ� 5�� �̻� ��Ÿ���� �ܾ�
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words) # �ּҴټ����� sms �޽����� ��Ÿ���� �� 1,136��

# DTM�� ���͸��� Ư�����Ϳ� ��Ÿ���� �� �����ϰ� �� �ʿ䰡 �ִ�
# DTM�� Ư�� �κ��� ��û�Ϸ��� �������������� ���̸��� DTM�� ���Ե� �ܾ ������
# ���� �Ʒ� �� ���� �����ͼ��� 1,137���� Ư¡�� �����ϸ�, �� Ư¡�� �ּ� �ټ����� �޽������� ��Ÿ���� �ܾ �ش�
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

# ���̺� ������ �з���� �Ϲ������� ������ Ư¡���� �� �����Ϳ� ���� �Ʒõȴ�.
# DTM�� ���� �����̸�, �޽����� ��Ÿ���� �ܾ��� Ƚ���� �����ϱ� �빮�̴�
# ���� ���� �ܾ ��Ÿ������ ���ο� ���� �ܼ��� yes or no �� ��Ÿ���� ������ ������ �ٲ� �ʿ䰡 �ִ�.
convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "NO")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN=2, convert_counts) # margin=2�� ��
sms_test <- apply(sms_dtm_freq_test, MARGIN=2, convert_counts)



# naiveBayes(train, class)
# predict(m, test) # m�� naibeBayes()�Լ��� ���� �Ʒõ� ��
library(e1071)
sms_naive <- naiveBayes(sms_train, sms_train_labels)
sms_test_predict <- predict(sms_naive, sms_test)

# �𵨼��� Ȯ�ι��1 
# 1,390���� �޽��� �� 36���� ����Ȯ�ϰ� �з���
library(gmodels)
CrossTable(sms_test_predict, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))

# �𵨼��� Ȯ�ι��2
library(caret)
confusionMatrix(sms_test_predict, sms_test_labels)


# �� ���ɰ��� : ���ö� ������
# 1,390���� �޽��� �� 34���� ����Ȯ�ϰ� �з���
sms_naive2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_predict2 <- predict(sms_naive2, sms_test)
CrossTable(sms_test_predict2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))