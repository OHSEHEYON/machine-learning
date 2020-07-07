# sex,smoker,region�� ��������������
insurance <- read.csv("C:\\R2\\dataset\\insurance.csv")
str(insurance)
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)
str(insurance)

# ���Ӻ����� charges�̹Ƿ� �����ľ�
# hist�� ���� ������ �������� �������� �˼�����
summary(insurance$charges)
hist(insurance$charges)

# Ư¡�� ����Ž�� : ������
# �װ��� ��ġ������ ���� ������ ����
cor(insurance[c("age","bmi","children","charges")])

# Ư¡�� ���� �ð�ȭ : ���������
# age�� charges�� ����� ��������� ���� �������� ����
# bmi�� expense�� ����� �ΰ��� ���еǴ� �׷��� ����
pairs(insurance[c("age","bmi","children","charges")])

# Ư¡�� ���� : ������ ���������
# �κ������� �������� Ÿ��������� ��Ÿ��
# Ÿ���� �þ������� �������� ������
# Ÿ���� �߽ɿ� �ִ� ���� �� ������ ���� ��հ� ����
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])


# ���Ʒ�
reg_model <- lm(charges~age+children+bmi+sex+smoker+region, data=insurance)
reg_model

# ����
# ��������� 0.7509, �� P���� 0.05���� �����Ƿ� ������
# regionnorthwest , sexmale�� �������� P���� 0.05���� ŭ.
summary(reg_model)


# �𵨰���
# age�� ���� charges�� 2�����¿� �������̶�� ����
# bmi�� ���� charges�� �������� �ξ� ���ú����� �ٲٴ°� �������̶�� ����
# �񸸰� �������� ��ȣ�ۿ� ����
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >=30, 1, 0)
reg_model2 <- lm(charges~age+children+bmi+sex+smoker+region+age2+bmi30*smoker, data=insurance)
summary(reg_model2)