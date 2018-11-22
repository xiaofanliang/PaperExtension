library(foreign)

#data from student survey 
w1_s <- read.dta("/Users/xiaofanliang/Documents/Minerva/Academic/CS112/Final Project/w1_s/C00124_2en/w1_j_s_v6.0.dta")

#data from parent survey 
w1_p <- read.dta("/Users/xiaofanliang/Documents/Minerva/Academic/CS112/Final Project/w1_p/C00124_8en/w1_j_p_v6.0.dta")

w1_s <- w1_s[which (w1_s$stud_id %in% w1_p$stud_id), ]
spdata <- cbind.data.frame(w1_s, w1_p)

spdata <- spdata[, c('w1nright', 'w1s1131', 'w1s4284', 'w1s108a', 'w1s4221', 'w1s4222', 'w1s4223', 'w1s4224', 'w1p515', 'w1p104', 'w1p121', 'w1p301', 'w1p302', 'w1p303', 'w1p304', 'w1p106', 'w1p512', 'w1p513')]

spdata <- as.data.frame(apply(spdata, 2, trimws)) # trim whitespace from values
spdata[spdata == "missing" | spdata == "illegal value"] <- NA # convert missing data to NA
spdata <- na.omit(droplevels(spdata))# drop NAs and then drop unused factors

attach(spdata)

#Cleaning data for X1 (CramschoolYN)
w1s4284 <- as.numeric(w1s4284)
w1s4284[w1s4284 == 1] <- 1
w1s4284[w1s4284 == 2] <- 0

w1s1131 <- as.numeric(w1s1131)
w1s1131[w1s1131 == 1] <- 0
w1s1131[w1s1131 == 2] <- 1

cramschoolYN <- w1s4284 + w1s1131
cramschoolYN[cramschoolYN > 0] = 1

X1 <- data.frame(cramschoolYN) 

#Cleaning data for X2 (hours spent at cram school)
w1s108a_num <- as.numeric(w1s108a)
w1s108a_num[w1s108a_num == 1] <- 0
w1s108a_num[w1s108a_num == 2] <- 2
w1s108a_num[w1s108a_num == 3] <- 6
w1s108a_num[w1s108a_num == 4] <- 10
w1s108a_num[w1s108a_num == 5] <- 14

X2 <- w1s108a_num

#Cleaning data for X3 (family total income)
w1p515_ordered <- factor(w1p515, levels = c("less than nt$20,000", "nt$20,000 - nt$49,999", "nt$50,000 - nt$99,999", "nt$100,000 - nt$149,999", "nt$150,000 - nt$199,999", "nt$200,000 or more"))

#---Cleaning data for X4 (parents' highest education)----#
#levels(w1p104)
w1p104_ordered <- factor(w1p104, levels = c("junior high school or less", "senior (vocational) high school", "junior/science and technological college", "university", "graduate school", "other"))

w1nright <- as.numeric(levels(w1nright))[w1nright]

#for visual display, change the names
income_ <- w1p515_ordered 
edu_ <- w1p104_ordered
cramhours <- X2

#this will break down the categorical variables and use the level 1 as the reference group. 
regression <- lm(w1nright ~ cramschoolYN + cramhours + income_ + edu_)
w1nright
coef(summary(regression))

everything <- cbind.data.frame(w1nright, cramschoolYN, cramhours, income_, edu_)

#------Logistic Regression-------#

glm.fit <- glm(cramschoolYN ~ income_ + edu_, data = everything, family=binomial)
summary(glm.fit)

train <- sample(c(1:12366), 6183, replace = F) #the row index of training set 
train_set <- everything[train, ]
test_set <- everything[-train, ]

#train the data
glm.fit2 <- glm(cramschoolYN ~ income_ + edu_, data = train_set, family=binomial)
summary(glm.fit2)

#fit the data 
logi.pred.test <- predict(glm.fit2, new = test_set, type = "response")
logi.pred.test[logi.pred.test < 0.5] <- 0
logi.pred.test[logi.pred.test >= 0.5] <- 1

table(logi.pred.test, test_set[["cramschoolYN"]])[1:4]
#accuracy 5299/6183 = 0.857 

#k-fold cross validation 
#set.seed(1)

shuffledata <- everything 
shuffledata<-everything[sample(nrow(everything)),]

folds <- cut(seq(1,nrow(shuffledata)),breaks=10,labels=FALSE)

for(i in 1:10){
  #Segement your data by fold using the which() function 
  train_n <- which(folds==i,arr.ind=TRUE)
  train_set_n <- shuffledata[train_n, ]
  test_set_n <- shuffledata[-train_n, ]
  glm.fit.n <- glm(cramschoolYN ~ income_ + edu_, data = train_set_n, family=binomial)
  logi.pred.test.n <- predict(glm.fit.n, new = test_set_n, type = "response")
  logi.pred.test.n[logi.pred.test.n < 0.5] <- 0
  logi.pred.test.n[logi.pred.test.n >= 0.5] <- 1
  print(table(logi.pred.test.n, test_set_n[["cramschoolYN"]])[1:4])
  #Use the test and train data partitions however you desire...
}

(9499/11129 + (9503+1)/11129 + (9438+17)/11129 + (9509)/11129 + 9503/11120 + 9508/11129 + 9505/11129 + 9484/11129 + (9441+16)/11129)/10
#0.7676 #more type II error 

#----------Logistic Regression with Interaction Term--------#
everything$I <- with(everything, interaction(income_, edu_))
glm.fit.interact <- glm(cramschoolYN ~ I, data = everything, family=binomial)

coef(summary(glm.fit.interact))
everything$income_num <- as.numeric(everything$income_)
everything$edu_num <- as.numeric(everything$edu_)

significant <- everything[which(everything$I == 'nt$20,000 - nt$49,999.junior high school or less' 
                                | everything$I == 'nt$50,000 - nt$99,999.junior high school or less'
                                | everything$I == 'nt$100,000 - nt$149,999.junior high school or less'
                                | everything$I == 'nt$150,000 - nt$199,999.junior high school or less'
                                | everything$I == 'nt$200,000 or more.junior high school or less'
                                | everything$I == 'nt$20,000 - nt$49,999.senior (vocational) high school'
                                | everything$I == 'nt$50,000 - nt$99,999.senior (vocational) high school'
                                | everything$I == 'nt$100,000 - nt$149,999.senior (vocational) high school'
                                | everything$I == 'nt$150,000 - nt$199,999.senior (vocational) high school'
                                | everything$I == 'nt$200,000 or more.senior (vocational) high school'
                                | everything$I == 'nt$20,000 - nt$49,999.junior/science and technological college'
                                | everything$I == 'nt$50,000 - nt$99,999.junior/science and technological college'
                                | everything$I == 'nt$100,000 - nt$149,999.junior/science and technological college'
                                | everything$I == 'nt$150,000 - nt$199,999.junior/science and technological college'
                                | everything$I == 'nt$200,000 or more.junior/science and technological college'
                                | everything$I == 'nt$20,000 - nt$49,999.university'
                                | everything$I == 'nt$50,000 - nt$99,999.university'
                                | everything$I == 'nt$100,000 - nt$149,999.university'
                                | everything$I == 'nt$150,000 - nt$199,999.university'
                                | everything$I == 'nt$200,000 or more.university'
                                | everything$I == 'nt$150,000 - nt$199,999.graduate school'), ]

significant$estimate <- c(0)
significant$estimate[significant$I == 'nt$20,000 - nt$49,999.junior high school or less'] <- 0.62
significant$estimate[significant$I == 'nt$50,000 - nt$99,999.junior high school or less'] <- 1.02
significant$estimate[significant$I == 'nt$100,000 - nt$149,999.junior high school or less'] <- 1.10
significant$estimate[significant$I == 'nt$nt$150,000 - nt$199,999.junior high school or less'] <- 1.47
significant$estimate[significant$I == 'nt$200,000 or more.junior high school or less'] <- 1.26
significant$estimate[significant$I == 'nt$20,000 - nt$49,999.senior (vocational) high school'] <- 0.93
significant$estimate[significant$I == 'nt$50,000 - nt$99,999.senior (vocational) high school'] <- 1.3
significant$estimate[significant$I == 'nt$100,000 - nt$149,999.senior (vocational) high school'] <- 1.36
significant$estimate[significant$I == 'nt$150,000 - nt$199,999.senior (vocational) high school'] <- 0.94
significant$estimate[significant$I == 'nt$200,000 or more.senior (vocational) high school'] <- 2.41
significant$estimate[significant$I == 'nt$20,000 - nt$49,999.junior/science and technological college'] <- 0.93
significant$estimate[significant$I == 'nt$50,000 - nt$99,999.junior/science and technological college'] <- 1.66
significant$estimate[significant$I == 'nt$100,000 - nt$149,999.junior/science and technological college'] <- 1.95
significant$estimate[significant$I == 'nt$150,000 - nt$199,999.junior/science and technological college'] <- 1.64
significant$estimate[significant$I == 'nt$200,000 or more.junior/science and technological college'] <- 1.78
significant$estimate[significant$I == 'nt$20,000 - nt$49,999.university'] <- 1.04
significant$estimate[significant$I == 'nt$50,000 - nt$99,999.university'] <- 1.02
significant$estimate[significant$I == 'nt$100,000 - nt$149,999.university'] <- 1.14
significant$estimate[significant$I == 'nt$150,000 - nt$199,999.university'] <- 1.59
significant$estimate[significant$I == 'nt$200,000 or more.university'] <- 1.06
significant$estimate[significant$I == 'nt$150,000 - nt$199,999.graduate school'] <- 0.99

for (i in 1:10973) {
  if (significant$income_num[i] == 1) {
    significant$income_num[i] = runif(1, min = -3.2, max = -2.8)
  }
  if (significant$income_num[i] == 2) {
    significant$income_num[i] = runif(1, min = -2.2, max = -1.8)
  }
  if (significant$income_num[i] == 3) {
    significant$income_num[i] = runif(1, min = -1.2, max = -0.8)
  }
  if (significant$income_num[i] == 4) {
    significant$income_num[i] = runif(1, min = 0.8, max = 1.2)
  }
  if (significant$income_num[i] == 5) {
    significant$income_num[i] = runif(1, min = 1.8, max = 2.2)
  }
  if (significant$income_num[i] == 6) {
    significant$income_num[i] = runif(1, min = 2.8, max = 3.2)
  }
}

for (i in 1:10971) {
  if (significant$edu_num[i] == 1) {
    significant$edu_num[i] = runif(1, min = -3.2, max = -2.8)
  }
  if (significant$edu_num[i] == 2) {
    significant$edu_num[i] = runif(1, min = -2.2, max = -1.8)
  }
  if (significant$edu_num[i] == 3) {
    significant$edu_num[i] = runif(1, min = -1.2, max = -0.8)
  }
  if (significant$edu_num[i] == 4) {
    significant$edu_num[i] = runif(1, min = 0.8, max = 1.2)
  }
  if (significant$edu_num[i] == 5) {
    significant$edu_num[i] = runif(1, min = 1.8, max = 2.2)
  }
  if (significant$edu_num[i] == 6) {
    significant$edu_num[i] = runif(1, min = 2.8, max = 3.2)
  }
}

plot(significant$income_num, significant$edu_num, main="logistic regression coefficients with regards to education and income", xlab = "income level", ylab = "education level")

#----------Matching------------#
#The covariates we want to match on
set.seed(1)
w1p515_num <- as.numeric(w1p515_ordered)
w1p104_num <- as.numeric(w1p104_ordered)

#w1p104_num[w1p104_num == 4 | w1p104_num == 5] <- 2
#w1p104_num[w1p104_num == 6] <- 3

X = cbind.data.frame(w1p515_num, w1p104_num) #income 

genout <- GenMatch(Tr=X1$cramschoolYN, X=X, BalanceMatrix=X, estimand="ATT", M=1, pop.size = 10,
                   max.generations=10, wait.generations=1, ties = FALSE)  #GenMatch takes in argument as vector, such as column value, but not dataframe. 

mout <- Match(Y=w1nright, Tr=X1$cramschoolYN, X=X, estimand="ATT", Weight.matrix=genout, ties = FALSE) 
summary(mout)

mb <- MatchBalance(X1$cramschoolYN ~ w1p515_num + w1p104_num, #+ rcat + rotc + rore + nboys + I(age^2) + I(srvlng^2), 
                   match.out=mout, nboots=1000)  






