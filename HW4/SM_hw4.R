#install.packages("readxl")
library(readxl)

dat1 = read_excel("C:/Users/RE612/Desktop/stat/hw4/Homework4_data.xlsx" ,
                  sheet = "Question 1")
dat2 = read_excel("C:/Users/RE612/Desktop/stat/hw4/Homework4_data.xlsx" ,
                  sheet = "Question 2")
dat3 = read_excel("C:/Users/RE612/Desktop/stat/hw4/Homework4_data.xlsx" ,
                  sheet = "Question 3")

### 1 ###
summary(dat1)
str(dat1)
boxplot(dat1$`Class A`, dat1$`Class B`,
        names = c("A","B"), ylab = "score", main = "Boxplot")

# Normality assumption
shapiro.test(dat1$`Class A`)
shapiro.test(dat1$`Class B`)

# homoscedasticity assumption
#var.test(dat1$`Class A`, dat1$`Class B`, alternative = "two.sided")

# Welch's t test
t.test(dat1$`Class A`, dat1$`Class B`, alternative = "two.sided")


### 2 ###
summary(dat2)
str(dat2)
T1 = dat2[dat2$`Type of grass`=='T1',]$Scores
T2 = dat2[dat2$`Type of grass`=='T2',]$Scores
T3 = dat2[dat2$`Type of grass`=='T3',]$Scores
T4 = dat2[dat2$`Type of grass`=='T4',]$Scores
boxplot(T1,T2,T3,T4, names = c('T1','T2','T3','T4'), ylab = 'score', main = 'Boxplot')

# homoscedasticity assumption
library(car)
leveneTest(`Scores` ~ factor(`Type of grass`) , data = dat2)

# Friedman test
friedman.test(y=dat2$Scores, groups=dat2$`Type of grass`, blocks=dat2$Judge)


### 3 ###
summary(dat3)
str(dat3)

df = matrix(c(12,3,5,30), nrow = 2, byrow = TRUE, dimnames = list(c('A','B'),c('A','B')))
mcnemar.test(df)



