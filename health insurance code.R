# read csv
df = read.csv('C:/Users/mcone/OneDrive/Desktop/Work/R/Medical costs personal dataset/insurance.csv' , header = TRUE)

# find all numeric cols
num_cols <- unlist(lapply(df , is.numeric))

# plot numeric cols against eachother
plot(df[num_cols])

# correlations
round(cor(df[,num_cols]),2)


smoker = as.factor(df$smoker)
sex = as.factor(df$sex)
region = as.factor(df$region)
bmi = as.factor(df$bmi)
age = as.factor(df$age)

#boxplots
boxplot(df$charges ~ smoker, main = 'smoker')
boxplot(df$charges ~ sex, main = 'sex')
boxplot(df$charges ~ region, main = 'region')
boxplot(df$charges ~ bmi, main = 'bmi')
boxplot(df$charges ~ age, main = 'age')

#linear regression to predict charges
model1 = lm(charges ~ smoker , data = df)
summary(model1)

model2 = lm(charges ~ smoker + bmi + age , data = df)
summary(model2)