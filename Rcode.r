install.packages("ggrepel")
# Importing some libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(cluster)
library(tidyr)
library(cluster)
library(waffle)

# Preprocessing the data
dat = read.csv("Glassdoor Gender Pay Gap.csv")
head(dat)
ggplot(data = dat,aes(x = JobTitle,fill = JobTitle)) + geom_bar(stat = "count",width = 0.5) + scale_fill_manual(values=c("cyan3", "steelblue", "dodgerblue","lightblue","darkturquoise","turquoise","cyan","deepskyblue","royalblue1","paleturquoise"))+geom_text(stat='count',aes(label=..count..),vjust = -1)+theme(legend.position = "none")+labs(title = "Number of entries for every Job Title",x = "Roles",y = "Number of entries")+coord_flip()
# Separate male and female
dat_male = filter(dat,Gender == 'Male')
dat_female = filter(dat,Gender == 'Female')
# Building two bar charts to see how genders vary across job titles
ggplot(data = dat_male,aes(x = JobTitle,fill = JobTitle)) + geom_bar(stat = "count",width = 0.5) + geom_text(stat='count',aes(label=..count..),vjust = -1)+theme(legend.position = "none")+scale_fill_manual(values=c("cyan3", "steelblue", "dodgerblue","lightblue","darkturquoise","turquoise","cyan","deepskyblue","royalblue1","paleturquoise"))+labs(title = "Number of entries for every Job Title in Males",x = "Roles",y = "Number of entries")+coord_flip()
ggplot(data = dat_female,aes(x = JobTitle,fill = JobTitle)) + geom_bar(stat = "count",width = 0.5) + geom_text(stat='count',aes(label=..count..),vjust = -1)+theme(legend.position = "none")+scale_fill_manual(values=c("cyan3", "steelblue", "dodgerblue","lightblue","darkturquoise","turquoise","cyan","deepskyblue","royalblue1","paleturquoise"))+labs(title = "Number of entries for every Job Title in Females",x = "Roles",y = "Number of entries")+coord_flip()

# Analyzing Education and Seniority level distribution
p = dat_male %>% count(Education)
p2 = dat_female %>% count(Education)
p = c('College' = 118,'High School' = 133,'Masters' = 149,'PhD' = 132)
p2 = c('College' = 123,'High School' = 132,'Masters' = 107,'PhD' = 106)

waffle(p/10,rows = 5,size = 2.5,title = "Education Distribution among Males",colors = c("midnightblue","royalblue1","lightblue","cyan3"),xlab = "1 Square = 10 Employees")

waffle(p2/10,rows = 5,size = 2.5,title = "Education Distribution among Females",colors = c("midnightblue","royalblue1","lightblue","cyan3"),xlab = "1 Square = 10 Employees")

p3 = dat_male %>% count(Seniority)
p4 = dat_female %>% count(Seniority)
p3 = c('Seniority 1' = 112,'Seniority 2' = 107,'Seniority 3' = 113,'Seniority 4' = 104,'Seniority 5' = 96 )
p4 = c('Seniority 1' = 83,'Seniority 2' = 102,'Seniority 3' = 106,'Seniority 4' = 80,'Seniority 5' = 97 )


waffle(p3/10,rows = 5,size = 2.5,title = "Seniority Distribution among Males",colors = c("midnightblue","royalblue1","lightblue","cyan3","deepskyblue"),xlab = "1 Square = 10 Employees")

waffle(p4/10,rows = 5,size = 2.5,title = "Seniority Distribution among Females",colors = c("midnightblue","royalblue1","lightblue","cyan3","deepskyblue"),xlab = "1 Square = 10 Employees")



# Calculate mean
mean_m = mean(dat_male$BasePay)
mean_f = mean(dat_female$BasePay)
df_mean = data.frame(Mean = c(mean_m,mean_f))
ggplot(data = df_mean,aes(x = c("Male","Female"),y = Mean,fill = c("Male","Female"))) + geom_bar(stat = "identity",color = "black",width = 0.5)+scale_fill_manual(values = c("#56B4E9","#E69F00")) + geom_text(aes(label = round(Mean)),vjust = 2,size = 5) + labs(title = "Average Base Pay for Females vs Males (in US$)",x = "Gender",fill = "Gender", y = "Mean (US$)")

# Add a new column to add Salary plus Bonus
dat$TotalPay = dat$BasePay + dat$Bonus
# Separate male and female
dat_male2 = filter(dat,Gender == 'Male')
dat_female2 = filter(dat,Gender == 'Female')
# Calculate mean
mean_m2 = mean(dat_male2$TotalPay)
mean_f2 = mean(dat_female2$TotalPay)
df_mean2 = data.frame(Mean = c(mean_m2,mean_f2))
ggplot(data = df_mean2,aes(x = c("Male","Female"),y = Mean,fill = c("Male","Female"))) + geom_bar(stat = "identity",color = "black",width = 0.5)+scale_fill_manual(values = c("#56B4E9","#E69F00")) + geom_text(aes(label = round(Mean)),vjust = 2,size = 5) + labs(title = "Average Total Compensation for Females vs Males (in US$)",x = "Gender",fill = "Gender", y = "Mean (US$)")

library(scales)
ggplot(data = dat,aes(x = JobTitle, y = TotalPay,fill = Gender)) + geom_bar(stat = "identity")+coord_flip()+scale_fill_manual(values = c("#56B4E9","#E69F00"))+scale_y_continuous(labels = label_number(suffix = " K",scale = 1e-5))+labs(y = "Total Pay (in US$)",x = "Job Title",title = "Total Pay Gap by Job Title")

# Calculating Mean Difference in pay for Job Title
df0 = aggregate(TotalPay ~ JobTitle,dat_male2,mean)
df1 = aggregate(TotalPay ~ JobTitle,dat_female2,mean)
df2 = merge(df0,df1,by = c("JobTitle"))
df3 = aggregate(TotalPay ~ JobTitle,dat_male2,mean)
df2$Difference = df2$TotalPay.x - df2$TotalPay.y
df2
ggplot(data = df2,aes(x = JobTitle, y = Difference)) + geom_bar(stat = "identity",fill = "steelblue")+scale_y_continuous(labels = label_number(suffix = " K",scale = 1e-3))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))+labs(y = "Difference in Pay (US$)",x = "Job Title",title = "Total Pay Gap by Job Title")


library(ggrepel)
ggplot(df2,aes(x = TotalPay.x,y = TotalPay.y)) + geom_point(color = "red")+labs(x = "Total Pay for Males (US$)",y = "Total Pay for Females (US$)",title = "Relationship between Male and Female Total Pay")+geom_label_repel(aes(label = df2$JobTitle,fill = df2$JobTitle), color = 'white',size = 3.5) +theme(legend.position = "none")+ geom_smooth(method="lm", se=FALSE, fullrange=FALSE)+ geom_smooth(se=FALSE,color="black")
summary(lm(data = df2, TotalPay.y ~ TotalPay.x))


