install.packages("dslabs")
library(dslabs)
data(package = .packages(all.available = TRUE))
data(movielens)
str(movielens)

class(movielens$title)

class(movielens$genres)

levels(movielens$genres)

nlevels(movielens$genres)

seq<-c(2,5,8,NA)
ind<-is.na(seq)
sum(!ind)
sum(seq[!ind])/sum(!ind)


n<-100
x<- seq(1,n)
sum(x)
sum_100 <- n*(n+1)/2

class(log)

library(dslabs)
data(murders)
str(murders)
murders$state[which.max(murders$population)]
murders$state[1:3]
levels(murders$region)

murder_rate<- (murders$total/murders$population)*100000
sort(murder_rate, decreasing = TRUE)
rank(murder_rate)
murders$state[rank(murder_rate)]
murders$state[sort(murder_rate)]
order(murder_rate)
murders$state[order(murder_rate, decreasing = TRUE)]

murders[2:3,]
x<-(1:100)

y<-x^2

z<-1/y
z

sum(z)
mean(z)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
mydf<-data.frame(Name =name , D = distance, t = time/60, speed=distance/time*60)
mydf
mydf$Name[which.max(mydf$speed)]
str(mydf)    

install.packages("dplyr")
library(dplyr)

m<-25
sum_n <- vector(length = m)
for (n in 1:m) {
  sum_n[n] <- mean(1:n)
}
sum_n

#New York GPS
nyc_latitude = 40.7128
nyc_longitude = -74.0060

#Create a map
m <- leaflet %>% setView(lng = nyc_longitude, 
                           lat = nyc_latitude, 
                           zoom = 12)
m %>% addTiles()

library(dslabs)
data("heights")
options(digits = 3)

mean(heights$height)
ind<-(heights$height>mean(heights$height))
sum(ind)
filter(heights, sex %in% c("Female")& height > mean(heights$height))
mean(heights$sex=="Female")
min(heights$height)
match(min(heights$height), heights$height)
which.min(heights$height)
heights$sex[which.min(heights$height)]
heights$sex[1032]
max(heights$height)
x<-max(heights$height):min(heights$height)
x<-50:82
x
y<-x %in% heights$height
y
sum(!y)

heights2<- mutate(heights, ht_cm = heights$height*2.54)
heights2$ht_cm[18]
mean(heights2$ht_cm)
females<- filter(heights2,sex %in% c("Female"))
mean(females$ht_cm)


avg <- function(n, arithmetic = TRUE){
  x<-1:n
    ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
avg(5,FALSE)

total<- function(n){
  x<-1:n
  sum(x)
}

data(murders)
new_name<-ifelse(nchar(murders$state)<9, murders$state, murders$abb)
new_name

a <- seq(min(murders$total), max(murders$total), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(murders$total <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

library(dslabs)
data(heights)
y <- heights$height[heights$sex == "Male"]
mean(y > 69 & y <= 80)

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dslabs)
data("murders")
p <- murders %>% ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty=2) +
  geom_point(aes(color = region), size=3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total Murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
p

r <- murders %>% summarise(rate = sum(total)/sum(population)*10^6) %>% pull(rate)

library(dslabs)
data("gapminder")
head((gapminder))

gapminder_Africa_2010 <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>% filter(continent =="Africa" & year == 2010 & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  geom_text()
facet_grid(year~.)

options(digits = 3)    # report 3 significant digits
install.packages("titanic")
library(tidyverse)
library(titanic)
data("titanic_train")
str(titanic_train)

hist(titanic_train$Pclass)
table1<- titanic_train %>% filter(Survived==1)
hist(table1$Pclass)

table2 <- titanic_train %>% filter(Sex =="female" & Survived==1)
hist(table2$Age)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

z<- seq(1,10)/2
prod(z)

beads<- rep(c("red","blue"), times = c(2,3))
beads
B<- 10000
events<- sample(beads,B, replace = TRUE)
table(events)
prop.table(table(events))


suits<- c("hearts","clubs","diamonds","spades")
numbers<- c("ace", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "jack", "queen", "king")
deck<- expand.grid(number = numbers, suit = suits)
deck<- paste(deck$number,deck$suit)

kings<- paste("king", suits)
mean(deck %in% kings)

install.packages("gtools")
library(gtools)

hands<- combinations(52,2,v = deck)
first_hand<- hands[,1]
second_hand<- hands[,2]

sum(first_hand %in% kings & second_hand %in% kings)
sum(first_hand %in% kings & second_hand %in% kings)/ sum(first_hand %in% kings)

hand<- sample(deck,2)
hand

B<-10000
set.seed(1)
runners<- c("Jamaica","Jamaica","Jamaica","USA","Ecuador","Netherlands","France","S_Africa")
result<- replicate(B,
{
 simulated<- sample(runners, 3, replace = FALSE)
 simulated[1]=="Jamaica" & simulated[2]=="Jamaica" & simulated[3]=="Jamaica"
})
mean(result)

sides<- seq(2,12,1)
meal_options<- function(n){
  meal<- 6*3*n*(n-1)/2
}
sapply(sides, meal_options)

x<- seq(-4,4, length=100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()

set.seed(16, sample.kind = "Rounding")
act_scores<- rnorm(10000, 20.9,5.7)

z_scores<- (act_scores - mean(act_scores))/sd(act_scores)
1 - pnorm(2, mean(z_scores), sd(z_scores))
qnorm(0.95, mean(act_scores), sd(act_scores))

cdf_act<- sapply(1:36, function(x){
  mean(act_scores <=x)
})

p<- seq(0.01, 0.99, 0.01)
sample_quantiles<- quantile(act_scores, p)
head(sample_quantiles)
plot(p,sample_quantiles)

names(sample_quantiles[max(which(sample_quantiles<26))])
theoretical_quantiles<- qnorm(p,mean(act_scores), sd(act_scores))
plot(theoretical_quantiles, sample_quantiles)

install.packages("caret")
library(caret)

n<- 1000
B<- 10000
S<- replicate(B,{
  X<- sample(c(-1,1),n, replace = TRUE, prob = c(1/2, 1/2))
  sum(X)
})
mean(S<0)

set.seed(21)
B<-10000
X<- replicate(B,{
  sum(sample(c(1,-0.25),44,replace = TRUE, prob=c(0.2,0.8)))
})
mean(X>8)

p<- seq(0.25,0.95,0.05)
exp_val<- sapply(p,function(x){
  mu<- 44*1*x
  sig<- sqrt(44)*1*sqrt(x*(1-x))
  1-pnorm(35,mu,sig)
})
p[which(exp_val>0.7)]

