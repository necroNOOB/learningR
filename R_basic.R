#library(dslabs)
data("murders")
#pop <- murders$population
#ord <- order(pop)
#which.min(ord)
#which.min(pop)

#states <- murders$state
#ranks <- rank(murders$population)
#ind <- order(ranks)
#my_df <- data.frame(states, ranks, ind)

#ind <- c(FALSE, TRUE, FALSE)
#mean(!ind)

#data("na_example")
#ind <- is.na(na_example)
#na_example[ind]


#murder_rate <- murders$total / murders$population * 100000

#abbs <- c("MA", "ME", "MI", "MO", "MU") 
#which(!abbs %in% murders$abb)

library(dplyr)

murders <- mutate(murders,rate=total/population * 100000, ranks = rank(rate))
filter(murders)


library(dslabs)
data(heights)
options(digits = 3)
heights
mean(heights$height)
ind <- heights$height > mean(heights$height)
ind2 <- heights$sex == 'Female'
ind2
sum(ind & ind2)
mean(ind2)
match(50, heights$height)
heights$sex[1032]
max(heights$height)
x <- 50:82
sum(!x %in% heights$height)

library(dplyr)
ht_cm <- heights$height * 2.54
height2 <- mutate(heights, ht_cm)
height2$ht_cm[18]
mean(height2$ht_cm)


females <- filter(height2, sex == 'Female')
count(females)
mean(females$ht_cm)


library(dslabs)
data("olive")
head(olive)
plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(olive$palmitic~olive$region, data = olive)

