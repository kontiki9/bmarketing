#Transform
#During data transformation step we might need to
#transform numeric variables using the log as required.
#transform factors into numeric variables (and vice versa) as necessary.

library(tidyverse)

#################Loading data into the environment#################
#bmarketing <- read.csv2("bmarketing.csv")#, sep = ".")
#?read.csv2
bmarketing <- read.csv2("bmarketing.csv", sep = ";", dec = ",")
View(bmarketing)
str(bmarketing)

colnames(bmarketing)

#fac <- c("emp.var.rate", "cons.price.idx", "cons.conf.idx", "nr.employed")

#i <- 1
#for (i in length(fac)) {
#  class(bmarketing$fac[i])
  #numeric(bmarketing$euribor3m)
#  bmarketing$eur_num <- as.numeric(levels(bmarketing$euribor3m))[bmarketing$euribor3m]
#  class(bmarketing$eur_num)
#}

#factors -> numeric
class(bmarketing$euribor3m)
bmarketing$euribor3m <- as.numeric(levels(bmarketing$euribor3m))[bmarketing$euribor3m]
class(bmarketing$euribor3m)

class(bmarketing$emp.var.rate)
bmarketing$emp.var.rate <- as.numeric(levels(bmarketing$emp.var.rate))[bmarketing$emp.var.rate]
class(bmarketing$emp.var.rate)


class(bmarketing$cons.price.idx)
bmarketing$cons.price.idx <- as.numeric(levels(bmarketing$cons.price.idx))[bmarketing$cons.price.idx]
class(bmarketing$cons.price.idx)

class(bmarketing$cons.conf.idx)
bmarketing$cons.conf.idx <- as.numeric(levels(bmarketing$cons.conf.idx))[bmarketing$cons.conf.idx]
class(bmarketing$cons.conf.idx)

class(bmarketing$nr.employed)
bmarketing$nr.employed <- as.numeric(levels(bmarketing$nr.employed))[bmarketing$nr.employed]
class(bmarketing$nr.employed)


#log transform
#euribor - no sense
par("mar") 
par(mar=c(1,1,1,1))
plot(bmarketing$euribor3m)

hist(bmarketing$euribor3m)

m <- mean(bmarketing$euribor3m)
sd <- sd(bmarketing$euribor3m)

eur_st <- (bmarketing$euribor3m -m) / sd
plot(bmarketing$eur_st)

eur_st_log <- log(eur_st)

str(eur_st)



#log transform
#age
par("mar") 
par(mar=c(1,1,1,1))




sapply(bmarketing, class)

numcols <- sapply(bmarketing, class) %in% c("integer", "numeric")
bmarketing_numcols <- bmarketing[, numcol]

numcols

head(bmarketing_numcols)

gather(bmarketing_numcols) %>% 
  ggplot(aes(value)) +
  geom_histogram()+
  facet_wrap(~key, scales = "free") +
#  geom_vline(aes(xintercept = median(value)), color = "red")+
  geom_density()
              
#function - user can specify which columns to take the log
log_col <- function(df,colnames  ) {
  for (col in colames){
    df[[col]] <- log(df[[col]])
  }
  
}
#for trees does not matter to have unscaled data

#duration campaign try log transform
bmarketing_numcols


#m <- mean(bmarketing$age)
#sd <- sd(bmarketing$age)

#age_st <- (bmarketing$age -m) / sd
#plot(bmarketing$eur_st)

age_st_log <- log(age_st)

age_log <- log(bmarketing$age)
plot(age_log)

str(eur_st)


#bmarketing %>% 
#  ggplot()+
#  aes(x= )
  


#Lets look at dataset and generate initial understanding about the column types
str(bmarketing)
summary(bmarketing)

# Let's find the range of individual variables
summary(bmarketing)

## ------------------------------------------------------------------------
bmarketing %>% 
  ggplot() + geom_histogram(aes(age), bins = 30) + 
  geom_vline(aes(xintercept= median(age)), color = "red")


bmarketing %>% 
  ggplot() + geom_histogram(aes(age_log), bins = 30) + 
  geom_vline(aes(xintercept= median(age_log)), color = "red")


bmarketing %>% 
  ggplot() + geom_histogram(aes(age), bins = 30) + 
  geom_vline(aes(xintercept= median(age)), color = "red")

# TODO: do boxplots for each data
# boxplot(duration~y,data=bmarketing_sub,col="red")
boxplot(duration~y,data=bmarketing,col="red")
