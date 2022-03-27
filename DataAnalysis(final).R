# Read the Global Terrorism Database 
getwd()
setwd("~/R/Thesis")
library(readr)
gtd <- read_csv("globalterrorismdb_0221dist.csv") # GTD file. You can get it from https://www.start.umd.edu/gtd/
library(dplyr)
summary(gtd)

# Select the data since 2015
data <- gtd %>% filter(iyear >= 2015) %>% select(eventid, iyear, extended, resolution, country, region, 
                                                 latitude, longitude, crit1, crit2, crit3, doubtterr, alternative,
                                                 multiple, success, attacktype1, weaptype1, targtype1, gname, nperps,
                                                 claimed, nkill, ishostkid, nhostkid, nhours, ndays, divert, ransom,
                                                 hostkidoutcome, INT_LOG, nreleased, kidhijcountry, nperps, suicide, nkillter)

library(ggplot2)
library(lubridate)

# Number of terror cases by attack types (2015 ~ 2019)
atk <- data %>% select(attacktype1) %>% count(attacktype1)
atk$attacktype1 <- c("Assassination", "Armed assault", "Bombing", "Hijacking", "Hostage taking(baricade)",
                     "Hostage taking(kinap)", "Facility attack", "Unarmed assault", "Unkown")
atk %>% arrange(desc(n))

# Making the pie chart for weapon type in hourly terror cases 
hour <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, nhours, weaptype1, gname, nkill, nperps, suicide) %>% filter(nhours > 0)
table(hour$weaptype1) # weapon type in hour

weapontype_hour <- c("Firearms", "Explosive", "Incendiary",
                     "Melee", "Other", "Unknown")
weacount_hour <- c(113,19,4,17,1,31)
wea_hour <- data.frame(weapontype_hour, weacount_hour)
total_hour <- sum(wea_hour[,2])
wea_hour <- wea_hour %>% mutate(prop=round(weacount_hour/total_hour, 2))
ggplot(wea_hour, aes(x = "", y=weacount_hour, fill = weapontype_hour)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual(values = c("red", "green", "purple", "grey", "orange", "coral"))+
  geom_text(aes(label = paste(prop*100, "%")), position=position_stack(vjust=0.5), size=3.5) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Weapon type in hourly terror") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# Making the pie chart for weapon type in day terror cases
weapontype_day <- c("Firearms", "Explosive", "Fake Weapons","Incendiary",
                    "Melee", "Other", "Unknown")
weacount_day <- c(740, 14, 1, 11, 63, 3, 422)
wea_day <- data.frame(weapontype_day, weacount_day)
total_day <- sum(wea_day[,2])
wea_day <- wea_day %>% mutate(prop=round(weacount_day/total_day, 2))

ggplot(wea_day, aes(x = "", y=weacount_day, fill = weapontype_day)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start=0) + 
  scale_fill_manual(values = c("red", "yellow", "green", "purple", "grey", "orange", "coral"))+
  geom_text(aes(label = paste(prop*100, "%")), position=position_stack(vjust=0.5), size=3.5) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Weapon type in daily terror") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# Hostage taking in each year(less than 24hours / more than 24hours) * check the number of cases
nhours2015 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, nhours, suicide) %>% filter(iyear == 2015, nhours > 0)
nhours2016 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, nhours, suicide) %>% filter(iyear == 2016, nhours > 0)
nhours2017 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, nhours, suicide) %>% filter(iyear == 2017, nhours > 0)
nhours2018 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, nhours, suicide) %>% filter(iyear == 2018, nhours > 0)
nhours2019 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, nhours, suicide) %>% filter(iyear == 2019, nhours > 0)
ndays2015 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, ndays, suicide) %>% filter(iyear == 2015, ndays > 0)
ndays2016 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, ndays, suicide) %>% filter(iyear == 2016, ndays > 0)
ndays2017 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, ndays, suicide) %>% filter(iyear == 2017, ndays > 0)
ndays2018 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, ndays, suicide) %>% filter(iyear == 2018, ndays > 0)
ndays2019 <- data %>% filter(attacktype1 <= 6 & attacktype1 >= 4) %>% select(iyear, ndays, suicide) %>% filter(iyear == 2019, ndays > 0)


# Suicide cases in hostage taking incidents * index(0: not suicide, 1: suicide)
table(nhours2015$suicide)
table(nhours2016$suicide)
table(nhours2017$suicide)
table(nhours2018$suicide)
table(nhours2019$suicide)
table(ndays2015$suicide)
table(ndays2016$suicide)
table(ndays2017$suicide)
table(ndays2018$suicide)
table(ndays2019$suicide)



# Kolmogorov-Smirnov test
# Taliban occurrence rate 2015 ~ 2019
library(dgof)
gap <- c()
terror_arrival <- data %>% filter(iyear >= 2015) %>% filter(gname == "Taliban") %>% filter(doubtterr == 0) %>% 
  filter(attacktype1 <= 6 & attacktype1 >=4)
terror_arrival$eventid <- substr(terror_arrival$eventid,1,8)

# check the event (one more hostage takings occurred in one day)
eventid_multiple <- terror_arrival %>% filter(multiple==1) %>% select(eventid)
length(table(eventid_multiple))

# check the event (one hostage taking occurred in one day)
terror_arrival_single <- terror_arrival %>% filter(multiple == 0) 
terror_arrival_single <- as.Date(as.character(terror_arrival_single$eventid), format="%Y%m%d")
for (i in 1:length(terror_arrival_single)) {
  gap[i] <- terror_arrival_single[i] - terror_arrival_single[i-1]
}
# delete first number because we don't consider 2014 last case
gap <- gap[-1]
gap[469:(469+34)] <- 0

# Histogram for hostage taking inter-arrival time
gap <- as.data.frame(gap)
ggplot(gap, aes(x = gap)) +
  geom_bar(width = 0.9) +
  labs(x = "Occurrence Interval (day)", y = "Frequecy",
       title = "Distribution of terror occurrence (Taliban)")

# Generate random variable following exponential distribution with estimated parameter lambda
lamda.terror <- 1/mean(gap$gap, na.rm = TRUE) # 0.2763
set.seed(123)
terror.rv <- round(rexp(503, lamda.terror))
hist(terror.rv, breaks = 10)
ks.test(gap, terror.rv)  # p-value: 0.09722

plot(ecdf(gap$gap), xlim=range(c(0,32)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror occurrence")
plot(ecdf(terror.rv), add=TRUE, lty="dashed", col="red")
legend(x=19, y=0.4, legend=c("Epirical", "Random variable"),
       col = c("blue", "red"), lty = 1)

# Successful operation rate test(service distribution)
service <- c()
operation <- data %>% filter(iyear >= 2015) %>% filter(doubtterr == 0) %>% 
  filter(attacktype1 <= 6 & attacktype1 >=4) %>% filter(hostkidoutcome == 5) %>% filter(nhours >=0 | ndays >= 0)
operation$ndays[is.na(operation$ndays)] <- 0 # make the hours variable as 0 days
operation <- operation %>% filter(ndays >= 0)
service <- operation$ndays
table(service)
service <- as.data.frame(service)
ggplot(service, aes(x = service)) +
  geom_bar(width = 0.9) +
  labs(x = "Terror Duration (day)", y = "Frequecy",
       title = "Distribution of hostage taking duration (successful operation)")

# Komogolov-Smirnov test
lamda.service <- 1/mean(service$service, na.rm = TRUE) 

set.seed(123)
service.rv <- round(rexp(138, lamda.service))
ks.test(service, service.rv)

min(boxplot(service)$out)
service1 <- service[service < 48]
boxplot(service1)

lamda.service <- 1/mean(service1, na.rm = TRUE)
set.seed(123)
service.rv <- round(rexp(119, lamda.service))
ks.test(service1, service.rv)

min(boxplot(service1)$out)
service2 <- service1[service1 < 19]
boxplot(service2)

lamda.service <- 1/mean(service2, na.rm = TRUE) # 0.2829
set.seed(123)
service.rv <- round(rexp(103, lamda.service))
ks.test(service2, service.rv) # p-value: 0.2248

plot(ecdf(service2), xlim=range(c(service2,service.rv)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror end with successful operation")
plot(ecdf(service.rv), add=TRUE, lty="dashed", col="red")
legend(x=15, y=0.4, legend=c("Empirical", "Random variable"),
       col = c("blue", "red"), lty = 1)

# Killing distribution(killing rate) Day
terror_kill <- data %>% filter(iyear >= 2015) %>% filter(doubtterr == 0) %>% filter(gname == "Taliban") %>%
  filter(attacktype1 <= 6 & attacktype1 >=4) %>% filter(hostkidoutcome == 4) %>% filter(nhours >=0 | ndays>=0)
terror_kill$ndays[is.na(terror_kill$ndays)] <- 0 # make the hours variable as 0 days after get rid of -99 and NA
table(terror_kill$ndays) # A lot of day variables are unknown value. Thus, we need to deal this variable.
killing <- terror_kill$ndays
table(killing)
killing <- as.data.frame(killing)
ggplot(killing, aes(x = killing)) +
  geom_bar(width = 0.9) +
  labs(x = "Terror Duration (day)", y = "Frequecy",
       title = "Distribution of hostage taking duration if hostages were killed")


# Komogolov-Smirnov test
lamda.killing <- 1/mean(killing$killing, na.rm = TRUE) # 0.1718
set.seed(123)
killing.rv <- round(rexp(39, lamda.killing))
ks.test(killing, killing.rv) # p-value: 0.1538

plot(ecdf(killing$killing), xlim=range(c(killing$killing,killing.rv)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror end with hotages killed by terrorists")
plot(ecdf(service.rv), add=TRUE, lty="dashed", col="red")
legend(x=23, y=0.4, legend=c("Empirical", "Random variable"),
       col = c("blue", "red"), lty = 1)



# Appendix
# Afghanistan occurrence rate 2015 ~ 2019
gap <- c()
terror_arrival <- data %>% filter(iyear >= 2015) %>% filter(country == 4) %>% filter(doubtterr == 0) %>% 
  filter(attacktype1 <= 6 & attacktype1 >=4)
terror_arrival$eventid <- substr(terror_arrival$eventid,1,8)
eventid_multiple <- terror_arrival %>% filter(multiple==1) %>% select(eventid)
length(table(eventid_multiple))
terror_arrival_single <- terror_arrival %>% filter(multiple == 0) 
terror_arrival_single <- as.Date(as.character(terror_arrival_single$eventid), format="%Y%m%d")
for (i in 1:length(terror_arrival_single)) {
  gap[i] <- terror_arrival_single[i] - terror_arrival_single[i-1]
}
gap[608:(608+37)] <- 0

table(gap)
gap <- as.data.frame(gap)
ggplot(gap, aes(x = gap)) +
  geom_bar(width = 0.9) +
  labs(x = "Occurrence Interval", y = "Frequecy",
       title = "Distribution of terror occurrence (Afghanistan)")

# Komogolov-Smirnov test
lamda.terror <- 1/mean(gap$gap, na.rm = TRUE) # 0.2763
set.seed(123)
terror.rv <- round(rexp(645, lamda.terror))
hist(terror.rv, breaks = 10)
ks.test(gap, terror.rv)  # p-value: 0.3247

plot(ecdf(gap$gap), xlim=range(c(0,32)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror occurrence")
plot(ecdf(terror.rv), add=TRUE, lty="dashed", col="red")
legend(x=19, y=0.4, legend=c("Epirical", "Random variable"),
       col = c("blue", "red"), lty = 1)


# Iraq occurrence rate 2015 ~ 2019
gap <- c()
terror_arrival <- data %>% filter(iyear >= 2015) %>% filter(country == 95) %>% filter(doubtterr == 0) %>% 
  filter(attacktype1 <= 6 & attacktype1 >=4)
terror_arrival$eventid <- substr(terror_arrival$eventid,1,8)
eventid_multiple <- terror_arrival %>% filter(multiple==1) %>% select(eventid)
length(table(eventid_multiple))
terror_arrival_single <- terror_arrival %>% filter(multiple == 0) 
terror_arrival_single <- as.Date(as.character(terror_arrival_single$eventid), format="%Y%m%d")
for (i in 1:length(terror_arrival_single)) {
  gap[i] <- terror_arrival_single[i] - terror_arrival_single[i-1]
}
length(gap)
gap[length(gap):(length(gap)+32)] <- 0

table(gap)
gap <- as.data.frame(gap)
ggplot(gap, aes(x = gap)) +
  geom_bar(width = 0.9) +
  labs(x = "Occurrence Interval", y = "Frequecy",
       title = "Distribution of terror occurrence (Iraq)")

# Komogolov-Smirnov test
lamda.terror <- 1/mean(gap$gap, na.rm = TRUE) # 0.2153
set.seed(123)
terror.rv <- round(rexp(390, round(lamda.terror, 2)))
ks.test(gap, terror.rv)  # p-value: 0.05465

plot(ecdf(gap$gap), xlim=range(c(0,32)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror occurrence")
plot(ecdf(terror.rv), add=TRUE, lty="dashed", col="red")
legend(x=19, y=0.4, legend=c("Epirical", "Random variable"),
       col = c("blue", "red"), lty = 1)


# India occurrence rate 2015 ~ 2019
gap <- c()
terror_arrival <- data %>% filter(iyear >= 2015) %>% filter(country == 92) %>% filter(doubtterr == 0) %>% 
  filter(attacktype1 <= 6 & attacktype1 >=4)
terror_arrival$eventid <- substr(terror_arrival$eventid,1,8)
eventid_multiple <- terror_arrival %>% filter(multiple==1) %>% select(eventid)
length(table(eventid_multiple))
terror_arrival_single <- terror_arrival %>% filter(multiple == 0) 
terror_arrival_single <- as.Date(as.character(terror_arrival_single$eventid), format="%Y%m%d")
for (i in 1:length(terror_arrival_single)) {
  gap[i] <- terror_arrival_single[i] - terror_arrival_single[i-1]
}
gap[581:(581+45)] <- 0

table(gap)
gap <- as.data.frame(gap)
ggplot(gap, aes(x = gap)) +
  geom_bar(width = 0.9) +
  labs(x = "Occurrence Interval", y = "Frequecy",
       title = "Distribution of terror occurrence (India)")

# Komogolov-Smirnov test
lamda.terror <- 1/mean(gap$gap, na.rm = TRUE) # 0.3443
set.seed(123)
terror.rv <- round(rexp(626, 0.37))
ks.test(gap, terror.rv)  # p-value: 0.06585

plot(ecdf(gap$gap), xlim=range(c(0,32)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror occurrence")
plot(ecdf(terror.rv), add=TRUE, lty="dashed", col="red")
legend(x=19, y=0.4, legend=c("Epirical", "Random variable"),
       col = c("blue", "red"), lty = 1)

# Killing distribution(killing rate) Afghanistan
terror_kill <- data %>% filter(iyear >= 2015) %>% filter(doubtterr == 0) %>% filter(country == 4 ) %>%
  filter(attacktype1 <= 6 & attacktype1 >=4) %>% filter(hostkidoutcome == 4) %>% filter(nhours >=0 | ndays>=0)
terror_kill$ndays[is.na(terror_kill$ndays)] <- 0 # make the hours variable as 0 days after get rid of -99 and NA
table(terror_kill$ndays) # A lot of day variables are unknown value. Thus, we need to deal this variable.
killing <- terror_kill$ndays
table(killing)
killing <- as.data.frame(killing)
ggplot(killing, aes(x = killing)) +
  geom_bar(width = 0.9) +
  labs(x = "Terror Duration", y = "Frequecy",
       title = "Distribution of Terror Ending before Operation")

# Komogolov-Smirnov test
lamda.killing <- round(1/mean(killing$killing, na.rm = TRUE), 4) # 0.1531
set.seed(123)
killing.rv <- round(rexp(100, 0.22))
ks.test(killing, killing.rv) # p-value: 0.0884

plot(ecdf(killing$killing), xlim=range(c(killing$killing,killing.rv)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror end with hotages killed by terrorists")
plot(ecdf(killing.rv), add=TRUE, lty="dashed", col="red")
legend(x=23, y=0.4, legend=c("Empirical", "Random variable"),
       col = c("blue", "red"), lty = 1)

# Killing distribution(killing rate) Iraq
terror_kill <- data %>% filter(iyear >= 2015) %>% filter(doubtterr == 0) %>% filter(country == 95) %>%
  filter(attacktype1 <= 6 & attacktype1 >=4) %>% filter(hostkidoutcome == 4) %>% filter(nhours >=0 | ndays>=0)
terror_kill$ndays[is.na(terror_kill$ndays)] <- 0 # make the hours variable as 0 days after get rid of -99 and NA
table(terror_kill$ndays) # A lot of day variables are unknown value. Thus, we need to deal this variable.
killing <- terror_kill$ndays
table(killing)
killing <- as.data.frame(killing)
ggplot(killing, aes(x = killing)) +
  geom_bar(width = 0.9) +
  labs(x = "Terror Duration", y = "Frequecy",
       title = "Distribution of Terror Ending before Operation")

# Komogolov-Smirnov test
lamda.killing <- round(1/mean(killing$killing, na.rm = TRUE), 4) # 0.2952
set.seed(123)
killing.rv <- round(rexp(100, 0.3))
ks.test(killing, killing.rv) # p-value: 0.2175

plot(ecdf(killing$killing), xlim=range(c(killing$killing,killing.rv)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror end with hotages killed by terrorists")
plot(ecdf(killing.rv), add=TRUE, lty="dashed", col="red")
legend(x=23, y=0.4, legend=c("Empirical", "Random variable"),
       col = c("blue", "red"), lty = 1)

# Killing distribution(killing rate) India
terror_kill <- data %>% filter(iyear >= 2015) %>% filter(doubtterr == 0) %>% filter(country == 92) %>%
  filter(attacktype1 <= 6 & attacktype1 >=4) %>% filter(hostkidoutcome == 4) %>% filter(nhours >=0 | ndays>=0)
terror_kill$ndays[is.na(terror_kill$ndays)] <- 0 # make the hours variable as 0 days after get rid of -99 and NA
table(terror_kill$ndays) # A lot of day variables are unknown value. Thus, we need to deal this variable.
killing <- terror_kill$ndays
table(killing)
killing <- as.data.frame(killing)
ggplot(killing, aes(x = killing)) +
  geom_bar(width = 0.9) +
  labs(x = "Terror Duration", y = "Frequecy",
       title = "Distribution of Terror Ending before Operation")

# Komogolov-Smirnov test
lamda.killing <- round(1/mean(killing$killing, na.rm = TRUE), 4) # 0.2305
set.seed(123)
killing.rv <- round(rexp(65, 0.23))
ks.test(killing, killing.rv) # p-value: 0.1538

plot(ecdf(killing$killing), xlim=range(c(killing$killing,killing.rv)), col="blue",
     xlab = "Time(day)", ylab = "Fn(day)", main = "ECDF of Terror end with hotages killed by terrorists")
plot(ecdf(killing.rv), add=TRUE, lty="dashed", col="red")
legend(x=23, y=0.4, legend=c("Empirical", "Random variable"),
       col = c("blue", "red"), lty = 1)


appendix <- data %>% filter(iyear >= 2015) %>% filter(doubtterr == 0) %>% filter(country == 92) %>%
  filter(attacktype1 <= 6 & attacktype1 >=4)  %>% 
  select(iyear, country, attacktype1, weaptype1, gname, nkill, nhours, ndays, hostkidoutcome, suicide)

write.csv(appendix, file='/Users/gooyh/Documents/R/Thesis/Appendix/India.csv')
