## Chapter 5: Data Transformation 

library(nycflights13)
library(tidyverse)
library(Lahman)
setwd("C:/Users/Kim F/Desktop/Semester 1/R&SQL_for_Business_Analytics/3. Zusammenfassungen & Guidelines/Ch._5_Data Transformation")

# data frame that will be mainly used is flights 
# contains 336,776 flights departured from NYC in 2013
# the data comes from Bureau of transportation statistics

# 5.2 Filter rows with filter() (p. 66 ff)
data=flights
first_filter <- filter(flights, month==11 | month==12)
alternative <- filter(flights, month %in% c(11,12))
second_filter <- filter(flights, xor(month==11, month==12))
third_filter <- filter(flights, month==11)
(test_filter <- filter(flights, month == 11 |12))
filter(flights, month %in% c(11,12))

near(sqrt(2) ^ 2, 2)

# de morgansche Regeln vs. relationale Operatoren 
de_morgansche_reglen <- filter(flights, !(arr_delay>=120 | dep_delay>= 120))
relationale_Operatoren <- filter(flights, arr_delay<120 & dep_delay<120)

# filter out NA data
filter(flights, !is.na(arr_delay))

## p. 71, Exercise 5.2.4
# 1. Find all flights that
# 1.1 had an arrival delay of 2 or more hours

filter(flights, arr_delay>=120)

# 1.2 Flew to Houston (IAH or HOU)
filter(flights, dest=="IAH" | dest == "HOU")

# alternative solution: use %in% shortcut instead of "or" expression
filter(flights, dest %in% c("IAH", "HOU"))

#1.3 Were operated by United, American, or Delta

#view airlines dataset first to check the abbreviations for the respective carriers
filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL")

# alternative solution: use %in% shortcut instead of "or" expression
filter(flights, carrier %in% c("UA","AA","DL"))

#1.4 Departed in summer (Juli, August or September)
a1 <- filter(flights, month == 7 | month == 8 |month == 9)
a2 <- filter(flights, month >= 7 & month <= 9)
a3 <- filter(flights, month %in% c(7:9))
a4 <- filter(flights, between(month, 7, 9))

#1.5 Arrived more than 2 hours late, but didn't leave late 
filter(flights, arr_delay > 120 & dep_delay <= 0)

#1.6 were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60 & dep_delay-arr_delay > 30)

#1.7 Departed between midnight and 6am (inclusive)
(a4 <- filter(flights, dep_time ==2400 | dep_time <= 0600))
# note: time is written as numbers here; 2400 is max "number" 
# we only have to take all flights with time "smaller" 600 and add those flights that 
# have time 2400

#Exercise 2 Another useful dplyr filtering helper is between() . 
# What does it do? Can you use it to simplify the code needed 
# to answer the previous challenges?

?between()
# for 1.4 Departed in summer (Juli, August and September)
a3 <- filter(flights, between(month, 7, 9))

# Exercise 3: How many flights have a missing dep_time (1)? 
# What other variables are missing (2)? What might these rows represent?

number_NA_deptime <- filter(flights, is.na(dep_time))
# one can see that arr_time is always missing as well when dep_time is NA
# those flights might be cancelled flights 

# Exercise 4: (siehe 2.2.4)
FALSE | TRUE
FALSE | FALSE
NA & TRUE
TRUE & TRUE
FALSE & TRUE

## Alternative für filter function: subset() mit relationalem Ausdruck:

subset(flights, arr_delay > 40)

## Change order of rows with arrange function (p. 73 ff.)

a1 <- arrange(flights, month, day)
a2 <- arrange(flights, day, month)
a3 <- arrange(flights, month)
data=flights

# Exercise 5.3.1
# 1.: How could you use arrange() to sort all missing values to the start? 
# (Hint: use is.na())

missing_values_first1 <- arrange(flights, desc(is.na(dep_time)))
# as desc is a function that is used in conjunction w/ numerical values
# boolean variable is automatically coerced into 0 and 1s

# 2. sort flights to find the most delayed flights. And find the flights 
# that left earliest

most_delay <- arrange(flights, desc(dep_delay))
earliest <-  arrange(flights, dep_delay) 
# here "earliest" is apparently meant in sense of having the least delay

# 3. sort flights to find the fastest flights 
# their exists a variable air_time in the data frame which gives the amount of time 
# spent in the air in minutes 

fastest_flights <- arrange(flights, air_time)

# 4. Which flights travelled the longest? Which travelled the shortest?

longest_flights <- arrange(flights, desc(distance))
shortest_flights <- arrange(flights, distance)

?select_helpers

## Select () Function
# Introduction
select(flights, year, month, day)
select(flights, -(year:day))


# renaming variables with select() and rename()
select(flights, Departure.Delay = dep_delay)
rename(flights, Departure.Delay = dep_delay)

# select() & everything()
select(flights, dep_time, arr_time, everything ())
 
# p. 77 ff, Exercises 5.4.1:
# 1. Brainstorm as many ways as possible to select dep_time , dep_delay , arr_time ,
# and arr_delay from flights
flights
b1 <- select(flights, dep_time, dep_delay, arr_time,arr_delay)
b2 <- select(flights, dep_time:arr_delay, -sched_dep_time, -sched_arr_time)
b3 <- select(flights, starts_with("dep_"), starts_with("arr_"))
b4 <- select(flights, ends_with("delay"), ends_with("time"), -sched_dep_time, -sched_arr_time, 
             -air_time)
vector <- c("dep_time", "dep_delay", "arr_time","arr_delay")
b5 <- select(flights,one_of(vector))


# 2. What happens if you include the name of a variable multiple times in a select() call?
select(flights, dep_time, dep_time, dep_time)

# no error or warning is given 

# 3. What does the one_of() function do? Why might it be helpful in conjunction with this
# vector? vars <- c("year", "month", "day", "dep_delay", "arr_delay")

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

select(flights, one_of(vars))
# it is helpful to easily integrate vector in select () function

# 4. Does the result of running the following code surprise you? How do the select helpers
# deal with case by default? How can you change that default?
# select(flights, contains("TIME"))

select(flights, contains("TIME"))
# the result is surprising as R is case-insensitive here 

select(flights, contains("Time", ignore.case=FALSE))

## Mutate()Function 
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
(flights_sml2 <- mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance/air_time*60))

(flights_sml3<- mutate(flights_sml, gain = arr_delay - dep_delay, 
                        speed = distance/air_time*60,
                        gain_in_hours = gain/60))

(flights_sml4<- transmute(flights_sml, gain = arr_delay - dep_delay, 
                        speed = distance/air_time*60,
                        gain_in_hours = gain/60))



# mutate() function and modular arithmetic operator 

(flights_5 <- mutate(flights, hour= dep_time %/% 100, minute= dep_time %% 100))
select(flights, hour, minute, everything())

# mutate() function and lag() Function
x <- 1:10
x-lag(x)

x != lag(x)

# mutate () function and cum. aggregate functions
x <-c(1,2,1,4,5,7,4)
cummin(x)
cummax(x)
cummean(x)
cumsum(x)
cumprod(x)

# min_rank
x <- c(NA, 1, 3, 4, 5, 6)
min_rank(x)

# ntile()

ntile(x, 3)
# mutate () function and %in% function

flights_6 <- mutate(flights, dep_equal_arr_delay= dep_delay %in% arr_delay)
select(flights_6, dep_equal_arr_delay, everything())

flights_7 <- mutate(flights, dep_equal_arr_delay= dep_delay == arr_delay)
select(flights_7, dep_equal_arr_delay, everything())

# diff. between == and %in%

x <- 1:5
y <- 1:10
x %in% y
x == y

## Exercises 5.5.2, p. 83 f. 
# 1. 
# Currently dep_time and sched_dep_time are convenient to look at, but hard to
# compute with because they're not really continuous numbers. Convert them to a more
# convenient representation of number of minutes since midnight

flights_new2 <- mutate(flights, dep_time_minutes = dep_time%/%100*60 + dep_time%%100)

# alternativ: floor() function:

flights_new <- mutate(flights, floor(dep_time/100)*60 + (dep_time - floor(dep_time/100)*100))
# as time is treated as numerical value here, we can get to the minutes by subtracting 
# the hour value * 100; e.g. from 1518 (hence 15:18) minutes are calc. as 1518-15*100

# alternativ 2: trunc() function: 
flights_new3 <- mutate(flights, trunc(dep_time/100)*60 + (dep_time - trunc(dep_time/100)*100))

# 2. Compare air_time with arr_time - dep_time . What do you expect to see? What
# do you see? What do you need to do to fix it?

flights_new1<- mutate(flights, air_time_2= arr_time - dep_time,
                      air_time_diff = air_time_2 - air_time)

(filter(flights_new1, air_time_diff !=0))

# filter shows that for many flights airtime 2 is not equal airtime 
# possible solution: conversion into minutes

flights_new2 <- mutate(flights, arr_time_minutes= arr_time%/%100*60 + arr_time%%100,
                       dep_time_minutes= dep_time%/%100*60 + dep_time%%100,
                       air_time_2= arr_time - dep_time,
                      air_time_diff = air_time_2 - air_time)

(filter(flights_new2, air_time_diff !=0))

# still does not solve the problem
# only explanation might be that there are time differences, so that the times
# stated are maybe not the ones that one can calculate with 

#3. Compare dep_time , sched_dep_time , and dep_delay . How would you expect
# those three numbers to be related?

# one would expect that dep_time = sched_dep_time + dep_delay 

flights_new2 <- mutate(flights,  dep_time_minutes= dep_time%/%100*60 + dep_time%%100, 
                       sched_dep_time_minutes= sched_dep_time%/%100*60 + sched_dep_time%%100,
                       dep_time_calc= sched_dep_time_minutes + dep_delay,
                       diff_dep_time_calc= dep_time_minutes - dep_time_calc)

diff <- filter(flights_new2, diff_dep_time_calc !=0)

# still 1207 rows left, were dep_time_calc is not equal dep_time 
# the problem is, that for those flights left, the sched_dep_time was on the day 
# before the actual dep. However, as we calcukated sched_dep_time_minutes above 
# we assumed that it was on the same day, getting a high number of minutes after midnight 

filter(diff, sched_dep_time > dep_time)

# the filter confirms that for all flights left sched_dep_time > dep_time 

flights_new3 <- mutate(diff, dep_time_minutes= dep_time%/%100*60 + dep_time%%100,
                       sched_dep_time_minutes = 24*60- (sched_dep_time%/%100*60 + sched_dep_time%%100),
                       dep_time_calc = dep_delay - sched_dep_time_minutes,
                       diff_dep_time_calc2 = dep_time_minutes - dep_time_calc)
# what I do here is that I 1) calculate sched_dep_time in minutes until midnight "they day before"
# if for instance the sched_dep_time was 23:59, in order to get the dep_time for the next day, 
# I only need to substract 1 minute from the stated dep_delay in minutes 
# to get to the dep_time on the next (!) day

diff_2 <- filter(flights_new3, diff_dep_time_calc2 !=0)

# now no differences are left :) 

# 4.  Find the 10 most delayed flights using a ranking function. How do you want to handle
# ties? Carefully read the documentation for min_rank()

most_delay <- mutate(flights, rank = min_rank(desc(dep_delay)))
ten_most_delay <- filter(most_delay, rank %in% 1:10)
                                                
# alternative: break the ties with order function
ten_most_delay2 <- mutate(flights, rank = min_rank(desc(dep_delay)))%>%
  arrange(rank) %>%
  mutate(rank_by_index= row_number(rank)) %>%
  filter(rank_by_index %in% 1:10)

top_ten <- flights %>% top_n(10, dep_delay)

most_delay2 <- mutate(most_delay, rank_by_index = order(rank))
ten_most_delay2 <- filter(most_delay2, rank_by_index %in% c(1:10))

flights %>%
  filter(min_rank(-(dep_delay)) %in% 1:10)

# 5. What does 1:3 + 1:10 return? Why?

1:3 + 1:10

## summarise () function

?summarise

summarise(flights, mean_delay= mean(dep_delay, na.rm=TRUE))

flights_grouped_by<- group_by(flights, year, month)
summarise(flights_grouped_by, mean_delay= mean(dep_delay, na.rm=TRUE))

# n_distinct

n_distinct_test <- flights %>% summarise(n_distinct(carrier))


# spannweite 
summarise(flights, spannweite = max(dep_delay, na.rm =TRUE)-min(dep_delay, na.rm =TRUE))

(max<- summarise(flights, max(dep_delay, na.rm = TRUE)))
(min<- summarise(flights, min(dep_delay, na.rm = TRUE)))

summarise(flights, range(dep_delay, na.rm = TRUE))


## the pipe 

flights_by_dest <- group_by(flights, dest)
comparison <- summarise(flights_by_dest, 
                        dist = mean(distance, na.rm = TRUE),
                        delay = mean(arr_delay, na.rm = TRUE),
                        count = n())

ggplot(data = comparison, aes(dist, delay)) +
  geom_point(aes(size = count)) +
  geom_smooth(se = FALSE)
                        
# filter to remove noise points: 
comparison <- filter(comparison, count > 20, dest != "HNL")
# this info was given in the book 

ggplot(data = comparison, aes(dist, delay)) +
  geom_point(aes(size = count)) +
  geom_smooth(se = FALSE)

## same code with pipe:

comparison <- flights %>%
  group_by(dest) %>%
  summarise(dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE),
            count = n()) %>%
  filter(count > 20, dest != "HNL")

ggplot(data = comparison, aes(dist, delay)) +
  geom_point(aes(size = count)) +
  geom_smooth(se = FALSE)

## counts
not_cancelled <- filter(flights, !is.na(dep_time))
(delay_statistic1 <- not_cancelled %>%
  group_by(tailnum) %>%  
  summarise(delay = mean(arr_delay, na.rm =TRUE),
  count = n()))

# this function provides the average arr. delay and the number of flights per tail number 
# (this is the number identifying a plane)

ggplot(data= delay_statistic1, aes(count, delay)) +
  geom_point(alpha = 1/10)

# filter out smallest groups: 

delay_statistic1 %>%
filter(count > 60) %>%
ggplot(aes(count, delay)) +
  geom_point(alpha = 1/10) 

# Alternative:   
ggplot(data= filter(delay_statistic1, count > 30), aes(count, delay)) +
  geom_point(alpha = 1/10)

not_cancelled <- filter(flights, !is.na(dep_time))
(delay_statistic2 <- not_cancelled %>%
    group_by(tailnum) %>%  
    summarise(delay = mean(arr_delay, na.rm =TRUE)) %>%  
    summarise(count = n()))

# this simply provides the number of tailnumber clusters with their respective mean 
# delay 

not_cancelled <- filter(flights, !is.na(dep_time))
not_cancelled %>% 
  group_by(year, month, day)%>%
  summarise(n_early = count(dep_time < 500))

# another example on using count function
# access average performance of batters [Schlagmann] in baseball related to the # of times 
# they're at bat 

library(Lahman)

(performance <- Batting %>%
  group_by(playerID) %>%
  summarise(ave_Perf = (sum(H, na.rm = TRUE)/sum(AB, na.rm = TRUE))*100, 
            number_AB=sum(AB, na.rm = TRUE)))

ggplot(data = filter(performance, number_AB > 100), aes(number_AB, ave_Perf))+
  geom_point(alpha = 1/10)+
  geom_smooth(se = FALSE, color = "red")

(x <- Batting %>% 
    summarise(quantile(H, 0.75)))

# count

flights %>%
count(tailnum)

flights %>%
  count(tailnum, wt=distance)

flights %>%
  group_by(year, month, day) %>%
  summarise(sum(dep_time < 500, na.rm= TRUE))

# 1. define dataset to be grouped progressively
daily <-  group_by(flights, year, month, day)

# 2. do first group
(flights_per_day <-
    summarise(daily, flights = n()))

# 3. do second group
(flights_per_month <- summarise(per_day, flights = sum(flights)))

# 4. do third group
(flightsper_year <- summarise(per_month, flights = sum(flights)))

## p. 105 f. Exercises 5.6.7

# 1. Brainstorm at least 5 different ways to assess the typical delay characteristics of a
# group of flights. Consider the following scenarios

scenario_analysis1 <- not_cancelled %>%
  group_by(flight) %>%
  filter(arr_delay >= 0) %>%
  summarise(quantile(arr_delay, 0.25), quantile(arr_delay, 0.5), "Q-0.75" = quantile(arr_delay, 0.75))

scenario_analysis2 <- not_cancelled %>%
  group_by(flight)%>%
  summarise(number_of_flights = n(),
    fifteen_minutes_early = round(mean(arr_delay == -15, na.rm = TRUE)*100, 0),
         fifteen_minutes_late = round(mean(arr_delay == 15, na.rm = TRUE)*100, 0), 
         ten_minutes_late = round(mean(arr_delay == 10, na.rm =TRUE)*100, 0),
         thirteen_minutes_early = round(mean(arr_delay == -30, na.rm = TRUE)*100, 0), 
         thirteen_minutes_late = round(mean(arr_delay == 30, na.rm = TRUE)*100, 0),
         on_time = round(mean(arr_delay == 0, na.rm = TRUE)*100, 0), 
         two_hours_late = round(mean(arr_delay == 120, na.rm = TRUE)*100, 0))

# A flight is 50% of the time 15 minutes early, and 50% of the time 15 minutes late
filter(scenario_analysis2, fifteen_minutes_early == 50 & fifteen_minutes_late == 50)
  
# A flight is always 10 minutes late 
filter(scenario_analysis2, ten_minutes_late == 100)

# A flight is 30 minutes early 50% of the time and 30 minutes late 50% of the time 
filter(scenario_analysis2, thirteen_minutes_early == 50 & thirteen_minutes_late == 50)

# 99% of the time a flight is one time. 1% of the time it's 2 hours late
filter (scenario_analysis2, on_time == 90 & two_hours_late == 1)
  
# 2. Come up with another approach that will give you the same output as not_cancelled
# %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).

# 2.1
(same_output1 <- not_cancelled %>%
  group_by(dest)%>%
  summarise(n()))

# test with count () Function
(not_cancelled %>%
    count(dest))

# it yields the same result

# 2.2
(same_output2 <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(total_distance = sum(distance, na.rm = TRUE)))

not_cancelled %>% 
  count(tailnum, wt= distance)

# also yields the same result

# 4. Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of
# cancelled flights related to the average delay?

cancelled_and_average_delay <- flights %>%
  group_by(year, month, day)%>%
  summarise(prop_cancelled_per_day = mean(is.na(dep_time)), 
            average_delay = mean(dep_delay, na.rm = TRUE))

ggplot(data= filter(cancelled_and_average_delay, prop_cancelled_per_day <=0.3), aes(prop_cancelled_per_day, average_delay))+
  geom_point(alpha =1/5)+
  geom_smooth(se=FALSE, color="red")

# 5. Which carrier has the worst delays?

(worst_dep_delay_per_carrier <- flights %>%
  group_by(carrier)%>%
  filter(dep_delay > 0)%>%
  summarise(avg_delay_per_year = mean(dep_delay, na.rm = TRUE))%>%
  arrange(desc(avg_delay_per_year)))

worst_arr_delay_per_carrier <- flights %>%
    group_by(carrier)%>%
    filter(arr_delay > 0)%>%
    summarise(avg_delay_per_year = mean(arr_delay, na.rm = TRUE))%>%
    arrange(desc(avg_delay_per_year))
  
ggplot(data = worst_arr_delay_per_carrier, aes(carrier, avg_delay_per_year))+ 
  geom_bar(stat="identity")

ggplot(data = worst_arr_delay_per_carrier, aes(carrier, avg_delay_per_year))+ 
  geom_col(color = "red")

# 5.2 Challenge: can you disentangle the effects of bad
# airports vs. bad carriers? Why/why not? (Hint: think about flights %>%
# group_by(carrier, dest) %>% summarise(n()) )

# what we basically need is a multiple regression, capturing the influence on an airport (origin)
# and carrier on the resulted dep_delay 

attach(flights)   # the attach function is to provide the lm function with the data that we want to use
multiple_regression <- lm(dep_delay~ carrier + origin)
summary(multiple_regression)

?attach
  
# 6. What does the sort argument to count() do. When might you use it?
flights %>%
  count(tailnum, sort = TRUE)

## Group and Filter
(gr_fil <- flights %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay))< 10))

(popular_dests <- flights %>%
    group_by(dest) %>%
    filter(n()> 365))

vignette("window-functions")

(good_performance <- Batting %>%
  group_by (playerID)%>%
  filter(G > lag(G)))


## Exercises 5.7.1 p. 109

# 1 Refer back to the lists of useful mutate and filtering functions. Describe how each
# operation changes when you combine it with grouping.

# tests are done with Lahman library and Batting data set

# 1) mutate functions 
# 1.1) cumulative aggregates
# cumsum

number_hits <- Batting %>%
  mutate(cumsum(H))
# simply cumulates all hits by order of the given dataset 

number_hits2 <- Batting %>%
  group_by(playerID)%>%
  mutate(cum_hits = cumsum(H))%>%
  filter(playerID == "torreyo01")
 # cumulates hits for each player 

# the same applies for all other cum functions, e.g.: 
# cummean
(number_hits <- Batting %>%
  mutate(cummean(H)))

(number_hits2 <- Batting %>%
  group_by(playerID)%>%
  mutate(cum_hits = cummean(H))%>%
  filter(playerID == "torreyo01"))
# 2) the same principle also applies to filter help functions

# 2. Which plane ( tailnum ) has the worst on-time record?
(worst <- flights%>%
  group_by(tailnum)%>%
  summarise(mean = mean(arr_delay, na.rm = TRUE)) %>%
    filter(min_rank(desc(mean))==1))

# 3. What time of a day should you fly if you want to avoid delays as much as possible?
# we are looking for hours(?) of the day in which most delays occur

# taking the length of delay as decisive attribute
(worst_time <- flights %>%
  filter(arr_delay >0 ) %>%
  group_by(hour) %>%
  summarise(mean = mean(arr_delay, na.rm = TRUE)) %>%
  filter(min_rank(desc(mean)) == 1))

# using probability of having a delay 

(worst_time <- flights %>%
    filter(!is.na(arr_delay))%>%
    group_by(hour) %>%
    summarise(number = mean(arr_delay > 0)) %>%
    filter(min_rank(desc(number)) == 1))

# with ggplot: 
(worst_time <- flights %>%
    filter(!is.na(arr_delay))%>%
    group_by(hour) %>%
    summarise(prob = mean(arr_delay > 0)) %>%
    ggplot(aes(hour, prob))+
    geom_point(aes(size= prob)))

# 4. For each destination, compute the total minutes of delay. For each flight, compute the
# proportion of the total delay for its destination.

(solution_mutate1 <- flights %>% 
  filter(arr_delay > 0) %>%  
  group_by(dest) %>%
    summarise(total_delay = sum(arr_delay, na.rm = TRUE)))

# for the second task it is better to use the mutate() function as here another column is added
# and values are not collapsed towards the grouping and it is required to the the calculation
# for each (!) flight

solution <- flights %>% 
  filter(!is.na(arr_delay), arr_delay > 0) %>%  
  group_by(dest) %>%
  mutate(total_delay = sum(arr_delay),prop_delay = arr_delay / sum(arr_delay))
      
# 5. Delays are typically temporally correlated: even once the problem that caused the
# initial delay has been resolved, later flights are delayed to allow earlier flights to leave.
# Using lag() explore how the delay of a flight is related to the delay of the
# immediately preceding flight.

(lag <- flights %>%
  group_by(year, month, day)%>% # note: we have to group by day to avoid using a day t-1 as a lag
  filter(!is.na(dep_delay))%>%
  mutate(lag_delay = lag(dep_delay)) %>%
  # ggplot zeigt an, dass 365 zeilen missing values beinhalten 
  # that makes perfectly sense because the first flight on each day does not have a lag and 
  # thus results in NA
  filter(!is.na(lag_delay))) # nachdem NA Werte herausgefilter wurden, wird es nicht mehr angezeigt
  
  ggplot (data = filter(lag, lag_delay <500), aes(lag_delay, dep_delay))+
  geom_point(alpha = 1/10)+
    geom_smooth()

# 6. Grouped mutations
# (1) Look at each destination. Can you find flights that are suspiciously fast (1)? (i.e. flights
# that represent a potential data entry error). 

(fast <- flights %>%
  filter(!is.na(dep_delay) | !is.na(arr_delay)) %>%
  group_by(dest)%>%
  mutate(sched_airtime = (sched_arr_time%/%100*60+sched_arr_time%%100) - (sched_dep_time%/%100*60 + sched_dep_time%%100), 
         diff_sched_actual = sched_airtime - air_time))

(fast_2 <- fast%>% arrange(distance) %>% filter(diff_sched_actual > 60) %>% 
  select(dest, sched_arr_time, sched_dep_time, sched_airtime, air_time, diff_sched_actual, distance))
  
# (2) Compute the air time of a flight relative to the shortest flight to that destination. 
# Which flights were most delayed in the air?
  
(airtime_vs_distance <- flights %>%
  filter(!is.na(dep_delay) | !is.na(arr_delay)) %>%
  group_by(dest)%>%
  mutate(min_distance = min(distance))%>%
  mutate(airtime_vs = air_time / min_distance))
        

# 7. Find all destinations that are flown by at least two carriers. Use that information to
# rank the carriers.

two_carriers <- flights %>% 
  group_by(dest, carrier) %>%
  summarise(n_flights_carrier = n())%>%
  mutate(number = row_number(n_flights_carrier))%>%
  group_by(dest)%>%
  filter(number >1)%>% 
  summarise(sum = sum(number))
# this is overall an unelegant solution because I create I variable that I don't need 
# the mutate function w/ number of rows allows me to select only dest that have more than 
# 1 row of carrier, hence min. 2 carriers

# more elegant solution: 

two_carriers <- flights %>% 
  group_by(dest) %>%
  summarise(more_than_2 = n_distinct(carrier))%>%
  filter(more_than_2 > 2)%>%
  select(dest, more_than_2)

# 8. For each plane, count the number of flights before the first delay of greater than 1
# hour.

# this can only be solved w/ boolean variables: 
(count <- flights %>%
  arrange(year, month, day)%>% # assures that flights are chronological 
  group_by(tailnum) %>%
  mutate(delay = dep_delay > 60) %>%  # creates columns w/ T/F 
  mutate(cum_delay = cumsum(delay)) %>%
  filter(cum_delay < 1)%>%
    count(sort= TRUE))

## filter all and filter if function

filter_all(mtcars, all_vars(. > 0))
filter_all(mtcars, any_vars(. > 200)
filter_at(mtcars, vars(starts_with("h")), any_vars(.>160))
filter(mtcars, hp > 160)
filter_if(mtcars, ~ all(floor(.) == .), all_vars(. != 0))
