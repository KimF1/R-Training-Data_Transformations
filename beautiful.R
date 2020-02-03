# dplyr
setwd("C:/Users/Kim F/Desktop/Semester 1/R&SQL_for_Business_Analytics/3. Zusammenfassungen & Guidelines/Ch._5_Data Transformation")
getwd()

library(tidyverse)
library(lubridate)
library(nycflights13)
install.packages("formatR")
library(formatR)
tidy_source("dplyr_lecture.R", file = "beautiful.R")

# Loading some old R data sets. Later on we will import from Excel, CSV-files, SAS etc.

Canteen_data_inRformat <- readRDS(file = "data20171001.Rda")
Canteen_clean <- readRDS(file = "Canteen_clean.Rda")
Canteen_data_inRformat001 <- readRDS(file = "Canteen_data_inRformat001.Rda")

# str is useful when you want to print the structure of the data (vectors)
str(Canteen_data_inRformat)
# View is more like Excel
View(Canteen_data_inRformat)
str(Canteen_clean)
View(Canteen_clean)
str(Canteen_data_inRformat001)
View(Canteen_data_inRformat001)

# dplyr basic Session 2

# filter(): keep/delete observations based on their values df %>% filter(a != NA)
View(Canteen_data_inRformat)

x_filter <- filter(Canteen_data_inRformat, !wday(Date01) %in% c(1, 7, NA))
View(x_filter)

# the function months extracts the month name (the local one)
x_filter <- Canteen_data_inRformat %>% filter(!wday(Date01) %in% c(1, 7, NA) & month(Date01) == 1)
View(x_filter)

x_mutate <- Canteen_data_inRformat %>% mutate(weekday = wday(Date01)) %>% filter(weekday %in% c(1, 7, NA))

# Removing the !
x_filter <- Canteen_data_inRformat %>% filter(wday(Date01) %in% c(1, 7, NA) & month(Date01) == 1)
x_filter


# arrange(): changing the order of the observations
x_arrange <- Canteen_data_inRformat %>% filter(!is.na(Date01)) %>% arrange(Date01, desc(Timofday03), Initials)
`?`(Initials)
x_arrange
tail(x_arrange)


# select(): choose (the order of) the variables in the new dataset
`?`(tidy_source())
tidy_source()
x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(Initials)
View(x_select)


x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(Initials, everything())
View(x_select)

x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(Date01:Department, Salesunit:Subsidy, Price01)
View(x_select)

# Similar to:

x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(-Units01, -Segment)
View(x_select)


x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(-(Units01:Segment))
View(x_select)

# starts with
(x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(starts_with("S")))
View(x_select)


# ends with
(x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(ends_with("ment")))
View(x_select)


# contains
(x_select <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(contains("0")))
View(x_select)

# More on matches and num_range later:)


# mutate()
x_mutate <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    mutate(Price_without_VAT = 0.8 * Price01, Danish_VAT = Price01/Price_without_VAT - 1)
View(x_mutate)


# transmute keeps only new variables
x_mutate <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    transmute(Price_without_VAT = 0.8 * Price01, Danish_VAT = Price01/Price_without_VAT - 1)
View(x_mutate)


# rename
(x_rename <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, Price = Price01))
View(x_rename)


# summarize() makes a data frame collapse to a single row (or to the number of groups)

(x_summarize <- Canteen_data_inRformat %>% filter(!Date01 %in% c(NA, "-")) %>% arrange(Date01, desc(Timofday03), Initials) %>% 
    select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, Price = Price01) %>% 
    summarize(Total = sum(Price)))
View(x_summarize)

(x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(months(Date), Initials) %>% summarize(Total = sum(Price), count = n()))
View(x_summarize)

`?`(n())
x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(months(Date), Initials) %>% summarize(Total = sum(Price), count = n(), 
    mean(Price)) %>% mutate(Average = Total/count)
View(x_summarize)


(x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(wday(Date)) %>% summarize(sum(!is.na(Price))))
View(x_summarize)
View(Canteen_data_inRformat)

(x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% mutate(weekday = wday(Date, label = TRUE)))

# Subsetting the variable Price
x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(wday(Date)) %>% summarize(avg_price1 = mean(Price), avg_price2 = mean(Price[Price > 
    0]))
View(x_summarize)
str(x_summarize)

x_price <- Canteen_data_inRformat %>% select(Price01) %>% filter(Price01 <= 0)

x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(wday(Date)) %>% summarize(min_price1 = min(Price), min_price2 = min(Price[Price > 
    0]))

`?`(week)

# Distinct values
x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(week(Date))
View(x_summarize)
View(Canteen_data_inRformat)



# Counts and proportions using logical values i.e. 0s and 1s (FALSE and TRUE) Useful in connection with sum() and
# mean()
x_summarize <- Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, 
    Units = Units01, Price = Price01) %>% group_by(week(Date)) %>% summarize(Pos_Price = sum(Price > 0), Neg_Price = sum(Price < 
    0), Prop_Pos_Price = mean(Price > 0))
View(x_summarize)

# Compare with subsetting:
Canteen_data_inRformat %>% select(Initials, everything()) %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, 
    Price = Price01) %>% group_by(week(Date)) %>% summarize(avg_price2 = mean(Price[Price > 0]))

# dplyr basic Session 3 Ungrouping

daily <- group_by(flights, year, month, day)

(per_day <- summarize(daily, flights = n()))

ungrouped <- daily %>% ungroup() %>% summarize(flights = n())


# Grouped mutates and filters

z_summarize_mutate <- Canteen_data_inRformat %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, Price = Price01) %>% 
    group_by(Initials) %>% mutate(counts = n(), rank = rank(counts)) %>% arrange(Initials)
View(z_summarize)
v <- c(2, 3, 4, 2, 1)
(rank_test <- rank(v))

z_summarize_filter <- Canteen_data_inRformat %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, Price = Price01) %>% 
    group_by(Initials) %>% mutate(counts = n()) %>% filter(rank(desc(counts)) < 3) %>% arrange(Initials)
View(z_summarize)

z_summarize_without_count <- Canteen_data_inRformat %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, 
    Price = Price01) %>% group_by(Initials) %>% filter(n() > 100) %>% arrange(Initials)
View(z_summarize)

z_summarize_wth_count <- Canteen_data_inRformat %>% rename(Date = Date01, Timeofday = Timofday03, Units = Units01, Price = Price01) %>% 
    group_by(Initials) %>% mutate(count = n())
filter(n() > 100) %>% arrange(Initials)

z_summarize_i <- z_summarize_wth_count %>% ungroup %>% filter(Price > 0) %>% mutate(Prop = Price/sum(Price)) %>% arrange(desc(Prop))

z_summarize_iI <- z_summarize_wth_count %>% summarise(sum(Price[Price > 0]))


