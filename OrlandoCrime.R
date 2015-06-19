#---------------------------------------------------------------------------
# ANALYSIS oF POLICE DISPATCH DATA
# 
# This file contains the analysis of a historical record of police dispatch
# Data in the city of Orlando
#
# URL: http://www.codefororlando.com/data-sources/
#---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(RSQLite)
library(maps)
library(ggmap)
library(lubridate)

con <- dbConnect(RSQLite::SQLite(), "C:\\sqlite\\2014-12-snapshot.sqlite3db")
from_db <- function(sql) {
  dbGetQuery(con, sql)
}

# Database Schema
# .schema
# CREATE TABLE reasons (id integer primary key, desc text, nice_desc text);
# CREATE TABLE calls (id integer primary key, t integer, reason integer, latitude real, longitude real, address text, source text);
# CREATE TABLE tweq (id, userid TEXT, message TEXT, t INTEGER, lat TEXT, long TEXT);
# CREATE TABLE tweet_queue (id integer primary key autoincrement, userid TEXT, message TEXT, t INTEGER, lat TEXT, long, TEXT);
# CREATE TABLE nicenames (id INTEGER PRIMARY KEY, address TEXT, desc TEXT);

# --------------------------------------------------------------------
# Let's examine the structure of the data from each table
# --------------------------------------------------------------------

# reasons
df.head <- from_db("SELECT * FROM reasons LIMIT 5")
str(df.head)
# id       : int  1 2 3 4 5
# desc     : chr  "suspicious incident" "battery" "stolen vehicle" "general disturbance" ...
# nice_desc: chr  NA NA NA NA ...

# calls
df.head <- from_db("SELECT * FROM calls LIMIT 5")
str(df.head)
# id       : int  90034373 90034384 90034607 90034824 90035333
# t        : chr  "2009-04-09 00:00:00" "2009-04-10 00:00:00" "2009-04-10 00:00:00" "2009-04-11 00:00:00" ...
# reason   : int  113 113 117 115 113
# latitude : num  28.7 28.6 28.7 28.7 28.7
# longitude: num  -81.5 -81.5 -81.5 -81.5 -81.5
# address  : chr  "1463 S Hawthorne Ave, Apopka, FL 32703" "7317 Country View Terrace, Orlando, FL 32818" "148 E 12th St, Apopka, FL 32703" "36 E 13th St, Apopka, FL 32703" ...
# source   : chr  "ocso" "ocso" "ocso" "ocso" ...

# tweq
df.head <- from_db("SELECT * FROM tweq LIMIT 5")
str(df.head)
# id     : int  3 7 8 9 11
# userid : chr  "orlpol32751" "orlpol32130" "orlpol32779" "orlpol34734" ...
# message: chr  "theft at Lake Ave & East St, Maitland, FL 32751" "residential alarm at 601 Johnson Lake Rd, De Leon Springs, FL 32130-3638" "suspicious person at 365 Wekiva Springs Road, Longwood, FL 32779-3685" "battery - shooting at 10000 W Colonial Dr, Gotha, FL 34734" ...
# t      : chr  "2009-12-19 01:45:00" "2010-01-07 13:13:00" "2010-01-29 21:46:00" "2010-01-31 03:15:00" ...
# lat    : chr  "28.618539" "29.155354" "28.690559" "28.550616" ...
# long   : chr  "-81.375646" "-81.353134" "-81.409246" "-81.527609" ...

# tweet_queue
df.head <- from_db("SELECT * FROM tweet_queue LIMIT 5")
str(df.head)
# id     : int 
# userid : chr 
# message: chr 
# t      : int 
# lat    : chr 
# long   : chr 

# The tweet_queue table is empty so we will not be using it in our analysis.

# nicenames
df.head <- from_db("SELECT * FROM nicenames LIMIT 5")
str(df.head)
# id     : int  1 2 3 4 5
# address: chr  "100 s hughey ave" "100 south hughey avenue" "555 n john young pkwy" "2803 arlington st" ...
# desc   : chr  "#OPDHq" "#OPDHq" "#GreyhoundStation" "#OCSOHq" ...

# ----------------------------------------------------------------------------
# QUESTIONS
#  Q. Where in Orlando are police dispatched the most?
#  Q. What are the most common reasons police are dispatched?
#  Q. Are certain areas in Orlando associated with particular dispatch
#     reasons?
#  Q. What time of the day, day of the week, and time of the year are police
#     dispatched the most?
#  Q. How has the amount of police dispatches changed over the years?
#
# IMPORTANT VARIABLES
# from reasons
#  id
#  desc
#  nice_desc
#
# from calls
#  id
#  t
#  reason
#  latitude
#  longitude
#  address
# ----------------------------------------------------------------------------

# Q. Where in Orlando are police dispatched the most?

# Let's first get an overview of the data. 
df.dispatch <- from_db("SELECT latitude, longitude FROM calls")
df.dispatchCounts <- from_db("SELECT latitude, longitude, count(*) FROM calls GROUP BY latitude, longitude")

# Filter out the records that have missing longitude and latitude values
df.dispatch <- filter(df.dispatch, !is.na(latitude) & !is.na(longitude))
df.dispatchCounts <- filter(df.dispatchCounts, !is.na(latitude) & !is.na(longitude))

# Print out the frequency table of dispatch counts

table(df.dispatchCounts$count)

# Let's see this table in bar chart form
ggplot(data = as.data.frame(table(df.dispatchCounts$count)), aes(x = as.numeric(Var1), y = Freq)) +
  geom_bar(stat = "identity")

# A very large majority of locations have been visited by police only a few times. A very small percentage of
# locations have been visited heavily.

# How many times were Police dispatched to 99% of the the locations?
sort(df.dispatchCounts$count)[(ceiling(0.99*length(df.dispatchCounts$count)))] # 59

# 99% of the locations in Orlando have been visited by Police 59 times!!!

# Let's create a density map of the dispatch locations.
df.orlandoMap <- qmap(location = "orlando", zoom = 11, maptype = "toner", source="stamen", darken = c(.3,"#BBBBBB"))
df.orlandoMap +
  geom_point(data = df.dispatch, aes(x = longitude, y = latitude), color = "dark red", alpha = 0.002, size = 0.1) +
  ggtitle("Orlando Police Call Locations, 2009 - 2014") +
  theme(text = element_text(family = "Tahoma", face = "bold", size = 12)) +
  theme(plot.title = element_text(color = "#747678", size = 24))

# Another density plot using tile geoms with stat_density2d
df.orlandoMap +
  geom_tile(data = df.dispatch, stat = "density2d", contour = FALSE, 
            aes(x = longitude, y=latitude, fill = ..density.., alpha = ..density..)) +
  scale_fill_continuous(low = "#2fff00", high = "#046d00", guide = "none") +
  scale_alpha(range = c(0,1), guide = "none")

# Both plots show Downtown Orlando as being the place Police are dispatched the most.  

# Q. What are the most common reasons Police are dispatched?

# First, let's retrieve our records from the database.

df.dispatchReasons <- from_db("SELECT reason, count(*) AS count FROM calls GROUP BY reason")
df.reasons <- from_db("SELECT id, desc FROM reasons WHERE id IN (SELECT DISTINCT reason FROM calls)")
df.reasons$desc <- tolower(df.reasons$desc)

# Now Join the tables and get the top 10 reasons

df.dispatchReasons %>%
  inner_join(df.reasons, by = c("reason" = "id")) %>%
  select(desc, count, reason) %>%
  arrange(desc(count)) %>%
  filter(min_rank(desc(count)) <= 10)

#                  desc count
#1  general disturbance 66650
#2    suspicious person 50120
#3     commercial alarm 34455
#4     accident (minor) 32452
#5              battery 31787
#6      unknown trouble 31398
#7           trespasser 26402
#8                theft 20316
#9  suspicious incident 20130
#10   residential alarm 19892

# Plot the first 63 records
df.dispatchReasons %>%
  inner_join(df.reasons, by = c("reason" = "id")) %>%
  select(desc, count) %>%
  arrange(desc(count)) %>%
  filter(min_rank(desc(count)) <= 63) %>%
  mutate(desc = factor(desc, levels = rev(desc))) %>%
  ggplot(aes(x=desc, y=count)) +
  geom_bar(stat="identity", fill = "orange", alpha = 0.7) +
  geom_text(aes(x=desc, y=count, label = count), hjust = 1) +
  scale_y_continuous(limits = c(0, 70000)) +
  coord_flip() +
  ggtitle("Active Police Calls By Reason") +
  theme(text = element_text(family = "Tahoma", face = "bold", size = 12)) +
  theme(plot.title = element_text(color = "#747678", size = 24)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

# Plot the remaining records
df.dispatchReasons %>%
  inner_join(df.reasons, by = c("reason" = "id")) %>%
  select(desc, count) %>%
  arrange(desc(count)) %>%
  filter(min_rank(desc(count)) > 63) %>%
  mutate(desc = factor(desc, levels = rev(desc))) %>%
  ggplot(aes(x=desc, y=count)) +
  geom_bar(stat="identity", fill = "orange", alpha = 0.7) +
  geom_text(aes(x=desc, y=count, label = count)) +
  scale_y_continuous(limits = c(0, 70000)) +
  coord_flip() +
  ggtitle("Active Police Calls By Reason") +
  theme(text = element_text(family = "Tahoma", face = "bold", size = 12)) +
  theme(plot.title = element_text(color = "#747678", size = 24)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

#  Q. Are certain areas in Orlando associated with particular dispatch reasons?

# Retrieve the top 12 dispatch reasons from the database
df.dispatch <- from_db("SELECT latitude, longitude, reason FROM calls WHERE reason IN (4,5,16,22,2,30,17,15,1,14,13,9)")

df.orlandoMap +
  geom_point(data = df.dispatch, aes(x = longitude, y = latitude, color = as.factor(reason)),size = 0.1) +
  facet_wrap(~reason)

# Retrieve the next 12 dispatch reasons from the database
df.dispatch <- from_db("SELECT latitude, longitude, reason FROM calls WHERE reason IN (77,26,20,34,7,31,104,27,35,42,61,24)")
df.orlandoMap +
  geom_point(data = df.dispatch, aes(x = longitude, y = latitude, color = as.factor(reason)),size = 0.1) +
  facet_wrap(~reason)

# There doesn't appear to be an association between dispatch reasons and locations

#  Q. What time of the day, day of the week, and time of the year are police
#     dispatched the most?


# Lets overlay the police call counts over a calendar chart. Are there any noticable patterns we can
# identify?

df.dispatch <- from_db("SELECT DATE(t) AS date, count(*) AS count FROM calls GROUP BY date")
df.dispatch$date <- ymd(df.dispatch$date)

df.dispatchCal <- mutate(df.dispatch,YEAR = factor(year(date)),
         MONTH = factor(month(date, label=TRUE, abbr=TRUE)),
         WEEKDAY = factor(wday(date, label=TRUE, abbr=TRUE), levels=c('Sun', 'Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat')),
         MONTHDAY = mday(date),
         MONTH_FIRST_WDAY=wday(ymd(paste(YEAR,MONTH,1,sep='-'))),
         MONTHWEEK=factor(floor((mday(date)+MONTH_FIRST_WDAY-2)/7)+1, levels=c(1:6))
  )

ggplot(data = df.dispatchCal, aes(x=MONTHWEEK,y=WEEKDAY, fill=count)) +
  geom_tile(color='white') +
  facet_grid(YEAR~MONTH, drop=FALSE) +
  scale_y_discrete(drop=FALSE, limits = rev(levels(df.dispatchCal$WEEKDAY))) +
  scale_x_discrete(drop=FALSE) +
  scale_fill_gradient(low = "#87eefd", high = "#011476", name = "Number of\nActive Calls") +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(text = element_text(family = "Tahoma", face = "bold"))

# I'm noticing a dramatic increase in police calls after March 2014. Aside from that I'm not seeing any noticable patterns.

# Question to explore later: What could have happened in 2014 to cause this increase? 
# to cause this? 

# Let's try plotting the averages for each day of the week, month, hour of the day and see if this gives us
# a better view of the data.

theme.averages <- theme(text = element_text(family = "Tahoma", face = "bold", size = 16)) +
                  theme(plot.title = element_text(color = "#747678", size = 24)) +
                  theme(axis.title.x = element_blank()) +
                  theme(axis.title.y = element_blank())

# Averages for each month of the Year

df.monthOfYear <- df.dispatch %>%
  mutate(YEAR = factor(year(date)),
         MONTH = factor(month(date, label=TRUE, abbr=TRUE))) %>%
  select(YEAR, MONTH, count) %>%
  group_by(YEAR, MONTH) %>%
  summarize(avg = mean(count))

ggplot(data = df.monthOfYear, aes(x=as.numeric(MONTH), y=avg)) +
  geom_area(fill = "#152ddd", alpha = 0.5) +
  facet_wrap(~YEAR) +
  scale_x_continuous(breaks = as.numeric(df.monthOfYear$MONTH), labels = df.monthOfYear$MONTH) +
  ggtitle("Average Active Police Calls By Month") +
  theme.averages

# There doesn't appear to be any noticable patterns in the montly police call averages.

# Averages for each day of the week

df.dayOfWeek <- df.dispatch %>%
  mutate(YEAR = factor(year(date)),
         WEEKDAY = factor(wday(date, label=TRUE, abbr=TRUE), levels=c('Sun', 'Mon', 'Tues', 'Wed', 'Thurs', 'Fri', 'Sat'))
  ) %>%
  select(YEAR, WEEKDAY,count) %>%
  group_by(YEAR,WEEKDAY) %>%
  summarize(avg = mean(count))

ggplot(data = df.dayOfWeek, aes(x=as.numeric(WEEKDAY), y=avg)) +
  geom_area(fill = "#152ddd", alpha = 0.5) +
  facet_wrap(~YEAR) +
  scale_x_continuous(breaks = as.numeric(df.dayOfWeek$WEEKDAY), labels = df.dayOfWeek$WEEKDAY) +
  ggtitle("Average Active Police Calls By Week Day") +
  theme.averages

# The average number of police calls during each day of the week seems to be pretty level,
# slightly higher on Friday and slightly lower on Sunday.

# Averages for each hour of the day

df.dispatchHours <- from_db("SELECT DATE(t) as date, strftime('%H',t) AS hour, count(*) as count FROM calls GROUP BY date, hour")
df.dispatchHours$date <- ymd(df.dispatchHours$date)
df.dispatchHours$hour <- as.numeric(df.dispatchHours$hour)

df.timeOfDay <- df.dispatchHours %>%
  mutate(YEAR = factor(year(date)),
         HOUR = factor(hour)) %>%
  select(YEAR, HOUR, count) %>%
  group_by(YEAR, HOUR) %>%
  summarize(avg = mean(count))

ggplot(data = df.timeOfDay, aes(x=as.numeric(HOUR), y=avg)) +
  geom_area(fill = "#152ddd", alpha = 0.5) +
  facet_wrap(~YEAR) +
  scale_x_continuous(breaks = as.numeric(df.timeOfDay$HOUR), labels = df.timeOfDay$HOUR) +
  ggtitle("Average Active Police Calls By the Hour") +
  theme.averages

# The number of police calls in Orlando peaks at 6PM and then drops to it's lowest point
# at 5AM.

# Q. How has the number of police calls change over the years?
#

df.callsOverTime <- df.dispatch %>%
  mutate(MonYear = as.Date(format(date, format="%Y-%m-01"))) %>%
  select(MonYear, count) %>%
  group_by(MonYear) %>%
  summarize(avg = mean(count)) 

ggplot(data = df.callsOverTime, aes(x = as.numeric(MonYear), y = avg)) +
  geom_area(fill = "#152ddd", alpha = 0.5) +
  scale_x_continuous(breaks = as.numeric(seq(min(df.callsOverTime$MonYear), max(df.callsOverTime$MonYear), by = "4 months")),
                     labels = format(seq(min(df.callsOverTime$MonYear), max(df.callsOverTime$MonYear), by = "4 months"), format = "%m/%Y")) +
  ggtitle("Average Active Police Calls Per Day") +
  theme.averages
