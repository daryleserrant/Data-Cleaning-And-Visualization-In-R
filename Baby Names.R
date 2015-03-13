
library(ggplot2)
library(dplyr)

#chart theme
theme.chart <-
  theme(plot.title = element_text(color="#666666", face="bold", size=22)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=18))

df.baby_names <- read.csv("F:\\References\\Data Science\\R scripts\\Baby_Names__Beginning_2007.csv")

# New York Babies Named Diana
df.diana_name <- df.baby_names %>%
  filter(First.Name == "DIANA") %>%
  group_by(Year) %>%
  summarise(total=sum(Count))

ggplot(data=df.diana_name, aes(x=Year, y=total)) +
  geom_line(colour="#990000", size=1) +
  ggtitle("New York Babies Named Diana") +
  labs(x="Year", y="Count") +
  theme.chart

# New York Babies Named Joshua
df.joshua_name <- df.baby_names %>%
  filter(First.Name == "JOSHUA") %>%
  group_by(Year) %>%
  summarise(total=sum(Count))

ggplot(data=df.joshua_name, aes(x=Year, y=total)) +
  geom_line(colour="#990000", size=1) +
  ggtitle("New York Babies Named Joshua") +
  labs(x="Year", y="Count") +
  theme.chart


# Babies Born in New York County, New York
df.new_york_county <- df.baby_names %>%
  filter(County == "NEW YORK") %>%
  group_by(Year, Sex) %>%
  summarise(Count = sum(Count))

str(df.new_york_county)

levels(df.new_york_county$Sex) <- c("Female","Male")

ggplot(data=df.new_york_county, aes(x=Year, y=Count)) +
  geom_line(colour="#990000") +
  facet_wrap(~ Sex) +
  ggtitle("Babies Born in New York County, New York") +
  theme.chart

