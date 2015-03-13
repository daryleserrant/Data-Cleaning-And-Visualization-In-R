library(ggmap)
library(dplyr)
library(RColorBrewer)

# CHART THEME
theme.chart <- 
  theme(panel.background=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.title=element_blank()) +
  theme(plot.title = element_text(size=18, family="Trebuchet MS", face="bold", hjust=0.5, color="#666666"))

ds.map    <- map_data("state")
ds.deaths <- read.csv("F:\\References\\Data Science\\R scripts\\Occupant_and_Alcohol-Impaired_Driving_Deaths_in_States__2003-2012.csv")

states <- tolower(state.name)

categorizeDeaths <- function(x) {
  ifelse(x < 500, 1, ifelse((x >= 500) & (x < 1000), 2, ifelse((x >= 1000) & (x < 1500),3,ifelse((x >= 2000) & (x < 2500),4,ifelse((x >= 2500) & (x < 3000),5,6)))))
}

ds.deaths_cl <- ds.deaths %>%
  filter(State != "United States (50 States & DC)") %>%
  mutate(alcohol_cat = categorizeDeaths(Alcohol.Impaired.Driving.Deaths), occupant_cat = categorizeDeaths(Occupant.Deaths))

ds.map_add <- mutate(ds.map, alcohol = ds.deaths_cl[match(region,states),5], occupant = ds.deaths_cl[match(region,states),6])

ds.map_add$alcohol  <- factor(ds.map_add$alcohol, levels = c(1,2,3,4,5,6,7), labels=c("< 500", "500 - 999", "1000 - 1499", "1500 - 1999", "2000 - 2499", "2500 - 2999", ">= 3000"))
ds.map_add$occupant <- factor(ds.map_add$occupant, levels = c(1,2,3,4,5,6,7), labels=c("< 500", "500 - 999", "1000 - 1499", "1500 - 1999", "2000 - 2499", "2500 - 2999", ">= 3000"))

# Plot Alcohol Driving Deaths 
ggplot(ds.map_add, aes(x=long, y=lat, group=group,fill=alcohol)) +
  geom_polygon(color="black") +
  ggtitle("2003 - 2012 Alcohol Impaired Driving Deaths") +
  scale_fill_manual(values=c("#E9EEFF","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C"))+
  theme.chart +
  coord_map("polyconic")

# Plot Occupant Driving Deaths
ggplot(ds.map_add, aes(x=long, y=lat, group=group,fill=occupant)) +
  geom_polygon(color="black") +
  ggtitle("2003 - 2012 Occupant Impaired Deaths") +
  scale_fill_manual(values=c("#E9EEFF","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C"))+
  theme.chart +
  coord_map("polyconic")