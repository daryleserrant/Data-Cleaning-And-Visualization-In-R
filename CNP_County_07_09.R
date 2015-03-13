library(ggplot2)
library(dplyr)
library(reshape2)

# Chart Themes
theme.chart1 <-
  theme(axis.text.x=element_text(angle=90)) +
  theme(plot.title = element_text(face="bold", size=18, hjust=0.5, color="#D84849")) +
  theme(axis.title = element_text(face="bold", color="#D84849"))

theme.chart2 <-
  theme(plot.title = element_text(face="bold", size=18, hjust=0.5, color ="#666666")) +
  theme(axis.title = element_text(face="bold", color = "#666666")) +
  theme(legend.title = element_text(face="bold", color = "#666666"))

theme.chart3 <-
  theme(plot.title = element_text(face="bold",size=18, hjust=0.5, color="#666666")) +
  theme(axis.title = element_text(face="bold", color = "#666666")) +
  theme(legend.title = element_text(face="bold", color = "#666666"))

# Read Data
#df.comp_07 <- read.csv("C:\\Users\\daryleserrant\\Downloads\\DataGov_CNP_County_FY07_Final.csv",na.strings=c("*"),stringsAsFactors=FALSE)
 #df.comp_08 <- read.csv("C:\\Users\\daryleserrant\\Downloads\\DataGov_CNP_County_FY08_Final.csv",na.strings=c("*"),stringsAsFactors=FALSE)
#df.comp_09 <- read.csv("C:\\Users\\daryleserrant\\Downloads\\DataGov_CnP_FY09_Data_File_starred.csv",na.strings=c("*"),stringsAsFactors=FALSE)
df.comp_07 <- read.csv("http://www1.va.gov/vetdata/docs/DataGov_CNP_County_FY07_Final.csv",na.strings=c("*"),stringsAsFactors=FALSE)
df.comp_08 <- read.csv("http://www1.va.gov/vetdata/docs/DataGov_CNP_County_FY08_Final.csv",na.strings=c("*"),stringsAsFactors=FALSE)
df.comp_09 <- read.csv("http://www.va.gov/VETDATA/docs/Datagov/DataGov_CnP_FY09_Data_File_starred.csv",na.strings=c("*"),stringsAsFactors=FALSE)
# Add Year Column
df.comp_07 <- mutate(df.comp_07, Year="2007")
df.comp_08 <- mutate(df.comp_08, Year="2008")
df.comp_09 <- mutate(df.comp_09, Year="2009")

# Change the column names of the 2009 Compensation Data frame to match the names of
# the other two data frames
names(df.comp_09) <- names(df.comp_07)

# Combine the data frames into one big one
df.comp_070809 <- rbind(df.comp_07,df.comp_08,df.comp_09)

names(df.comp_070809) <- c("State","County","TotalCP","TotalCompensation","TotalPension","MissingAge","AgeBelow35","AgeBetween35N44","AgeBetween45N54","AgeBetween55N64","AgeBetween65N74","AgeAbove75","CompBelow30","CompBetween30N50","CompBetween60N90","Comp100","Year")

convertToNumeric <- function(x) {
  x <- as.numeric(gsub(",","",x))
  ifelse(is.na(x),0,x)
}

# Replace NAs with zeroes
df.comp_070809$TotalCP <- convertToNumeric(df.comp_070809$TotalCP)
df.comp_070809$TotalCompensation <- convertToNumeric(df.comp_070809$TotalCompensation)
df.comp_070809$TotalPension <- convertToNumeric(df.comp_070809$TotalPension)
df.comp_070809$MissingAge <- convertToNumeric(df.comp_070809$MissingAge)
df.comp_070809$AgeBelow35 <- convertToNumeric(df.comp_070809$AgeBelow35)
df.comp_070809$AgeBetween35N44 <- convertToNumeric(df.comp_070809$AgeBetween35N44)
df.comp_070809$AgeBetween45N54 <- convertToNumeric(df.comp_070809$AgeBetween45N54)
df.comp_070809$AgeBetween55N64 <- convertToNumeric(df.comp_070809$AgeBetween55N64)
df.comp_070809$AgeBetween65N74 <- convertToNumeric(df.comp_070809$AgeBetween65N74)
df.comp_070809$AgeAbove75 <- convertToNumeric(df.comp_070809$AgeAbove75)
df.comp_070809$CompBelow30 <- convertToNumeric(df.comp_070809$CompBelow30)
df.comp_070809$CompBetween30N50 <- convertToNumeric(df.comp_070809$CompBetween30N50)
df.comp_070809$CompBetween60N90 <- convertToNumeric(df.comp_070809$CompBetween60N90)
df.comp_070809$Comp100 <- convertToNumeric(df.comp_070809$Comp100)

df.comp_070809$State <- toupper(df.comp_070809$State)
df.comp_070809$County <- toupper(df.comp_070809$County)

# Plot Veterans Total Pension in Florida in 2009
df.comp_070809 %>%
  filter(State == "FLORIDA" & Year=="2009") %>%
  ggplot(aes(x=County, y=TotalPension)) +
  geom_bar(stat="identity") +
  ylab("Count") +
  ggtitle("Florida Veterans Receiving Pension Benefits in 2009\n(By County)") +
  theme.chart1

# Now lets look at the Total Compensation 
df.comp_070809 %>%
  filter(State == "FLORIDA" & Year=="2009") %>%
  ggplot(aes(x=County, y=TotalCompensation)) +
  geom_bar(stat="identity") +
  ylab("Count") +
  ggtitle("Florida Veterans Receiving Compensation Benefits in 2009\n(By County)") +
  theme.chart1

# Let's sort the data to get a better view of the counties with the largest compensation
df.comp_070809 %>%
  filter(State == "FLORIDA" & Year=="2009") %>%
  arrange(desc(TotalCompensation)) %>%
  head()

# The top six Florida counties are Hillsborough, Duval, Pinellas, Brevard, Orange, and Broward
# Let's zoom in to get some more details 

counties <- c("HILLSBOROUGH","DUVAL","PINELLAS","BREVARD","ORANGE","BROWARD")
legendLabels <- c("<30%", "30-50%", "60-90%", "100%")

# Let's look at the distribution of compensation
df.comp_070809 %>%
  filter(State == "FLORIDA" & Year=="2009" & County %in% counties) %>%
  select(County,CompBelow30,CompBetween30N50,CompBetween60N90,Comp100) %>%
  melt(id.var = "County") %>%
  ggplot(aes(x=County, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  labs(title="Florida Veterans Receiving Compensation Benefits in 2009", x="County",y="Count") +
  scale_fill_discrete(name="Percentages", labels = legendLabels) +
  theme.chart2

# Sort the data in ascending order to view the smallest compensation counties
df.comp_070809 %>%
  filter(State == "FLORIDA" & Year=="2009") %>%
  arrange(TotalCompensation) %>%
  head()

smCounties <- c("LAFAYETTE","LIBERTY","GLADES","FRANKLIN","HARDEE","CALHOUN")

# Let's look at the distribution of compensation
df.comp_070809 %>%
  filter(State == "FLORIDA" & Year=="2009" & County %in% smCounties) %>%
  select(County,CompBelow30,CompBetween30N50,CompBetween60N90,Comp100) %>%
  melt(id.var = "County") %>%
  ggplot(aes(x=County, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  labs(title="Florida Veterans Receiving Compensation Benefits in 2009", x="County",y="Count") +
  scale_fill_discrete(name="Percentages", labels = legendLabels) +
  theme.chart2

# Let's look at veterans who receive 100% compensation. How many of them are there in each state?
df.comp_070809 %>%
  filter(Year == "2009" & Comp100 != 0) %>%
  group_by(State) %>%
  summarize(total = sum(Comp100)) %>%
  ggplot(aes(x=State, y = total)) +
  geom_bar(stat="identity") +
  theme.chart1

# Let's look at Total CP in each state
df.comp_070809 %>%
  filter(Year == "2009") %>%
  group_by(State) %>%
  summarize(total = sum(TotalCP)) %>%
  ggplot(aes(x=State, y = total)) +
  geom_bar(stat="identity") +
  theme.chart1

# Zoom in to get age distribution of cP in Florida, California, and Texas
topStates <- c("CALIFORNIA","FLORIDA","TEXAS")

df.comp_070809 %>%
  filter(Year == "2009" & State %in% topStates) %>%
  group_by(State) %>%
  summarize(Below35 = sum(AgeBelow35),Between35N44 = sum(AgeBetween35N44),Between45N54 = sum(AgeBetween45N54), Between55N64= sum(AgeBetween55N64), Between65N74 = sum(AgeBetween65N74), Above75 = sum(AgeAbove75)) %>%
  melt(id.var = "State") %>%
  ggplot(aes(x=State, y = value, fill= variable)) +
  geom_bar(stat="identity") +
  theme.chart2

# Let's see how the number of veterans below 35 receiving CP changed between 2007 and 2009
df.veteransBelow35 <- df.comp_070809 %>%
                      group_by(Year) %>%
                      summarise(Below35 = sum(AgeBelow35))
df.veteransBelow35$Year <- as.integer(df.veteransBelow35$Year)

ggplot(data = df.veteransBelow35, aes(x = Year, y = Below35)) +
  geom_line(color = "#880011", size=2) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Below 35 Veterans Receiving CP") +
  theme.chart3