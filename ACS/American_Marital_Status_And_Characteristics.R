library(ggplot2)
library(dplyr)
library(reshape2)

# This file analyzes marriage and divorce data provided by the American Community
# Service url(http://www.census.gov/hhes/socdemo/marriage/data/acs/)

setwd("[R script directory]")

#Read Data
df.marital_status <- read.csv("ACS_13_5YR_MARITAL_STATUS.csv")

# Plot the Estimated Percentages for each State
legendLabels <- c("Married (Except Separated)", "Widowed", "Divorced", "Separated", "Never Married")
df.marital_status %>%
  select(Geography,Now_married_Except_Separated_15_Years_And_Over,
         Widowed_15_Years_And_Over,Divorced_Population_15_Years_And_Over,Separated_Population_15_Years_And_Over,
         Never_Married_Estimate_Population_15_Years_And_Over) %>%
  melt(id.var="Geography") %>%
  ggplot(aes(x=Geography,y=value,fill=variable)) +
  geom_bar(stat="identity",position="fill") +
  labs(title="Estimated US Marital Status Percentages Ages 15 and Over", x="State", y="Percentage") +
  scale_fill_discrete(name="Marital Status", labels=legendLabels) +
  coord_flip()

# Washington DC Stands out in the above chart, having the highest percentage of
# single Americans. Let's take a closer look at this region. Let's first look
# at the ages of the people living in DC

df.marital_status %>%
  filter(Geography=="District of Columbia") %>%
  mutate(Ages15To19=Total_Males_15_Years_And_Over_15_To_19_Years+Total_Females_15_To_19_Years,
         Ages20To34=Total_Males_20_To_34_Years+Total_Females_20_To_34_Years,
         Ages35To44=Total_Males_35_To_44_Years+Total_Females_35_To_44_Years,
         Ages45To54=Total_Males_45_To_54_Years+Total_Females_45_To_54_Years,
         Ages55To64=Total_Males_55_To_64_Years+Total_Females_55_To_64_Years,
         Ages65AndOver=Total_Males_65_Years_And_Over+Total_Females_65_Years_And_Over) %>%
  select(Geography,Ages15To19,Ages20To34,Ages35To44,Ages45To54,Ages55To64,Ages65AndOver) %>%
  melt(id.var="Geography") %>%
  ggplot(aes(x=variable,y=value)) +
  geom_bar(stat="identity") +
  labs(title="Estimated Washington DC Population (By Age)", x="Age Group", y="Count")

# Interesting, the majority of people living in DC appears to be between 20 and 35. 
# Let's take a look at the marital status of each group to see if we can get a clearer picture

# First let's create some functions we need to preprocess the data

computePopulation <- function(totalMales,percentMales,totalFemales,percentFemales) {
  return (totalMales*percentMales)+(totalFemales*percentFemales)
}

computeAgeGroup <- function(category) {
  ifelse(category == "Married15To19","Ages15To19",
   ifelse(category =="Widowed15To19","Ages15To19",
    ifelse(category == "Divorced15To19","Ages15To19",
     ifelse(category == "Separated15To19","Ages15To19",
      ifelse(category == "NeverMarried15To19","Ages15To19",
       ifelse(category == "Married20To34","Ages20To34",
        ifelse(category == "Widowed20To34","Ages20To34",
         ifelse(category == "Divorced20To34","Ages20To34",
          ifelse(category =="Separated20To34","Ages20To34",
           ifelse(category == "NeverMarried20To34","Ages20To34",
            ifelse(category == "Married35To44", "Ages35To44",
             ifelse(category == "Widowed35To44","Ages35To44",
              ifelse(category =="Divorced35To44", "Ages35To44",
               ifelse(category == "Separated35To44","Ages35To44",
                ifelse(category == "NeverMarried35To44","Ages35To44",
                 ifelse(category == "Married45To54","Ages45To54",
                  ifelse(category == "Widowed45To54","Ages45To54",
                   ifelse(category == "Divorced45To54","Ages45To54",
                    ifelse(category == "Separated45To54","Ages45To54",
                     ifelse(category == "NeverMarried45To54","Ages45To54",
                      ifelse(category =="Married55To64","Ages55To64",
                       ifelse(category=="Widowed55To64","Ages55To64",
                        ifelse(category=="Divorced55To64","Ages55To64",
                         ifelse(category=="Separated55To64","Ages55To64",
                          ifelse(category=="NeverMarried55To64","Ages55To64",
                           ifelse(category=="Married65AndOver","Ages65AndOver",
                            ifelse(category=="Widowed65AndOver","Ages65AndOver",
                             ifelse(category=="Divorced65AndOver","Ages65AndOver",
                              ifelse(category=="Separated65AndOver","Ages65AndOver",
                               ifelse(category=="NeverMarried65AndOver","Ages65AndOver",NA)
                              )))))))))))))))))))))))))))))
}

computeMaritalStatus <- function(category) {
  ifelse(category == "Married15To19","Married (Except Separated)",
         ifelse(category =="Widowed15To19","Widowed",
                ifelse(category == "Divorced15To19","Divorced",
                       ifelse(category == "Separated15To19","Separated",
                              ifelse(category == "NeverMarried15To19","Never Married",
                                     ifelse(category == "Married20To34","Married (Except Separated)",
                                            ifelse(category == "Widowed20To34","Widowed",
                                                   ifelse(category == "Divorced20To34","Divorced",
                                                          ifelse(category =="Separated20To34","Separated",
                                                                 ifelse(category == "NeverMarried20To34","Never Married",
                                                                        ifelse(category == "Married35To44", "Married (Except Separated)",
                                                                               ifelse(category == "Widowed35To44","Widowed",
                                                                                      ifelse(category =="Divorced35To44", "Divorced",
                                                                                             ifelse(category == "Separated35To44","Separated",
                                                                                                    ifelse(category == "NeverMarried35To44","Never Married",
                                                                                                           ifelse(category == "Married45To54","Married",
                                                                                                                  ifelse(category == "Widowed45To54","Widowed",
                                                                                                                         ifelse(category == "Divorced45To54","Divorced",
                                                                                                                                ifelse(category == "Separated45To54","Separated",
                                                                                                                                       ifelse(category == "NeverMarried45To54","Never Married",
                                                                                                                                              ifelse(category =="Married55To64","Married",
                                                                                                                                                     ifelse(category=="Widowed55To64","Widowed",
                                                                                                                                                            ifelse(category=="Divorced55To64","Divorced",
                                                                                                                                                                   ifelse(category=="Separated55To64","Separated",
                                                                                                                                                                          ifelse(category=="NeverMarried55To64","Never Married",
                                                                                                                                                                                 ifelse(category=="Married65AndOver","Married (Except Separated)",
                                                                                                                                                                                        ifelse(category=="Widowed65AndOver","Widowed",
                                                                                                                                                                                               ifelse(category=="Divorced65AndOver","Divorced",
                                                                                                                                                                                                      ifelse(category=="Separated65AndOver","Separated",
                                                                                                                                                                                                             ifelse(category=="NeverMarried65AndOver","Never Married",NA)
                                                                                                                                                                                                      )))))))))))))))))))))))))))))
}

df.marital_status %>%
  filter(Geography=="District of Columbia") %>%
  mutate(Married15To19 = computePopulation(Total_Males_15_Years_And_Over_15_To_19_Years,
                                           Now_Married_Except_Separated_Males_15_To_19_Years,
                                           Total_Females_15_To_19_Years,
                                           Now_Married_Except_Separated_Females_15_To_19_Years),
         Widowed15To19 = computePopulation(Total_Males_15_Years_And_Over_15_To_19_Years,
                                           Widowed_Males_15_To_19_Years,
                                           Total_Females_15_To_19_Years,
                                           Widowed_Females_15_To_19_Years),
         Divorced15To19 = computePopulation(Total_Males_15_Years_And_Over_15_To_19_Years,
                                            Divorced_Males_15_To_19_Years,
                                            Total_Females_15_To_19_Years,
                                            Divorced_Females_15_To_19_Years),
         Separated15To19 = computePopulation(Total_Males_15_Years_And_Over_15_To_19_Years,
                                             Separated_Males_15_To_19_Years,
                                             Total_Females_15_To_19_Years,
                                             Separated_Females_15_To_19_Years),
         NeverMarried15To19 = computePopulation(Total_Males_15_Years_And_Over_15_To_19_Years,
                                                Never_Married_Males_15_To_19_Years,
                                                Total_Females_15_To_19_Years,
                                                Never_Married_Females_15_To_19_Years),
         Married20To34 = computePopulation(Total_Males_20_To_34_Years,
                                           Now_Married_Except_Separated_Males_20_To_34_Years,
                                           Total_Females_20_To_34_Years,
                                           Now_Married_Except_Separated_Females_20_To_34_Years),
         Widowed20To34 = computePopulation(Total_Males_20_To_34_Years,
                                           Widowed_Males_20_To_34_Years,
                                           Total_Females_20_To_34_Years,
                                           Widowed_Females_20_To_34_Years),
         Divorced20To34 = computePopulation(Total_Males_20_To_34_Years,
                                            Divorced_Males_20_To_34_Years,
                                            Total_Females_20_To_34_Years,
                                            Divorced_Females_20_To_34_Years),
         Separated20To34 = computePopulation(Total_Males_20_To_34_Years,
                                             Separated_Males_20_To_34_Years,
                                             Total_Females_20_To_34_Years,
                                             Separated_Females_20_To_34_Years),
         NeverMarried20To34 = computePopulation(Total_Males_20_To_34_Years,
                                                Never_Married_Males_20_To_34_Years,
                                                Total_Females_20_To_34_Years,
                                                Never_Married_Females_20_To_34_Years),
         Married35To44 = computePopulation(Total_Males_35_To_44_Years,
                                           Now_Married_Males_35_To_44_Years,
                                           Total_Females_35_To_44_Years,
                                           Now_Married_Except_Separated_Females_35_To_44_Years),
         Widowed35To44 = computePopulation(Total_Males_35_To_44_Years,
                                           Widowed_Males_35_To_44_Years,
                                           Total_Females_35_To_44_Years,
                                           Widowed_Females_35_To_44_Years),
         Divorced35To44 = computePopulation(Total_Males_35_To_44_Years,
                                            Divorced_Males_35_To_44_Years,
                                            Total_Females_35_To_44_Years,
                                            Divorced_Females_35_To_44_Years),
         Separated35To44 = computePopulation(Total_Males_35_To_44_Years,
                                             Separated_Males_35_To_44_Years,
                                             Total_Females_35_To_44_Years,
                                             Separated_Females_35_To_44_Years),
         NeverMarried35To44 = computePopulation(Total_Males_35_To_44_Years,
                                                Never_Married_Males_35_To_44_Years,
                                                Total_Females_35_To_44_Years,
                                                Never_Married_Females_35_To_44_Years),
         Married45To54 = computePopulation(Total_Males_45_To_54_Years,
                                           Now_Married_Except_Separated_Males_15_Years_And_Over_45_To_54_Years,
                                           Total_Females_45_To_54_Years,
                                           Now_Married_Except_Separated_Females_45_To_54_Years),
         Widowed45To54 = computePopulation(Total_Males_45_To_54_Years,
                                           Widowed_Males_45_To_54_Years,
                                           Total_Females_45_To_54_Years,
                                           Widowed_Females_45_To_54_Years),
         Divorced45To54 = computePopulation(Total_Males_45_To_54_Years,
                                            Divorced_Males_45_To_54_Years,
                                            Total_Females_45_To_54_Years,
                                            Divorced_Females_45_To_54_Years),
         Separated45To54 = computePopulation(Total_Males_45_To_54_Years,
                                             Separated_Males_45_To_54_Years,
                                             Total_Females_45_To_54_Years,
                                             Separated_Females_45_To_54_Years),
         NeverMarried45To54 = computePopulation(Total_Males_45_To_54_Years,
                                                Never_Married_Males_45_To_54_Years,
                                                Total_Females_45_To_54_Years,
                                                Never_Married_Females_45_To_54_Years),
         Married55To64 = computePopulation(Total_Males_55_To_64_Years,
                                           Now_Married_Except_Separated_Males_55_To_64_Years,
                                           Total_Females_55_To_64_Years,
                                           Now_Married_Except_Separated_Females_55_To_64_Years),
         Widowed55To64 = computePopulation(Total_Males_55_To_64_Years,
                                           Widowed_Males_55_To_64_Years,
                                           Total_Females_55_To_64_Years,
                                           Widowed_Females_55_To_64_Years),
         Divorced55To64 = computePopulation(Total_Males_55_To_64_Years,
                                            Divorced_Males_55_To_64_Years,
                                            Total_Females_55_To_64_Years,
                                            Divorced_Females_55_To_64_Years),
         Separated55To64 = computePopulation(Total_Males_55_To_64_Years,
                                             Separated_Males_55_To_64_Years,
                                             Total_Females_55_To_64_Years,
                                             Separated_Females_55_To_64_Years),
         NeverMarried55To64 = computePopulation(Total_Males_55_To_64_Years,
                                                Never_Married_Males_55_To_64_Years,
                                                Total_Females_55_To_64_Years,
                                                Never_Married_Females_55_To_64_Years),
         Married65AndOver = computePopulation(Total_Males_65_Years_And_Over,
                                              Now_Married_Except_Separated_Males_65_Years_And_Over,
                                           Total_Females_65_Years_And_Over,
                                           Now_Married_Except_Separated_Females_65_Years_And_Over),
         Widowed65AndOver = computePopulation(Total_Males_65_Years_And_Over,
                                           Widowed_Males_65_Years_And_Over,
                                           Total_Females_65_Years_And_Over,
                                           Widowed_Females_65_Years_And_Over),
         Divorced65AndOver = computePopulation(Total_Males_65_Years_And_Over,
                                            Divorced_Males_65_Years_And_Over,
                                            Total_Females_65_Years_And_Over,
                                            Divorced_Females_65_Years_And_Over),
         Separated65AndOver = computePopulation(Total_Males_65_Years_And_Over,
                                             Separated_Males_65_Years_And_Over,
                                             Total_Females_65_Years_And_Over,
                                             Separated_Females_65_Years_And_Over),
         NeverMarried65AndOver = computePopulation(Total_Males_65_Years_And_Over,
                                                Never_Married_65_Years_And_Over,
                                                Total_Females_65_Years_And_Over,
                                                Never_Married_Females_65_Years_And_Over)
         ) %>%
  select(Geography,Married15To19,Widowed15To19,Divorced15To19,Separated15To19,NeverMarried15To19,
         Married20To34,Widowed20To34,Divorced20To34,Separated20To34,NeverMarried20To34,
         Married35To44,Widowed35To44,Divorced35To44,Separated35To44,NeverMarried35To44,
         Married45To54,Widowed45To54,Divorced45To54,Separated45To54,NeverMarried45To54,
         Married55To64,Widowed55To64,Divorced55To64,Separated55To64,NeverMarried55To64,
         Married65AndOver,Widowed65AndOver,Divorced65AndOver,Separated65AndOver,NeverMarried65AndOver) %>%
  melt(id.vars="Geography", variable.name="Category",value.name="Count") %>%
  mutate(AgeGroup = computeAgeGroup(Category), MaritalStatus = computeMaritalStatus(Category)) %>%
  select(AgeGroup, MaritalStatus, Count) %>%
  ggplot(aes(x=AgeGroup,y=Count,fill=MaritalStatus)) +
  ggtitle("Est DC Pop By Age Group and Marital Status") +
  geom_bar(stat="identity")

# Let's compare the age distribution in DC with the rest of the country
df.marital_status %>%
  mutate(Ages15To19=Total_Males_15_Years_And_Over_15_To_19_Years+Total_Females_15_To_19_Years,
         Ages20To34=Total_Males_20_To_34_Years+Total_Females_20_To_34_Years,
         Ages35To44=Total_Males_35_To_44_Years+Total_Females_35_To_44_Years,
         Ages45To54=Total_Males_45_To_54_Years+Total_Females_45_To_54_Years,
         Ages55To64=Total_Males_55_To_64_Years+Total_Females_55_To_64_Years,
         Ages65AndOver=Total_Males_65_Years_And_Over+Total_Females_65_Years_And_Over) %>%
  select(Geography,Ages15To19,Ages20To34,Ages35To44,Ages45To54,Ages55To64,Ages65AndOver) %>%
  melt(id.var="Geography") %>%
  ggplot(aes(x=Geography,y=value, fill=variable)) +
  geom_bar(stat="identity", position="Fill") +
  labs(title="Estimated US Population (By Age)", x="Age Group", y="Count") +
  coord_flip()

# After doing some research, it appears that Washington DC does in fact have a
# large population of singles, a large majority of which are young, ambitious
# professionals between ages 20 - 39. Let's look at the ratio of unmarried males
# to unmarried females and vice versa

# Male to Female Ratio
df.marital_status %>%
  mutate(SingleMaleToFemale = (Total_Males_15_Years_And_Over*Never_Married_Males_15_Years_And_Over)/
                              (Total_Females_15_Years_And_Over*Never_Married_Females_15_Years_And_Over)) %>%
  select(Geography, SingleMaleToFemale) %>%
  arrange(SingleMaleToFemale)

# Female to Male Ratio
df.marital_status %>%
  mutate(SingleFemaleToMale = (Total_Females_15_Years_And_Over*Never_Married_Females_15_Years_And_Over)/
                              (Total_Males_15_Years_And_Over*Never_Married_Males_15_Years_And_Over)) %>%
  select(Geography, SingleFemaleToMale) %>%
  arrange(SingleFemaleToMale)

# Washington DC has the largest male to female ratio (more females per male) in the US
# Looks like I found the perfect place to find a girlfriend. Lol!

# Let's take a look at the marital status among different ethic groups.

# Married americans by ethinicity

df.marital_status %>%
  mutate(MarriedWhite = (Now_Married_Except_Separated_White*Total_Population_White)/Total_Population_15_Years_And_Over,
         MarriedBlack = (Now_Married_Except_Separated_Black*Total_Population_Black)/Total_Population_15_Years_And_Over,
         MarriedNative = (Now_Married_Except_Separated_American_Indian_And_Alaska_Native*Total_Population_American_Indian_And_Alaska_Native)/Total_Population_15_Years_And_Over,
         MarriedAsian = (Now_Married_Except_Separated_Asian*Total_Population_Asian)/Total_Population_15_Years_And_Over,
         MarriedPacific = (Now_Married_Except_Separated_Native_Hawaiian_And_Other_Pacific_Islander*Total_Population_Native_Hawaiian_And_Other_Pacific_Islander)/Total_Population_15_Years_And_Over,
         MarriedHispanic = (Now_Married_Except_Separated_Hispanic_Or_Latino_Origin*Total_Hispanic_Or_Latino_Origin)/Total_Population_15_Years_And_Over,
         MarriedOther = (Now_Married_Except_Separated_Some_Other_Race*Total_Population_Some_Other_Race)/Total_Population_15_Years_And_Over) %>%
  select(Geography,MarriedWhite,MarriedBlack,MarriedNative,MarriedAsian,MarriedPacific,MarriedHispanic,MarriedOther) %>%
  melt(id.var="Geography", variable.name="Ethnicity", value.name="MarriageRate") %>%
  ggplot(aes(x=Geography, y=MarriageRate, fill=Ethnicity)) +
  geom_bar(stat="identity", position="fill") +
  labs(title="American Marriage Rates by Ethnicity", x="State", y="Marriage Rate")+
  coord_flip()
  
 # Divorced Americans By Ethicity 
df.marital_status %>%
  mutate(DivorcedWhite = (Divorced_White*Total_Population_White)/Total_Population_15_Years_And_Over,
         DivorcedBlack = (Divorced_Black*Total_Population_Black)/Total_Population_15_Years_And_Over,
         DivorcedNative = (Divorced_American_Indian_And_Alaska_Native*Total_Population_American_Indian_And_Alaska_Native)/Total_Population_15_Years_And_Over,
         DivorcedAsian = (Divorced_Asian*Total_Population_Asian)/Total_Population_15_Years_And_Over,
         DivorcedPacific = (Divorced_Native_Hawaiian_And_Other_Pacific_Islander*Total_Population_Native_Hawaiian_And_Other_Pacific_Islander)/Total_Population_15_Years_And_Over,
         DivorcedHispanic = (Divorced_Hispanic_Or_Latino_Origin*Total_Hispanic_Or_Latino_Origin)/Total_Population_15_Years_And_Over,
         DivorcedOther = (Divorced_Some_Other_Race*Total_Population_Some_Other_Race)/Total_Population_15_Years_And_Over) %>%
  select(Geography,DivorcedWhite,DivorcedBlack,DivorcedNative,DivorcedAsian,DivorcedPacific,DivorcedHispanic,DivorcedOther) %>%
  melt(id.var="Geography", variable.name="Ethnicity", value.name="DivorceRate") %>%
  ggplot(aes(x=Geography, y=DivorceRate, fill=Ethnicity)) +
  geom_bar(stat="identity", position="fill") +
  labs(title="American Divorce Rates by Ethnicity", x="State", y="Divorce Rate")+
  coord_flip()

# Widowed Americans By Ethnicity
df.marital_status %>%
  mutate(WidowedWhite = (Widowed_White*Total_Population_White)/Total_Population_15_Years_And_Over,
         WidowedBlack = (Widowed_Black*Total_Population_Black)/Total_Population_15_Years_And_Over,
         WidowedNative = (Widowed_American_Indian_And_Alaska_Native*Total_Population_American_Indian_And_Alaska_Native)/Total_Population_15_Years_And_Over,
         WidowedAsian = (Widowed_Asian*Total_Population_Asian)/Total_Population_15_Years_And_Over,
         WidowedPacific = (Widowed_Native_Hawaiian_And_Other_Pacific_Islander*Total_Population_Native_Hawaiian_And_Other_Pacific_Islander)/Total_Population_15_Years_And_Over,
         WidowedHispanic = (Widowed_Estimate_Hispanic_Or_Latino_Origin*Total_Hispanic_Or_Latino_Origin)/Total_Population_15_Years_And_Over,
         WidowedOther = (Widowed_Some_Other_Race*Total_Population_Some_Other_Race)/Total_Population_15_Years_And_Over) %>%
  select(Geography,WidowedWhite,WidowedBlack,WidowedNative,WidowedAsian,WidowedPacific,WidowedHispanic,WidowedOther) %>%
  melt(id.var="Geography", variable.name="Ethnicity", value.name="WidowRate") %>%
  ggplot(aes(x=Geography, y=WidowRate, fill=Ethnicity)) +
  geom_bar(stat="identity", position="fill") +
  labs(title="American Widow Rates by Ethnicity", x="State", y="Widow Rate")+
  coord_flip()

# Separated Americans By Ethnicity
df.marital_status %>%
  mutate(SeparatedWhite = (Separated_White*Total_Population_White)/Total_Population_15_Years_And_Over,
         SeparatedBlack = (Separated_Black*Total_Population_Black)/Total_Population_15_Years_And_Over,
         SeparatedNative = (Separated_American_Indian_And_Alaska_Native*Total_Population_American_Indian_And_Alaska_Native)/Total_Population_15_Years_And_Over,
         SeparatedAsian = (Separated_Asian*Total_Population_Asian)/Total_Population_15_Years_And_Over,
         SeparatedPacific = (Separated_Native_Hawaiian_And_Other_Pacific_Islander*Total_Population_Native_Hawaiian_And_Other_Pacific_Islander)/Total_Population_15_Years_And_Over,
         SeparatedHispanic = (Separated_Hispanic_Or_Latino_Origin*Total_Hispanic_Or_Latino_Origin)/Total_Population_15_Years_And_Over,
         SeparatedOther = (Separated_One_Race_Some_Other_Race*Total_Population_Some_Other_Race)/Total_Population_15_Years_And_Over) %>%
  select(Geography,SeparatedWhite,SeparatedBlack,SeparatedNative,SeparatedAsian,SeparatedPacific,SeparatedHispanic,SeparatedOther) %>%
  melt(id.var="Geography", variable.name="Ethnicity", value.name="SeparationRate") %>%
  ggplot(aes(x=Geography, y=SeparationRate, fill=Ethnicity)) +
  geom_bar(stat="identity", position="fill") +
  labs(title="American Separation Rates by Ethnicity", x="State", y="Separation Rate")+
  coord_flip()

# Single Americans By Ethnicity
df.marital_status %>%
  mutate(SingleWhite = (Never_Married_White*Total_Population_White)/Total_Population_15_Years_And_Over,
         SingleBlack = (Never_Married_Black*Total_Population_Black)/Total_Population_15_Years_And_Over,
         SingleNative = (Never_Married_American_Indian_And_Alaska_Native*Total_Population_American_Indian_And_Alaska_Native)/Total_Population_15_Years_And_Over,
         SingleAsian = (Never_Married_Asian*Total_Population_Asian)/Total_Population_15_Years_And_Over,
         SinglePacific = (Never_Married_Native_Hawaiian_And_Other_Pacific_Islander*Total_Population_Native_Hawaiian_And_Other_Pacific_Islander)/Total_Population_15_Years_And_Over,
         SingleHispanic = (Never_Married_Hispanic_Or_Latino_Origin*Total_Hispanic_Or_Latino_Origin)/Total_Population_15_Years_And_Over,
         SingleOther = (Never_Married_Some_Other_Race*Total_Population_Some_Other_Race)/Total_Population_15_Years_And_Over) %>%
  select(Geography,SingleWhite,SingleBlack,SingleNative,SingleAsian,SinglePacific,SingleHispanic,SingleOther) %>%
  melt(id.var="Geography", variable.name="Ethnicity", value.name="SingleRate") %>%
  ggplot(aes(x=Geography, y=SingleRate, fill=Ethnicity)) +
  geom_bar(stat="identity", position="fill") +
  labs(title="Single Americans by Ethnicity", x="State", y="% Single")+
  coord_flip()

# Very high divorce and widow rate for blacks in Washington DC. I wonder why.
# High divorce rate and widow rate for Asians in Hawaii. I wonder why.

