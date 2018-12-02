library(dplyr)
library(tidyr)
library(ggplot2)

myData <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings=9999) %>%
  filter(LEVEL1ID == 26 & SURVEYR == 2017) %>%
  select(QUESTION,DESCRIP_E,POSITIVE,NEUTRAL,NEGATIVE)

mySummary <- myData %>%
  gather(key="sentiment",value="prop",-DESCRIP_E,-QUESTION) %>%
  mutate(QSECTION = substr(QUESTION,1,1)) %>%
  group_by(DESCRIP_E,QSECTION,sentiment) %>%
  summarise(prop = mean(prop))
  
myPlot <- ggplot(data=mySummary, aes(x=sentiment, y=prop)) +
  geom_col(aes(fill=sentiment)) +
  facet_grid(QSECTION ~ DESCRIP_E) +
  theme_minimal()

ggsave(plot=myPlot,"test.pdf")
