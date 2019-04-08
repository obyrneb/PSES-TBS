library(tidyverse)
library(broom)
library(networkD3)
library(plotly)

# LOAD DATA
#------------
mainDir <- getwd()
dataDir <- "datasets"
plotDir <- "plots"
ss1File_2018 <- file.path(mainDir,dataDir,"pses2018_ss1.csv")
ss2File_2018 <- file.path(mainDir,dataDir,"pses2018_ss2.csv")
ss3File_2018 <- file.path(mainDir,dataDir,"pses2018_ss3.csv")
ss4File_2018 <- file.path(mainDir,dataDir,"pses2018_ss4.csv")
ss5File_2018 <- file.path(mainDir,dataDir,"pses2018_ss5.csv")
ss1URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-1_Sous-ensemble-1.csv"
ss2URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-2_Sous-ensemble-2.csv"
ss3URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-3_Sous-ensemble-3.csv"
ss4URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-4_Sous-ensemble-4.csv"
ss5URL_2018 <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2018/2018_PSES_SAFF_Subset-5_Sous-ensemble-5.csv" # Due for release in March 2018

ifelse(!dir.exists(file.path(mainDir, dataDir)), dir.create(file.path(mainDir, dataDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, plotDir)), dir.create(file.path(mainDir, plotDir)), FALSE)

if(!file.exists(c(ss1File_2018,ss2File_2018,ss3File_2018,ss4File_2018,ss5File_2018))) {
  download.file(ss1URL_2018,ss1File_2018)
  download.file(ss2URL_2018,ss2File_2018)
  download.file(ss3URL_2018,ss3File_2018)
  download.file(ss4URL_2018,ss4File_2018)
  download.file(ss5URL_2018,ss5File_2018)
}

if(!exists("pses2018")) {
  ss1_2018 <- read.csv(ss1File_2018, na.strings = "9999")
  ss2_2018 <- read.csv(ss2File_2018, na.strings = "9999")
  ss3_2018 <- read.csv(ss3File_2018, na.strings = "9999")
  ss4_2018 <- read.csv(ss4File_2018, na.strings = "9999")
  ss5_2018 <- read.csv(ss5File_2018, na.strings = "9999")
  indicatorMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Indicator_Mapping.csv")) %>%
    select(-TITLE_E,-TITLE_F)
  pses2018 <- bind_rows(ss1_2018,ss2_2018,ss3_2018,ss4_2018,ss5_2018) %>%
    left_join(indicatorMap, by = "QUESTION")
}

demoMap <- read.csv(file.path(mainDir,dataDir,"PSES2018_Demographic_Mapping.csv"))
#------------

# CALCULATE CHI-SQUARES
#------------

TBSdata <- pses2018 %>%
  filter(LEVEL1ID == "26" & SURVEYR == 2018 & !endsWith(BYCOND, c("200","303","304","201","202","999"))) %>%
  mutate(unitcode = ifelse(startsWith(TBS.df$BYCOND,"LEVEL"), 
                                        word(TBS.df$BYCOND, 2, sep = " = "), 
                                        ifelse(TBS.df$BYCOND %in% c("TBS","PS"), TBS.df$BYCOND, NA))) %>%
  filter(!(unitcode %in% c("200","303","304","201","202","999"))) %>%
  mutate(DemoQ = ifelse(is.na(BYCOND),"TBS",
                        ifelse(startsWith(BYCOND, "LEVEL"),"org",
                        word(BYCOND, 1, sep = " =")))) %>%
  mutate(DKNA = (ifelse(is.na(ANSWER6),0,ANSWER6)+ifelse(is.na(ANSWER7),0,ANSWER7))) %>%
  mutate(notDKNA_n = (100-DKNA)/100*ANSCOUNT) %>%
  mutate(positive_n = round(POSITIVE/100*notDKNA_n),0) %>%
  mutate(neutral_n = round(NEUTRAL/100*notDKNA_n),0) %>%
  mutate(negative_n = round(NEGATIVE/100*notDKNA_n),0) %>%
  drop_na(positive_n,negative_n) %>%
  mutate(neutralNA = ifelse(is.na(neutral_n),TRUE,FALSE)) %>%
  #filter((positive_n + negative_n)>0) %>%
  #filter(neutralNA == FALSE) %>%
  select(QUESTION,DemoQ,BYCOND,#DESCRIP_E,DESCRIP_F,neutralNA
         positive_n,negative_n,neutral_n)

TBSdata$BYCOND[TBSdata$DemoQ == "TBS"] <- "TBS" 

TBSresiduals <- TBSdata %>%
  group_by(QUESTION,DemoQ) %>%
  gather("Sentiment","Freq",positive_n,negative_n,neutral_n) %>%
  do((function(x) augment(chisq.test(xtabs(Freq ~ Sentiment + BYCOND, data=.))))(.)) %>%
  ungroup()

TBSpvalues <- TBSdata %>%
  group_by(QUESTION,DemoQ) %>%
  gather("Sentiment","Freq",positive_n,negative_n,neutral_n) %>%
  do((function(x) tidy(chisq.test(xtabs(Freq ~ Sentiment + BYCOND, data=.))))(.)) %>%
  ungroup()

ChiSquares <- left_join(TBSresiduals,TBSpvalues, by=c("QUESTION","DemoQ")) %>%
  left_join(demoMap, by = "DemoQ") %>% 
  mutate(sentiment = recode(Sentiment, negative_n = "NEGATIVE", neutral_n = "NEUTRAL", positive_n = "POSITIVE"))

TBS_Xsq <- pses2018 %>%
  filter(LEVEL1ID == "26" & SURVEYR == 2018 & !endsWith(BYCOND, c("200","303","304","201","202","999"))) %>%
  gather("sentiment","prop",POSITIVE,NEUTRAL,NEGATIVE) %>%
  select(QUESTION,TITLE_E,BYCOND,DESCRIP_E,sentiment,prop,ANSCOUNT) %>%
  left_join(select(ChiSquares,QUESTION,DemoQ,DemoQ_E,BYCOND,p.value,sentiment,.stdres,.observed),
            by=c("QUESTION","BYCOND","sentiment"))

# CREATE INTERACTIVE GRAPHIC
#------------
balloon <- TBS_Xsq %>%
  filter(p.value <= 0.05 & 
           .stdres >= 2) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "First official language - French", "First OL: French")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "First official language - English", "First OL: English")) %>%
  mutate(descrip_e_cut = paste0(substr(DESCRIP_E,1,15),"...")) %>%
  arrange(BYCOND) %>%
  mutate(descrip_e_cut = factor(descrip_e_cut, levels = unique(descrip_e_cut))) %>%
  select(QUESTION,TITLE_E,DemoQ_E,BYCOND,DESCRIP_E,descrip_e_cut,p.value,sentiment,.stdres,.observed,ANSCOUNT)

#balloon[balloon == "First official language - French"] <- "First OL: French"
#balloon[balloon == "First official language - English"] <- "First OL: English"

bp <- ggplot(balloon,aes(x = descrip_e_cut, y = reorder(QUESTION,desc(QUESTION)), 
                         text = paste0(sentiment,"<br>",
                                       TITLE_E,"<br>",
                                       "Demographic: ", DemoQ_E, " - ", DESCRIP_E, " (n=",.observed,")","<br>",
                                       " (standardized residual = ",round(.stdres,2),", p-value = ", round(p.value,5),")"
                         ))) +
  geom_point(aes(size=.observed, colour=sentiment,alpha=(.observed/ANSCOUNT))) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "PSES@TBS 2018 - Sentiment by Question and Demographic",
       subtitle = "Results of chi-square tests on sentiment responses by demogrphic for every PSES question at TBS.\nEach dot represents a significant finding (p < 0.05, standardized residual > 2).\nColours denote sentiment, size correspond to standardized residuals, opaqueness relfects the p-value.",
       caption = "Data from the 2018 Public Service Employee Survey") + 
  theme_bw() +
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  #theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major = element_line(colour = "grey90")) +
  theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5, vjust = 0)) +
  theme(plot.subtitle    = element_text(hjust = 0.5))

bp <- ggplotly(bp, tooltip = c("text"))

htmlwidgets::saveWidget(as.widget(bp), file.path(mainDir,plotDir,"ChiSquare_BalloonChart.html"))