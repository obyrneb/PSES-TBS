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

if (!file.exists(c(ss1File_2018,ss2File_2018,ss3File_2018,ss4File_2018,ss5File_2018))) {
  download.file(ss1URL_2018,ss1File_2018)
  download.file(ss2URL_2018,ss2File_2018)
  download.file(ss3URL_2018,ss3File_2018)
  download.file(ss4URL_2018,ss4File_2018)
  download.file(ss5URL_2018,ss5File_2018)
}

if (!exists("pses2018")) {
  ss1_2018 <- read.csv(ss1File_2018, na.strings = "9999")
  ss2_2018 <- read.csv(ss2File_2018, na.strings = "9999")
  ss3_2018 <- read.csv(ss3File_2018, na.strings = "9999")
  ss4_2018 <- read.csv(ss4File_2018, na.strings = "9999")
  ss5_2018 <- read.csv(ss5File_2018, na.strings = "9999")
  indicatorMap <- read.csv(file.path(mainDir,dataDir,
                                     "PSES2018_Indicator_Mapping.csv")) %>%
    select(-TITLE_E,-TITLE_F)
  pses2018 <- bind_rows(ss1_2018,ss2_2018,ss3_2018,ss4_2018,ss5_2018) %>%
    left_join(indicatorMap, by = "QUESTION")
}

demoMap <- read.csv(file.path(mainDir,dataDir,
                              "PSES2018_Demographic_Mapping.csv"))
sectorAbbr <- read.csv(file.path(mainDir,dataDir,
                                 "PSES2018_TBS_Sector_Abbreviations.csv"))
#------------

# CALCULATE CHI-SQUARES
#------------

TBSdata <- pses2018 %>%
  filter(LEVEL1ID == "26" & SURVEYR == 2018) %>%
  mutate(unitcode = ifelse(startsWith(BYCOND,"LEVEL"), 
                                        word(BYCOND, 2, sep = " = "), 
                                        ifelse(BYCOND %in% c("TBS","PS"),
                                               TBS.df$BYCOND, NA))) %>%
  filter(!(unitcode %in% c("200","303","304","201","202","999"))) %>%
  mutate(DemoQ = ifelse(is.na(BYCOND),"TBS",
                        ifelse(startsWith(BYCOND, "LEVEL"),"org",
                        word(BYCOND, 1, sep = " =")))) %>%
  mutate(DKNA =       (ifelse(is.na(ANSWER6),0,ANSWER6) + 
                         ifelse(is.na(ANSWER7),0,ANSWER7))) %>%
  mutate(notDKNA_n =  (100 - DKNA) / 100 * ANSCOUNT) %>%
  mutate(positive_n = round(POSITIVE / 100 * notDKNA_n),0) %>%
  mutate(neutral_n =  round(NEUTRAL / 100 * notDKNA_n),0) %>%
  mutate(negative_n = round(NEGATIVE / 100 * notDKNA_n),0) %>%
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
  do((function(x) 
    augment(chisq.test(xtabs(Freq ~ Sentiment + BYCOND, data = .))))(.)) %>%
  ungroup()

TBSpvalues <- TBSdata %>%
  group_by(QUESTION,DemoQ) %>%
  gather("Sentiment","Freq",positive_n,negative_n,neutral_n) %>%
  do((function(x) 
    tidy(chisq.test(xtabs(Freq ~ Sentiment + BYCOND, data = .))))(.)) %>%
  ungroup()

ChiSquares <- left_join(TBSresiduals,TBSpvalues, by = c("QUESTION","DemoQ")) %>%
  left_join(demoMap, by = "DemoQ") %>% 
  mutate(sentiment = recode(Sentiment, 
                            negative_n = "NEGATIVE", 
                            neutral_n = "NEUTRAL", 
                            positive_n = "POSITIVE"))

TBS_Xsq <- pses2018 %>%
  filter(LEVEL1ID == "26" & SURVEYR == 2018 & 
           !endsWith(BYCOND, c("200","303","304","201","202","999"))) %>%
  gather("sentiment","prop",POSITIVE,NEUTRAL,NEGATIVE) %>%
  select(INDICATOR_E,QUESTION,TITLE_E,BYCOND,DESCRIP_E,
         sentiment,prop,ANSCOUNT) %>%
  left_join(select(ChiSquares,QUESTION,DemoQ,DemoQ_E,BYCOND,
                   p.value,sentiment,.stdres,.observed),
            by = c("QUESTION","BYCOND","sentiment"))


# Clean up some names
TBS_Xsq <- TBS_Xsq %>%
  # Official Languages
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "First official language - French", "First OL: French")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "First official language - English", "First OL: English")) %>%
  # Number of years in public service
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "Less than 3 years in the federal public service", "PS: <3 yrs")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "3 to 10 years in the federal public service", "PS: 3-10 yrs")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "11 to 20 years in the federal public service", "PS: 11-20 yrs")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "More than 20 years in the federal public service", "PS: >20 yrs")) %>%
  # Number of years in department
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "Less than 3 years in current department or agency", "Dept: <3 yrs")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "3 to 10 years in current department or agency", "Dept: 3-10 yrs")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "11 to 20 years in current department or agency", "Dept: 11-20 yrs")) %>%
  mutate(DESCRIP_E = replace(DESCRIP_E, DESCRIP_E == "More than 20 years in current department or agency", "Dept: >20 yrs"))

# CREATE INTERACTIVE GRAPHIC
#------------
balloon <- TBS_Xsq %>%
  filter(p.value <= 0.01 & 
           .stdres >= 2) %>%
  left_join(sectorAbbr, by = "DESCRIP_E") %>%
  mutate(DESCRIP_E = ifelse(startsWith(BYCOND,"LEVEL"),
                            as.character(abbr_E),DESCRIP_E)) %>%
  mutate(descrip_e_cut = paste0(substr(DESCRIP_E,1,15),"...")) %>%
  arrange(BYCOND) %>%
  mutate(descrip_e_cut = factor(descrip_e_cut, levels = unique(descrip_e_cut))) %>%
  mutate(QUESTION = fct_rev(QUESTION))
  select(INDICATOR_E,QUESTION,TITLE_E,DemoQ_E,BYCOND,DESCRIP_E,
         descrip_e_cut,p.value,sentiment,.stdres,.observed,ANSCOUNT,prop)

#balloon[balloon == "First official language - French"] <- "First OL: French"
#balloon[balloon == "First official language - English"] <- "First OL: English"

bp <- ggplot(balloon,aes(x = descrip_e_cut, y = QUESTION, 
                         text = paste0(sentiment," (n = ",.observed," / ",ANSCOUNT,", proportion = ",prop,"%)","<br>",
                                       TITLE_E,"<br>",
                                       "Demographic: ", DemoQ_E, " - ", DESCRIP_E, "<br>",
                                       " (standardized residual = ",round(.stdres,2),", p-value = ", round(p.value,5),")"
                         ))) +
  geom_point(aes(size = .observed, colour = sentiment,alpha = prop), shape = 18) +
  scale_colour_brewer(palette = "Set1") +
  #facet_grid(rows = vars(INDICATOR_E), cols = vars(DemoQ_E), switch = "both", scales = "free",space = "free") +
  labs(title = "PSES@TBS 2018 - Sentiment by Question and Demographic",
       subtitle = "Results of chi-square tests on sentiment responses by demogrphic for every PSES question at TBS.\nEach dot represents a significant finding (p < 0.05, standardized residual > 2).\nColours denote sentiment, size correspond to standardized residuals, opaqueness relfects the p-value.",
       caption = "Data from the 2018 Public Service Employee Survey") + 
  theme_bw() +
  theme(panel.spacing = unit(0,"mm")) +
  theme(panel.border     = element_blank()) +
  theme(strip.background = element_blank()) +
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
  theme(plot.title       = element_text(size = 14, face = "bold", hjust = 0.5, vjust = 0)) +
  theme(plot.subtitle    = element_text(hjust = 0.5))

bp_plotly <- ggplotly(bp, tooltip = c("text")) %>% 
  layout()

htmlwidgets::saveWidget(as_widget(bp_plotly), file.path(mainDir,plotDir,"ChiSquare_BalloonChart.html"))

# library(trelliscopejs)
# 
# trellis <- qplot(x = descrip_e_cut, y = QUESTION, data = balloon) +
#   theme_bw() +
#   facet_trelliscope( ~ DemoQ_E,
#     nrow = 2, ncol = 6, width = 300,
#     path = "C://Users//bobyrne//AppData//Local//Temp//2//Rtmps98VDs",
#     as_plotly = TRUE, 
#     plotly_args = list(dynamicTicks = T),
#     plotly_cfg = list(displayModeBar = F)
#   )
# 
# qplot(cty, hwy, data = mpg) +
#   geom_abline(alpha = 0.5) +
#   xlim(7, 37) + ylim(9, 47) + theme_bw() +
#   facet_trelliscope(~ manufacturer + class, nrow = 2, ncol = 4)