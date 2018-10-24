library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)

#Read all PSES 2017 Subsets
ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv", na.strings = "9999")
ss2 <- read.csv("datasets//2017_PSES_SAFF_Subset-2_Sous-ensemble-2.csv", na.strings = "9999")
ss3 <- read.csv("datasets//2017_PSES_SAFF_Subset-3_Sous-ensemble-3.csv", na.strings = "9999")
ss4 <- read.csv("datasets//2017_PSES_SAFF_Subset-4_Sous-ensemble-4.csv", na.strings = "9999")
ss5 <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")

#Combine subsets as appropriate
ss1_5 <- rbind(ss1,ss2)
ss1_5 <- rbind(ss1_5,ss3)
#ss1_5 <- rbind(ss1_5,ss4)
#ss1_5 <- rbind(ss1_5,ss5)

#Extract TBS data only
ss.df <- subset(ss1_5, (LEVEL1ID == "26" & LEVEL2ID == "0"),
                select=c(LEVEL1ID,SURVEYR,BYCOND,DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,POSITIVE,NEUTRAL,NEGATIVE,SCORE100,ANSCOUNT))

#Create question subsections
ss.df$QSection <- substr(ss.df$QUESTION,0,1)
ss.df$QSection <- factor(ss.df$QSection)
ss.df$BYCOND[is.na(ss.df$BYCOND)] <- "TBS" 
ss.df$DemoQ <- word(ss.df$BYCOND, 1, sep = " =")

#Filter data for only those questions with 3 years or more
SYFilterQ.df <- as.data.frame(aggregate(data = ss.df, SURVEYR ~ QUESTION, function(x) length(unique(x))))
SYFilterQ.df <- subset(SYFilterQ.df, SYFilterQ.df$SURVEYR >= 3)
FilteredQs <- unique(SYFilter.df$QUESTION)
ss.df <- subset(ss.df, QUESTION %in% FilteredQs)

#Filter data for only those demographics collected over three years or more
SYFilterDemQ.df <- as.data.frame(aggregate(data = ss.df, SURVEYR ~ BYCOND, function(x) length(unique(x))))
SYFilterDemQ.df <- subset(SYFilterDemQ.df, SYFilterDemQ.df$SURVEYR >= 3)
FilteredDemQs <- unique(SYFilterDemQ.df$BYCOND)
ss.df <- subset(ss.df, BYCOND %in% FilteredDemQs & 
                  !(DemoQ %in% c("M_Q108","M_Q109","M_Q110","M_Q111")))


#Aggregate by demographic and question subsection
demoMeans.df <- aggregate(data = ss.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ LEVEL1ID + QSection + QUESTION + TITLE_E + TITLE_F + DemoQ + BYCOND + DESCRIP_E + DESCRIP_F + SURVEYR, mean)
#toplineMeans.df <- aggregate(data = ss.df, cbind(ANSCOUNT,SCORE100,NEGATIVE,NEUTRAL,POSITIVE) ~ LEVEL1ID + QSection + BYCOND + DESCRIP_E + DESCRIP_F + SURVEYR, mean) 
#demoMeans.df <- rbind(demoMeans.df,toplineMeans.df)
#demoMeans.df <- demoMeans.df %>% arrange(DemoQ,BYCOND)
demoMeans.df <- melt(demoMeans.df, measure.vars = c("NEGATIVE","NEUTRAL","POSITIVE"))
demoMeans.df <- demoMeans.df %>% 
  arrange(DemoQ,BYCOND) %>%
  mutate(DESCRIP_E = factor(DESCRIP_E, unique(DESCRIP_E))) %>%
  mutate(DESCRIP_F = factor(DESCRIP_F, unique(DESCRIP_F)))

#Make TBS the first level
demoMeans.df$DESCRIP_E <- relevel(demoMeans.df$DESCRIP_E, "Treasury Board of Canada Secretariat")
demoMeans.df$DESCRIP_F <- relevel(demoMeans.df$DESCRIP_F, "Secrétariat du Conseil du Trésor du Canada")

#English captions.
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question category and TBS sector. The bars over each column represent the average value for this question category across sectors."
expl_E <- paste0(strwrap(expl_E, 140), sep="", collapse="\n")
ttl_E <- "PSES 2017 - TBS Demographic Results by Question Category"
cap_E <- "2017 Public Service Employee Survey Open Datasets"
demoMeans.df$QSection_E <- mapvalues(demoMeans.df$QSection, c("A","B","C","D","E","F","G","H","I","J","K","L"),
                             c("My Job (Q1-Q22)",
                               "My Work Unit (Q23-Q29)",
                               "My Immediate Supervisor (Q30-Q38)",
                               "Senior Management (Q39-Q44)",
                               "My Organization (Q45-Q60)",
                               "Mobility and Retention (Q61-Q62)",
                               "Harassment (Q63-Q69)",
                               "Labour Relations and Collective Agreements (Q71-Q74)",
                               "Discrimination (Q75-Q82)",
                               "Stress and well being (Q83-Q87)",
                               "Duty to Accommodate (Q88-Q90)",
                               "Compensation (Q91-Q95)"))
demoMeans.df$variable_E <- mapvalues(demoMeans.df$variable, 
                           c("NEGATIVE","NEUTRAL","POSITIVE"),
                           c("negative","neutral","positive"))
TBSmeans_E.df <- aggregate(data = demoMeans.df, value ~ TITLE_E + variable_E, mean)
PNN_E.clrs <- c("negative" = "#CD202C", "neutral" = "#63CECA", "positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat" = "#d1e7ee")

#French Captions
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour une catégorie de questions particulière et un secteur du SCT. Les barres au dessus de chaque colonne représentent la valeur moyenne de cette catégorie de question pour tous les secteurs."
expl_F <- paste0(strwrap(expl_F, 140), sep="", collapse="\n")
ttl_F <- "SAFF 2017 - Résultats démographiques du SCT par catégorie de question" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2017"
demoMeans.df$QSection_F <- mapvalues(demoMeans.df$QSection, c("A","B","C","D","E","F","G","H","I","J","K","L"),
                             c("Mon Travail (Q1-Q22)",
                               "Mon unité de travail (Q23-Q29)",
                               "Mon (ma) superviseur(e) immédiat(e) (Q30-Q38)",
                               "La haute direction (Q39-Q44)",
                               "Mon organisation (Q45-Q60)",
                               "Mobilité et maintien en poste (Q61-Q62)",
                               "Harcèlement (Q63-Q69)",
                               "Relations patronales-syndicales et conventions collectives (Q71-Q74)",
                               "Discrimination (Q75-Q82)",
                               "Stress et bien-être (Q83-Q87)",
                               "Obligation de prendre des mesures d'adaptation (Q88-Q90)",
                               "Rémunération (Q91-Q95)"))
demoMeans.df$variable_F <- mapvalues(demoMeans.df$variable, 
                           c("NEGATIVE","NEUTRAL","POSITIVE"),
                           c("négatif","neutre","positif"))
PNNmeans_F.df <- aggregate(data = demoMeans.df, value ~ QSection_F + variable_F, mean)
PNN_F.clrs <- c("négatif" = "#CD202C", "neutre" = "#63CECA", "positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada" = "#d1e7ee")

#EN plot
pdf("PSES 2017 @ TBS - Question Categories by Demographic.pdf", width = 14, height = 10)
ggplot(data=demoMeans.df, aes(x=SURVEYR,y=value, fill = variable_E)) +
  geom_rect(data = subset(demoMeans.df,DESCRIP_E == "Treasury Board of Canada Secretariat"),
            aes(fill = DESCRIP_E),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_area(stat = "identity") +
  #scale_y_continuous(labels = scales::percent) +
  ggtitle(ttl_E) +
  labs(fill = "Response type", 
       subtitle = expl_E,
       caption = cap_E) +
  scale_fill_manual(values = PNN_E.clrs, labels = c("Negative", "Neutral", "Positive", "TBS")) +
  #geom_errorbar(data = PNNmeans_E.df, aes(ymax=value, ymin=value), colour = "grey45") +
  #scale_colour_manual(values = PNNmeans_E.clrs) +
  facet_grid(TITLE_E ~ DESCRIP_E, switch = "y", scales = "free_y",
             labeller = labeller(DESCRIP_E = label_wrap_gen(20), TITLE_E = label_wrap_gen(120)))  +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
        plot.subtitle = element_text(face = "bold", size = 8, colour = "grey40"),
        plot.caption = element_text(face = "italic", size = 8, colour = "grey45"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "mm"),
        panel.spacing.x = unit(0.5, "mm"),
        strip.text.y = element_text(angle = 180, size = 6, colour = "grey40", hjust = 0, inherit.blank = FALSE),
        strip.text.x = element_text(angle = 90, size = 6, colour = "grey40", inherit.blank = FALSE),
        strip.background = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8, colour = "grey45"),
        legend.title = element_blank())
dev.off()

#FR plot
pdf("SAFF 2017 @ SCT - Catégories de question par secteur.pdf", width = 8, height = 10)
ggplot(data=demoMeans.df, aes(x=SURVEYR,y=value, fill = variable_F)) +
  geom_rect(data = subset(demoMeans.df,DESCRIP_F == "Secrétariat du Conseil du Trésor du Canada"),
            aes(fill = DESCRIP_F),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_area(stat = "identity") +
  #scale_y_continuous(labels = scales::percent) +
  ggtitle(ttl_F) +
  labs(fill = "Response type",
       subtitle = expl_F,
       caption = cap_F) +
  scale_fill_manual(values = PNN_F.clrs, labels = c("Négatif", "Neutre", "Positif", "SCT")) +
  #geom_errorbar(data = PNNmeans_F.df, aes(ymax=value, ymin=value), colour = "grey45") +
  #scale_colour_manual(values = PNNmeans_F.clrs) +
  facet_grid(QSection_F ~ DESCRIP_F, switch = "y", scales = "free_y",
             labeller = labeller(DESCRIP_F = label_wrap_gen(30), QSection_F = label_wrap_gen()))  +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
        plot.subtitle = element_text(face = "bold", size = 8, colour = "grey40"),
        plot.caption = element_text(face = "italic", size = 8, colour = "grey45"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        panel.grid = element_blank(),
        panel.spacing.y = unit(0, "mm"),
        panel.spacing.x = unit(0.5, "mm"),
        strip.text.y = element_text(angle = 180, size = 8, colour = "grey40", inherit.blank = FALSE),
        strip.text.x = element_text(angle = 90, size = 8, colour = "grey40", inherit.blank = FALSE),
        strip.background = element_blank(),
        legend.text = element_text(size = 8, colour = "grey45"),
        legend.title = element_blank())
dev.off()

#ss.df <- ss.df %>% 
#  arrange(desc(SURVEYR), desc(NEGATIVE)) %>%
#  mutate(QUESTION = factor(QUESTION, unique(QUESTION))) %>%
#  mutate(BYCOND = factor(BYCOND, unique(BYCOND)))

