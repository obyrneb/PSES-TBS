library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)

#Load PSES TBS Sector data and set "9999" as n/a
SectorData <- read_excel("2017_PSES_SAFF_26_TBS_SCT_Units_Unites.xlsx", na = "9999")
SectorData <- read.csv()

#Set factors
#SectorData$DESCRIP_E <- factor(SectorData$DESCRIP_E)
#SectorData$QUESTION <- factor(SectorData$QUESTION)

#Create table for Positive, Neutral and Negative rankings by sector
df <- subset(ss5, LEVEL1ID == "26" & (LEVEL3ID != "000" | LEVEL2ID == "000"),
              #& !(QUESTION %in% c("F_Q61","F_Q62","K_Q88","K_Q89","K_Q90")),
              select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,POSITIVE,NEUTRAL,NEGATIVE))
  
#Melt the Positive, Neutral and Negative columns into a single column
df <- melt(df)

#Order Negative, Neutral and Postive
df$variable <- factor(df$variable, 
                      levels = c("NEGATIVE","NEUTRAL","POSITIVE"),
                      ordered = TRUE)

#Set DESCRIP order
df$DESCRIP_E <- factor(df$DESCRIP_E)
df$DESCRIP_E <- relevel(df$DESCRIP_E, "Treasury Board of Canada Secretariat")
df$DESCRIP_F <- factor(df$DESCRIP_F)
df$DESCRIP_F <- relevel(df$DESCRIP_F, "Secrétariat du Conseil du Trésor du Canada")

#Set TBS colours for Negative, Neutral and Postive
#PNN.clrs <- c("NEGATIVE" = "#CD202C", "NEUTRAL" = "#63CECA", "POSITIVE" = "#CCDC00")
#PNNmeans.clrs <- c("NEGATIVE" = "#88151d", "NEUTRAL" = "#428986", "POSITIVE" = "#889200")

# Create question sub-sections
df$SubSection <- substr(df$QUESTION,0,1)
df$SubSection <- factor(df$SubSection)

#Create a data frame of PNN means by question
#PNNmeans.df <- aggregate(data = df, value ~ SubSection + variable, mean)

#English captions.
expl_E <- "Each cell of this chart displays the proportion of responses for a particular question category and TBS sector. The bars over each column represent the average value for this question category across sectors."
expl_E <- paste0(strwrap(expl_E, 140), sep="", collapse="\n")
ttl_E <- "PSES 2017 - TBS Sector Results by Question Category"
cap_E <- "2017 Public Service Employee Survey Open Datasets"
df$SubSection_E <- mapvalues(df$SubSection, c("A","B","C","D","E","F","G","H","I","J","K","L"),
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
df$variable_E <- mapvalues(df$variable, 
                           c("NEGATIVE","NEUTRAL","POSITIVE"),
                           c("negative","neutral","positive"))
PNNmeans_E.df <- aggregate(data = df, value ~ SubSection_E + variable_E, mean)
PNN_E.clrs <- c("negative" = "#CD202C", "neutral" = "#63CECA", "positive" = "#CCDC00", 
                "Treasury Board of Canada Secretariat" = "#d1e7ee")

#French Captions
expl_F <- "Chaque cellule de ce graphique affiche la proportion de réponses pour une catégorie de questions particulière et un secteur du SCT. Les barres au dessus de chaque colonne représentent la valeur moyenne de cette catégorie de question pour tous les secteurs."
expl_F <- paste0(strwrap(expl_F, 140), sep="", collapse="\n")
ttl_F <- "SAFF 2017 - Résultats des secteurs du SCT par catégorie de question" 
cap_F <- "Ensemble de données ouvertes du Sondage auprès des fonctionnaires fédéraux de 2017"
df$SubSection_F <- mapvalues(df$SubSection, c("A","B","C","D","E","F","G","H","I","J","K","L"),
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
df$variable_F <- mapvalues(df$variable, 
                           c("NEGATIVE","NEUTRAL","POSITIVE"),
                           c("négatif","neutre","positif"))
PNNmeans_F.df <- aggregate(data = df, value ~ SubSection_F + variable_F, mean)
PNN_F.clrs <- c("négatif" = "#CD202C", "neutre" = "#63CECA", "positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada" = "#d1e7ee")

#EN plot
pdf("PSES 2017 @ TBS - Question Categories by Sector.pdf", width = 10, height = 8)
ggplot(data=df, aes(x=variable_E,y=value, fill = variable_E)) +
  geom_rect(data = subset(df,DESCRIP_E == "Treasury Board of Canada Secretariat"),
            aes(fill = DESCRIP_E),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle(ttl_E) +
  labs(fill = "Response type", 
       subtitle = expl_E,
       caption = cap_E) +
  scale_fill_manual(values = PNN_E.clrs, labels = c("Negative", "Neutral", "Positive", "TBS")) +
  geom_errorbar(data = PNNmeans_E.df, aes(ymax=value, ymin=value), colour = "grey45") +
  #scale_colour_manual(values = PNNmeans_E.clrs) +
  facet_grid(SubSection_E ~ DESCRIP_E, switch = "y", scales = "free_y",
             labeller = labeller(DESCRIP_E = label_wrap_gen(30), SubSection_E = label_wrap_gen()))  +
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

#FR plot
pdf("SAFF 2017 @ SCT - Catégories de question par secteur.pdf", width = 10, height = 8)
ggplot(data=df, aes(x=variable_F,y=value, fill = variable_F)) +
  geom_rect(data = subset(df,DESCRIP_F == "Secrétariat du Conseil du Trésor du Canada"),
            aes(fill = DESCRIP_F),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 0.3) +
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle(ttl_F) +
  labs(fill = "Response type",
       subtitle = expl_F,
       caption = cap_F) +
  scale_fill_manual(values = PNN_F.clrs, labels = c("Négatif", "Neutre", "Positif", "SCT")) +
  geom_errorbar(data = PNNmeans_F.df, aes(ymax=value, ymin=value), colour = "grey45") +
  #scale_colour_manual(values = PNNmeans_F.clrs) +
  facet_grid(SubSection_F ~ DESCRIP_F, switch = "y", scales = "free_y",
             labeller = labeller(DESCRIP_F = label_wrap_gen(30), SubSection_F = label_wrap_gen()))  +
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

  #facet_wrap(~DESCRIP_E) +
  #ggtitle("Small Multiples in R") +
  #theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  #theme(axis.text.x = element_text(angle=90)) 

