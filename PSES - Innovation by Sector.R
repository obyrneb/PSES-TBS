library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)
library(GGally)

#PSES Data---------------------------------------------

#Load PSES TBS Sector data and set "9999" as n/a
SectorData <- read_excel("2017_PSES_SAFF_26_TBS_SCT_Units_Unites.xlsx", na = "9999")
SectorOrder <- read.csv("datasets//Sector_Order.csv")

#Set factors
#SectorData$DESCRIP_E <- factor(SectorData$DESCRIP_E)
#SectorData$QUESTION <- factor(SectorData$QUESTION)

#Create table for Positive, Neutral and Negative rankings by sector
df <- subset(ss5, LEVEL1ID == "26" & (LEVEL3ID != "0" | LEVEL2ID == "0") & LEVEL4ID == "0"
             & (QUESTION %in% c("A_Q18","E_Q54")),
             select=c(LEVEL2ID,LEVEL3ID,LEVEL4ID,LEVEL5ID,DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,POSITIVE,NEUTRAL,NEGATIVE))
#df <- merge(df,SectorOrder, by = c("DESCRIP_E","DESCRIP_F"))

df_org <- df

#Melt the Positive, Neutral and Negative columns into a single column
df <- melt(df, measure = c("POSITIVE","NEUTRAL","NEGATIVE"))

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
expl_E <- paste0(strwrap(expl_E, 80), sep="", collapse="\n")
ttl_E <- "Innovation @ TBS"
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
PNNmeans_E.df <- aggregate(data = df, value ~ TITLE_E + variable_E, mean)
PNN_E.clrs <- c("negative" = "#CD202C", "neutral" = "#63CECA", "positive" = "#CCDC00")

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
PNNmeans_F.df <- aggregate(data = df, value ~ TITLE_F + variable_F, mean)
PNN_F.clrs <- c("négatif" = "#CD202C", "neutre" = "#63CECA", "positif" = "#CCDC00",
                "Secrétariat du Conseil du Trésor du Canada" = "#d1e7ee")





#EN plot


PSESIQ <- ggplot(data=df, aes(x=variable_E,y=value, fill= variable_E)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_y_continuous(limits = c(0,100), breaks = c(0,100)) +
  scale_fill_manual(values = PNN_E.clrs, labels = c("Negative", "Neutral", "Positive")) +
  coord_flip() +
  geom_text(colour = "grey45", size=2, aes(label=value, hjust=-0.5, vjust=0.5)) +
  labs(fill = "Response type", 
       caption = cap_E) +
  #scale_colour_manual(values = PNN_E.clrs, labels = c("Negative", "Neutral", "Positive")) +
  facet_grid(DESCRIP_E ~ TITLE_E , switch = "y", scales = "free_y",
             labeller = labeller(DESCRIP_E = label_wrap_gen(30), TITLE_E = label_wrap_gen(40)))  +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
        plot.subtitle = element_text(face = "bold", size = 8, colour = "grey40"),
        plot.caption = element_text(face = "italic", size = 8, colour = "grey45"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6, colour = "grey60", angle = 0),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "grey60"),
        panel.background = element_rect(fill = "grey95"),
        panel.grid = element_blank(),
        panel.spacing.y = unit(1, "mm"),
        panel.spacing.x = unit(0.5, "mm"),
        strip.text.y = element_blank(),
        #strip.text.y = element_text(angle = 180, size = 8, colour = "grey40", inherit.blank = FALSE),
        strip.text.x = element_text(angle = 0, size = 8, colour = "grey40", inherit.blank = FALSE),
        strip.background = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8, colour = "grey45"),
        legend.title = element_blank())

#IMM Data-----------------------------------------------------------

IMMQ.df <- read_excel("datasets//IMM_Sector_Levels.xlsx", na = "-")

IMMQ.df <- subset(IMMQ.df, n > 0)

#IMMQ.df <- mutate(IMMQ.df, attributeMeans = rowMeans(IMMQ.df[4:9], na.rm = TRUE))

IMMQ.df_org <- IMMQ.df

IMMQ.df <- melt(IMMQ.df, id= c("DESCRIP_E", "DESCRIP_F", "n", "imputedValueMean"))

IMMQ.df <- IMMQ.df %>%
  arrange(desc(imputedValueMean,DESCRIP_E)) %>%
  mutate(DESCRIP_E = paste0(DESCRIP_E," (n=",n,")")) %>%
  mutate(DESCRIP_E = factor(DESCRIP_E, unique(DESCRIP_E))) %>%
  mutate(DESCRIP_F = paste0(DESCRIP_F," (n=",n,")")) %>%
  mutate(DESCRIP_F = factor(DESCRIP_F, unique(DESCRIP_F)))
#IMMQ.df$DESCRIP_E <- relevel(IMMQ.df$DESCRIP_E, "Treasury Board of Canada Secretariat (n=343)")

plotIMMQ <- function(language) {
  
  if (language == "E") {
    TBS = "Treasury Board of Canada Secretariat (n=343)"
    cap = "2018 TBS Innovation Maturity Model Questionnaire"
    file = "Innovation Maturity by Sector.pdf"
    IMMQ.df$DESCRIP_lang = IMMQ.df$DESCRIP_E
  } else if (language == "F") {
    TBS = "Secrétariat du Conseil du Trésor du Canada (n=343)"
    cap = "Questionnaire sur la maturité de l'innovation au SCT 2018"
    file = "Maturité de l'innovation par secteur.pdf"
    IMMQ.df$DESCRIP_lang = IMMQ.df$DESCRIP_F
    IMMQ.df$variable <- mapvalues(IMMQ.df$variable, 
                                  c("Client expectations",
                                    "Strategic alignment",
                                    "Internal innovation activities",
                                    "External innovation activities",
                                    "Organization",
                                    "Culture"),
                                  c("Attentes des clients",
                                    "Harmonisation stratégique",
                                    "Activités internes",
                                    "Activités externes",
                                    "Organisation",
                                    "Culture"))
    
  }
  
  IMMQ <- ggplot(data=IMMQ.df, aes(x=1,y=value, fill = variable)) +
    geom_rect(data = subset(IMMQ.df,DESCRIP_lang == TBS),
              fill = "grey40",xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,alpha = 0.3) +
    geom_bar(stat = "identity") +
    scale_y_continuous(limits = c(0,5), breaks = c(0,5)) +
    coord_flip() +
    geom_text(colour = "white", size = 2.5, aes(label=signif(value,2), hjust=1.1, fontface = "bold")) +
    labs( 
         #subtitle = expl_E,
         caption = cap) +
    #scale_fill_manual(values = PNN_E.clrs) +
    facet_grid(DESCRIP_lang ~ variable , switch = "y", scales = "free_y",
               labeller = labeller(DESCRIP_lang = label_wrap_gen(30), variable = label_wrap_gen(10)))  +
    theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
          plot.subtitle = element_text(face = "bold", size = 8, colour = "grey40"),
          plot.caption = element_text(face = "italic", size = 8, colour = "grey45"),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6, colour = "grey60", angle = 0),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_line(colour = "grey60"),
          panel.background = element_rect(fill = "grey95"),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(colour = "white"),
          panel.spacing.y = unit(1, "mm"),
          panel.spacing.x = unit(0.5, "mm"),
          strip.text.y = element_text(angle = 180, size = 8, colour = "grey40", hjust = 0.5, inherit.blank = FALSE),
          strip.text.x = element_text(angle = 0, size = 8, colour = "grey40", hjust = 0.5, inherit.blank = FALSE),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 8, colour = "grey45"),
          legend.title = element_blank())
  
  ggsave(file, height = 6, width = 6.5)
}

plotIMMQ("F")

#Graph--------------------------------------------------------------

pdf("PSES 2017 @ TBS - Innovation Questions by Sector.pdf", width = 11, height = 6)

#GraphTitle <- textGrob("Innovation by Sector @ TBS", gp=gpar (fontsize=16, col="grey40"))

lay <- rbind(c(1,1,1,2,2))
             
grid.arrange(IMMQ, PSESIQ, ncol=2, layout_matrix=lay)

dev.off()

#Correlations-------------------------------------------------------

a.df <- subset(df_org,select = c(DESCRIP_E,QUESTION,NEGATIVE,NEUTRAL,POSITIVE))

a_pos.df <- dcast(a.df,DESCRIP_E ~ QUESTION, value.var = "POSITIVE")
names(a_pos.df)[names(a_pos.df) == "A_Q18"] <- "Q18_Positive"
names(a_pos.df)[names(a_pos.df) == "E_Q54"] <- "Q54_Positive"

a_neu.df <- dcast(a.df,DESCRIP_E ~ QUESTION, value.var = "NEUTRAL")
names(a_neu.df)[names(a_neu.df) == "A_Q18"] <- "Q18_Neutral"
names(a_neu.df)[names(a_neu.df) == "E_Q54"] <- "Q54_Neutral"

a_neg.df <- dcast(a.df,DESCRIP_E ~ QUESTION, value.var = "NEGATIVE")
names(a_neg.df)[names(a_neg.df) == "A_Q18"] <- "Q18_Negative"
names(a_neg.df)[names(a_neg.df) == "E_Q54"] <- "Q54_Negative"

a.df <- merge(a_pos.df, a_neu.df, by = "DESCRIP_E")
a.df <- merge(a.df, a_neg.df, by = "DESCRIP_E")

b.df <- subset(IMMQ.df_org, select = c(1,4:9))

both.df <- merge(a.df, b.df, by = "DESCRIP_E")

both.df <- both.df %>% 
  arrange(DESCRIP_E) %>%
  mutate(DESCRIP_E = factor(DESCRIP_E, unique(DESCRIP_E)))
both.df$DESCRIP_E <- relevel(both.df$DESCRIP_E, "Treasury Board of Canada Secretariat")


ggpairs(both.df, columns = c(2,3,8:13), cardinality_threshold = 16)

ggcorr(both.df, palette = "RdBu", label = TRUE, layout.exp = 1, hjust = 1)

ggsave("PSES 2017 @ TBS - Innovation Questions Correlations.pdf", width = 10, height = 8)







