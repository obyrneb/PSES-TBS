library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)

#Import sector-specifc dataset - subset 5 
ss5.df <- read.csv("datasets//2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv", na.strings = "9999")

sectorList<- subset(ss5.df, LEVEL1ID =="26" ,select = c(LEVEL3ID, LEVEL4ID, BYCOND, DESCRIP_E,DESCRIP_F))

sector.ID <- "23" # LEVEL3ID for selected sector
sector_E.short <- "CSS" #Set English short name
sector_F.short <- "SSC" #Set French short name
sector.names <- subset(ss5.df, LEVEL1ID =="26" & LEVEL3ID == sector.ID,select = c(DESCRIP_E,DESCRIP_F))
sector_E.long <- toString(unique(sector.names$DESCRIP_E)) #Set English long name
sector_F.long <- toString(unique(sector.names$DESCRIP_F)) #Set French long name

#Filter data
sd.df <- subset(ss5.df, LEVEL1ID == "0" |                       #Include Public Service topline
                       (LEVEL1ID == "26" & (LEVEL2ID == "0" |   #Include TBS topline
                        LEVEL3ID == sector.ID)),                 #Include selected TBS sector
                select=c(DESCRIP_E,DESCRIP_F,QUESTION,TITLE_E,TITLE_F,POSITIVE,NEUTRAL,NEGATIVE))




#Create column comparing sector and TBS negative scores
#sd.df <- sd.df %>%
#  group_by(QUESTION) %>%
#  arrange(variable) %>%
#  mutate(levelup.delta = as.numeric(value-lag(value,1)))

#Sort by largest negative difference
#sd.df <- sd.df %>%
#  ungroup(QUESTION) %>%
#  arrange(value,levelup.delta) %>%
#  mutate(QUESTION = factor(QUESTION, unique(QUESTION)))

#Shorten text and order organizations
sd.df$Org_E <- mapvalues(sd.df$DESCRIP_E, 
                         c("Public Service","Treasury Board of Canada Secretariat",sector_E.long),
                         c("PS","TBS",sector_E.short))
sd.df$Org_E <- factor(sd.df$Org_E, levels = c("PS","TBS",sector_E.short), ordered = TRUE)
sd.df$Org_F <- mapvalues(sd.df$DESCRIP_F, 
                         c("Fonction publique","Secrétariat du Conseil du Trésor du Canada",sector_F.long),
                         c("FP","SCT",sector_F.short))
sd.df$Org_F <- factor(sd.df$Org_F, levels = c("FP","SCT",sector_F.short), ordered = TRUE)

#Sort by most negative
sd.df <- sd.df %>% 
  arrange(desc(Org_E),desc(NEGATIVE)) %>%
  mutate(QUESTION = factor(QUESTION, unique(QUESTION)))

sd.df <- melt(sd.df, measure.vars = c("POSITIVE","NEUTRAL","NEGATIVE"))      #Convert positive, neutral and negative columns to a single list 
sd.df <- na.omit(sd.df)   #Remove n/a values


subttl <- paste("Each cell of this graph represents the proportion of negative, neutral and positive responses to each PSES question. The three bars represent, from left to right, the Public Service, TBS and the ", sector_E.long, ".")
subttl <- paste0(strwrap(subttl, 140), sep="", collapse="\n")

#Plot Data

ggplot(data = sd.df, aes(x = Org_E, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(position = "top") +
  #geom_text(aes(label = Org_E), 
  #          colour = "grey45", angle = 90, vjust = 0.5, hjust = 0.5, check_overlap = TRUE) +
  facet_wrap(~QUESTION) +
  scale_fill_manual(values = c("NEGATIVE" = "#CD202C", "NEUTRAL" = "#63CECA", "POSITIVE" = "#CCDC00")) +
  labs(title = paste("PSES 2017 Sector Results - ",sector_E.long), 
       caption = "2017 Public Service Employee Survey Open Datasets",
       subtitle = subttl) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "grey40"),
        plot.subtitle = element_text(face = "bold", size = 8, colour = "grey40"),
        plot.caption = element_text(face = "italic", size = 8, colour = "grey45"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(size = 8, angle= 90, colour = "grey45", vjust = 1, hjust = 1),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey95"),
        panel.grid = element_blank(),
        #strip.placement = "inside",
        #panel.spacing.y = unit(5, "mm"),
        #panel.spacing.x = unit(0.5, "mm"),
        strip.text.x = element_text(size = 8, colour = "grey40", inherit.blank = FALSE),
        strip.background = element_blank(),
        legend.text = element_text(size = 8, colour = "grey45"),
        legend.position = "right",
        legend.title = element_blank())

ggsave(paste("PSES 2017 - ",sector_E.long,".pdf"), width = 10, height = 8)