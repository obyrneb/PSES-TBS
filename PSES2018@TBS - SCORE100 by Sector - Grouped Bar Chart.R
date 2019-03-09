library(scales)
library(readxl)
library(tidyverse)
library(ggrepel)
library(grid)

mainDir <- getwd()
dataDir <- "datasets"
plotDir <- "plots"

ifelse(!dir.exists(file.path(mainDir, dataDir)), dir.create(file.path(mainDir, dataDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, plotDir)), dir.create(file.path(mainDir, plotDir)), FALSE)

pses2018_units <- read_excel(file.path(mainDir,dataDir,"2018_PSES_SAFF_26_TBS_SCT_Units_Unites.xlsx"), na = "9999")

score100s_TBS <- pses2018_units %>%
  mutate(unitcode = ifelse(is.na(BYCOND),"TBS",word(BYCOND, 2, sep = " = "))) %>%
  filter(!(unitcode %in% c("200","303","304","201","202","999")) & !(is.na(SCORE100))) %>%
  group_by(unitcode) %>%
  mutate(overall100 = mean(SCORE100)) %>%
  ungroup() %>%
  group_by(unitcode,DESCRIP_E,DESCRIP_F,INDICATORID,INDICATORENG,INDICATORFRA,overall100) %>%
  summarise(indicator100 = mean(SCORE100)) %>%
    ungroup()
    
score100s_PS <- ss1 %>%
  left_join(indicatorMap, by = "QUESTION") %>%
  filter(LEVEL1ID == "0" & SURVEYR == 2018 & !(is.na(SCORE100))) %>%
  mutate(unitcode = "PS") %>%
  mutate(overall100 = mean(SCORE100)) %>%
  ungroup() %>%
  group_by(unitcode,DESCRIP_E,DESCRIP_F,INDICATORID,INDICATORENG,INDICATORFRA,overall100) %>%
  summarise(indicator100 = mean(SCORE100)) %>%
  ungroup()

score100s <- bind_rows(score100s_TBS,score100s_PS) %>%
  filter()

ggplot(score100s, aes(x = fct_reorder(DESCRIP_E,overall100), y = indicator100,fill = INDICATORENG)) +
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(labels = wrap_format(30)) +
  coord_flip() +
  labs(#title = "TBS Sectors by Indicator",
       #subtitle = str_wrap("Each row displays the mean scores out of 100 for each question grouping.Sectors are ordred from highest overall mean score to lowest.TBS and Public Service numbers are included for reference.",
       #                   80),
       caption = "2018 Public Service Employee Survey",
       x = "Sector",
       y = "Mean score out of 100") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")

ggsave("TBS Sectors by Indicator.pdf", height = 8, width = 10)


slp <- ggplot(score100s, aes(x = str_wrap(INDICATORENG,15), y = indicator100, group = DESCRIP_E)) +
  geom_line(aes(color = DESCRIP_E, alpha = 1), size = 1) +
  geom_text_repel(data = score100s %>% filter(INDICATORID == 1), 
                  aes(label = str_wrap(DESCRIP_E,30), colour = DESCRIP_E), 
                  hjust = 2, 
                  fontface = "bold", 
                  size = 4,
                  nudge_x = -1, 
                  direction = "y") +
  geom_text_repel(data = score100s %>% filter(is.na(INDICATORID)), 
                  aes(label = str_wrap(DESCRIP_E,30), colour = DESCRIP_E), 
                  hjust = -1, 
                  fontface = "bold", 
                  size = 4,
                  nudge_x = 1, 
                  direction = "y") +
  geom_point(colour = "white", size = 8, shape = 16) +
  geom_text(aes(label = round(indicator100,0), y = round(indicator100,0)),
            size = 4, colour = "grey30", fontface = "bold") +
  # move the x axis labels up top
  scale_x_discrete(position = "top") +
  theme_bw() +
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none") +
  # Remove the panel border
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=10)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  # Change canvas size
  theme(plot.margin = margin(t = .2, r = 1, b = .2, l = 1, "in")) +
  #  Labelling as desired
  labs(
    title = "TBS Sectors by indicator",
    subtitle = "Indicators are based on question groupings and scores are out of 100.",
    caption = "Data from 2018 Public Service Employee Survey"
  )
# Code to turn off clipping
gt <- ggplotGrob(slp)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

ggsave("TBS Sectors by Indicator - Slopechart.pdf", height = 8, width = 14)