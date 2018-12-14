# ------------------------------
# R & PSES: A SHORT TUTORIAL
#
# The code below creates a simple (and somewhat inelegant) small multiples graph.
# It charts sentiment by proportion for each question category for each sector of a given department.
#
# ("#" is the comment character - anything after "#" on a line is not interpreted as code by R.)
# ------------------------------

# Load libraries, from which we will be using data manipulation and graphing functions.
library(dplyr)
library(tidyr)
library(ggplot2)

# This is the URL for the data we are manipulating - the organizational subset of the 2017 Public Service Employee Survey.
# The URL long and cumbersome, so we save it to the "URL" variable for use below.
URL <- "https://www.canada.ca/content/dam/tbs-sct/documents/datasets/pses-saff/2017/2017_PSES_SAFF_Subset-5_Sous-ensemble-5.csv"

# Create "datasets_t" and "plots_t" folders. The "_t" denotes tutotrial.
dir.create("datasets_t")
dir.create("plots_t")

# We download this data using the "URL" variable, put it in a "datasets" folder, and name it something simpler.
download.file(URL, "datasets_t//tutorialData.csv")

# Load data from CSV file, reading "9999" as empty.
PSESdata <- read.csv("datasets_t//tutorialData.csv", na.strings=9999)

# Make a list of LEVEL1ID identifiers and department names and print it to the console. 
# You can find your department on the generated list. Note its LEVEL1ID for use in the next step.
idList <- distinct(PSESdata, LEVEL1ID, Dept_E)
idList

# Filter for TBS data (LEVEL1ID == 26) and survey year 2017 (SURVEYR == 2017). You can also your own department ID.
# Select only 5 columns: Question number, sector description and the 3 sentiment categories.
myData <- PSESdata %>%
  filter(LEVEL1ID == 26 & SURVEYR == 2017) %>%
  select(QUESTION,DESCRIP_E,POSITIVE,NEUTRAL,NEGATIVE)

# Gather the 3 sentiment categories into a single field, named "sentiment" and the value field named "prop".
# Create a variabel for question section (QSECTION) by using the first character of the question (e.g. A_Q001).
# Group by sector name, question section and sentiment.
# summarise a sentiment mean for each sentiment by questions section, by sector name.
mySummary <- myData %>%
  gather(key="sentiment",value="prop",-DESCRIP_E,-QUESTION) %>%
  mutate(QSECTION = substr(QUESTION,1,1)) %>%
  group_by(DESCRIP_E,QSECTION,sentiment) %>%
  summarise(prop = mean(prop))

# Create small bar charts using the X axis for sentiment and the y axis for proportion. Base fill colour on sentiment.
# Construct the larger small multiples chart using the question section and sector names as X and Y axes for the small charts.
# Add a very simple theme (theme_void).
myPlot <- ggplot(data=mySummary, aes(x=sentiment, y=prop)) +
  geom_col(aes(fill=sentiment)) +
  facet_grid(DESCRIP_E ~ QSECTION) +
  theme_void()

# Save the plot as a PDF file the "plots" folder, 8 inches wide by 10 inches tall.
ggsave(plot=myPlot, file="plots_t/tutorialPlot.pdf", width = 8, height = 10)
