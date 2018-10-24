library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library("tibble")
library(reshape2)
library(stringr)
library(scales)

ss1 <- read.csv("datasets//2017_PSES_SAFF_Subset-1_Sous-ensemble-1.csv")

qe.df <- subset(ss1, LEVEL1ID%in% c(0,26) 
                & QUESTION %in% c("F_Q61","F_Q62","K_Q88","K_Q89","K_Q90"),
                select=c(DESCRIP_E,QUESTION,TITLE_E,POSITIVE,NEUTRAL,NEGATIVE))