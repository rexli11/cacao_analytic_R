library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)

# 文件要預處理，會有?值產生
setwd("D:/Github_version_file_R/data_set/cacao")
cacao_df <- read.csv("cacao.csv")

head(cacao_df)
str(cacao_df)

colnames(cacao_df)
