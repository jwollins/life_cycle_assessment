### HAU Conservation Ag Experiment
## Life Cycle Assessment
## Joe Collins 
## 2024-06-24
### 01 - Packages required

## 01.1 PACKAGES ####

suppressPackageStartupMessages({
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(ggpubr)) install.packages("ggpubr")
  if (!require(gridExtra)) install.packages("gridExtra")
  if (!require(readxl)) install.packages("readxl")
  if (!require(readr)) install.packages("readr")
  if (!require(plotrix)) install.packages("plotrix")
  if (!require(lmerTest)) install.packages("lmerTest")
  if (!require(openxlsx)) install.packages("openxlsx")
  if (!require(kableExtra)) install.packages("kableExtra")
  
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(ggpubr) # custom plotting
  library(gridExtra) # grid plotting
  library(readxl) # read .xlsx files
  library(readr) # read .txt files
  library(plotrix) # standard error
  library(lmerTest) # linear mixed effect models
  library(openxlsx) # read xl files 
  library(kableExtra) # write latex
})

