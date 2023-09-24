getwd()
library(readxl)
library(tidyr)
library(dplyr)

gdp <- read_excel('GDPOutputData.xlsx', sheet = 'Data')
sector <- read.csv('Sectors.csv')
View(sector)


pivoted_table <- gdp %>%
  pivot_longer(cols = -Date,
               names_to = "Code",
               values_to = "value")

left_join(pivoted_table, sector, by = 'Code')

join_table <- gdp %>%
  pivot_longer(cols = -Date,
               names_to = "Code",
               values_to = "value") %>%
  left_join(sector, by = 'Code')

join_table



