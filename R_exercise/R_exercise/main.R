#imports 
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)

# To do 1 -----------------------------------------------------------------

gdp <- read_excel('data/GDPOutputData.xlsx',sheet = 'Data')
sector <- read.csv('data/Sectors.csv')

head(gdp)
dim(sector)
gdp$Date <- as.Date(gdp$Date, format = "%Y-%m-%d")

# To do 2 -----------------------------------------------------------------
#a
df1 <- select(gdp, c('TOTAL', 'A','F','H'))
#b
df2 <- select(gdp, c(1:5))
head(df2)

#c
df3 <- select(gdp, !c(sector$Code))
d <-  select(df3, !matches("[[:digit:]]"))


# To do 3 -----------------------------------------------------------------
#a
d <- gdp %>% 
  select(!c(sector$Code), !matches("[[:digit:]]")) %>% 
  rename(GDP = 'TOTAL') %>%
  mutate(C = sqrt(CC), year = year(Date), month = format(Date, "%b")) %>% 
  filter(C < 50) %>%
  select(c('Date','GDP','C','year','month'))

write.csv(d, 'output_table/first_summary.csv')


# To do 4 -----------------------------------------------------------------

selected_df <- gdp %>% 
  select('Date','TOTAL',sector$Code) %>% 
  rename(GDP = 'TOTAL') %>% 
  mutate(GDP_in_1000 = GDP/1000) %>% 
  select(!c('GDP')) %>% 
  filter(year(Date) >= (year(max(Date))-10), GDP_in_1000 > 300)

joined_df <- selected_df %>% 
  pivot_longer(cols = -c(Date), names_to = "Code", values_to = "Observation") %>% 
  left_join(sector, by = 'Code') %>% 
  mutate(year = year(Date), month = format(Date, "%b"))

##quizes
#1
joined_df %>%
  filter(Sector %in% c('AGRICULTURE, FORESTRY AND FISHING')) %>% 
  group_by(year) %>% 
  summarize(max_obs = max(Observation)) %>%  
  filter(max_obs == max(max_obs) )


#2
joined_df %>%
  filter((Sector %in% c('MANUFACTURING')) & (Code %in% c('GDP_in_1000'))) %>% 
  group_by(month) %>% 
  summarize(total_GDP = sum(Observation, na.rm = T))


## RUN ###
sectors = read.csv('data/Sectors.csv')
gdp = read_xlsx('data/GDPOutputData.xlsx', sheet = 'Data')

data_sectors <- gdp %>%
  select(Date, TOTAL, sectors$Code) %>%
  mutate(DE = D + E, 
         AB = A + B) %>%
  pivot_longer(-Date, names_to = "Code", values_to = "Level") %>% 
  left_join(sectors, by = "Code") %>% 
  filter(!Code %in% c('A', 'B', 'D', 'E'))  %>% 
  group_by(Code) %>% 
  mutate(Date = as.Date(Date),
         QoQ = Level/lag(Level, 1)*100-100,
         YoY = Level/lag(Level, 4)*100-100)

write.csv(data_sectors, "output_table/data_sectors.csv")













































  
