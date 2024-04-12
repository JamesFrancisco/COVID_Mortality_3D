# Load COVID-19 data by state
# Calculate deaths per day
# Plot as a 3D scatter plot
library("pastecs")
library("gclus")
library("plotly")
library(dplyr)
library(zoo)
library(jsonlite)

df <- read.csv("https://raw.githubusercontent.com/coviddata/coviddata/master/data/sources/ny_times/us-states.csv")
df3 <- fromJSON("https://covidtracking.com/api/v1/states/daily.json", flatten = TRUE)
df3$date <- as.Date(as.character(df3$date), format("%Y%m%d"))


df <- df %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(deaths.day = deaths -lag(deaths, default = first(deaths))) %>%
  mutate(cases.day = cases -lag(cases, default = first(cases))) %>%
  mutate(deaths.rma = rollmean(deaths.day, 7, fill = NA, align = 'right')) %>%
  mutate(cases.rma = rollmean(cases.day, 7, fill = NA, align = 'right'))

df3 <- df3 %>%
  group_by(state) %>%
  arrange(date) %>% 
  mutate(deaths.day = death -lag(death, default = first(death))) %>%
  mutate(cases.day = positive -lag(positive, default = first(positive))) %>%
  mutate(hospitalized.day = hospitalizedCumulative -lag(hospitalizedCumulative, default = first(hospitalizedCumulative)))%>%
  mutate(hospitalized.rma = rollmean(hospitalizedCurrently, 7, fill = NA, align = 'right')) %>%
  mutate(deaths.rma = rollmean(deaths.day, 7, fill = NA, align = 'right')) %>%
  mutate(cases.rma = rollmean(cases.day, 7, fill = NA, align = 'right'))

df4 <- subset(df3, ((df3$state == 'NY')|(df3$state == 'FL')))


plot_ly(subset(df3, df3$state == 'NY'), x=~date, y=~cases.rma, type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'NY'), x=~date, y=~deaths.rma, type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'NY'), x=~date, y=~hospitalized.rma, type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'NY'), x=~date, y=~inIcuCurrently, type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'FL'), x=~date, y=~cases.rma, type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'FL'), x=~date, y=~(deaths.rma), type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'FL'), x=~date, y=~hospitalized.rma, type="scatter", mode = 'markers')

plot_ly(subset(df3, df3$state == 'FL'), x=~date, y=~inIcuCurrently, type="scatter", mode = 'markers')
