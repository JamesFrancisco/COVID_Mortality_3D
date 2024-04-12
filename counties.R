# Load COVID-19 data by county
# Calculate deaths per day
# Plot as a 3D scatter plot

library("pastecs")
library("gclus")
library("plotly")
library("dplyr")
library(zoo)

df2 <- read.csv("https://raw.githubusercontent.com/coviddata/coviddata/master/data/sources/ny_times/us-counties.csv")

df2 <- df2 %>%
group_by(state, county) %>%
arrange(date) %>%
mutate(deaths.day = deaths -lag(deaths, default = first(deaths)) )%>%
mutate(cases.day = cases -lag(cases, default = first(cases)))%>%
mutate(cases.rma = rollmean(cases.day, 7, fill = NA, align = 'right'))%>%
mutate(deaths.rma = rollmean(deaths.day, 7, fill = NA, align = 'right'))  

# plot_ly(df2, x=~date, y=~county, z=~deaths.day, type="scatter3d", mode="markers", size=1)

plot_ly(filter(df2, state == "New York"), x=~date, y=~county, z=~deaths.rma, type="scatter3d", mode="markers", size=1)

plot_ly(filter(df2, state == "New York"), x=~date, y=~county, z=~cases.rma, type="scatter3d", mode="markers", size=1)

plot_ly(filter(df2, state == "Florida"), x=~date, y=~county, z=~deaths.rma, type="scatter3d", mode="markers", size=1)

plot_ly(filter(df2, state == "Florida"), x=~date, y=~county, z=~cases.rma, type="scatter3d", mode="markers", size=1)


