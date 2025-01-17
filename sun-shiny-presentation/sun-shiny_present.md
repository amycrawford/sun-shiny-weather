A (sun)Shiny Recreation of the New York Times Weather Chart
========================================================
author: Amy Crawford, Kiegan Rice, and Katie Will
date: 4/25/17
autosize: true
font-family: 'Avenir'

Inspiration
========================================================

- In 1979 The New York Times published a visualization of weather data that maps how temperatures compare to historic averages and records over the course of one year.
- In 2016 they published an online, interactive version.

<https://www.nytimes.com/interactive/2016/02/19/us/2015-year-in-weather-temperature-precipitation.html#new-york_ny>.


Project Outline
========================================================
1. Scrape data
2. Recreate static weather chart
3. Introduce Shiny interactivity


Scraping the Data
========================================================
1. <font size="6">Loop for daily temperatures (~24,500 days)</font>
2. <font size="6">Loop for historic records and averages (~365 days)</font>
3. <font size="6">Current temperature</font>  

<img src="figures/weather_underground_screenshot.png" height = "350px" width = "600px" />  
<small><https://www.wunderground.com/history/airport/KDSM/2015/1/1/DailyHistory></small>


Creating a Static Visual
========================================================

The visual was created with four layers in ggplot.

<img src="figures/static_visual.png" height="500px" width="1000px" />



Final Static Visual
========================================================

<img src="figures/Final_static.png" height = "300px" width = "1000px" />
<img src="figures/NYT_visual.png" height = "300px" width = "1000px" />



Introducing Shiny Interactivity
========================================================

1. Original weather chart
  - Choose between 10 Midwestern cities
  - Slider for year
  - Choosing plot layers
  - Adding current temperature
  - Submit changes

2. Spaghetti plot
  - Making use of the average temperature data
  - Choice of city
  - 2016 and 2017 highlighted


Shiny App in Action!
========================================================

Our Shiny app can be accessed with the following R code: 


```r
shiny::runGitHub(repo="amycrawford/sun-shiny-weather", subdir="shiny")
```


Discussion
========================================================

- Impact of additional elements
- Demonstrating variability
- Visual power



Future Work
========================================================

- Fahrenheit/Celsius
- Rethink structure and optimize code
  - Web scraping
  - Graphic
- Precipitation data
- Plotly
- What was NYT thinking? (finding a better way to summarize yearly weather patterns)
- Package?



=========================================================

#Hofmann2018



Questions?
=========================================================


