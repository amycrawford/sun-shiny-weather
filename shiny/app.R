library(shiny)
require(tidyverse)
require(dplyr)
library(plotly)
library(rvest)

data(temps)
##### I'm not sure why this did this... it is in the data folder so we should just be able to look at it
# Define UI for application that draws a histogram
temps <- temps %>% na.omit()


shinyApp(
  ui = navbarPage(
    "NYT Weather Chart: \n Major Midwestern Cities",
    tabPanel("Single Year",
             inputPanel(selectInput("citySelect", label = "City:",
                                    choices = unique(temps$city), selected = "Des Moines, IA"),
                        sliderInput("yearSlider", label = "Year:",
                                    min = 1950, max = 2016, step = 1, sep = "",value = 2015),
                        
                        checkboxGroupInput("checkLayer", label = ("Data Layers:"), 
                                           choices = list("Record Values" = 1, 
                                                          "Historical Averages" = 2, 
                                                          "Daily for Chosen Year" = 3), selected = c(1,2,3)),
                        checkboxInput("currentTemp", label = "Add Current Temperature",
                                           value = FALSE),
                        submitButton("Update View", icon("refresh"))),
             plotOutput('tempsplot')#,
             #verbatimTextOutput('selected')
             ),
    
    tabPanel("Spaghetti Plot",
             inputPanel(selectInput("citySelectSpag", label = "City:",
                                    choices = unique(temps$city), selected = "Des Moines, IA"),
                        submitButton("Update View", icon("refresh"))),
             plotOutput('spagplot'))
    ),
  
  
  server = function(input, output, session){
    
    
    ####### REACTIVE FOR SINGLE YEAR TABLE #######
    gg <- reactive({
      
      ############# BASE AND GENERAL SET-UP #############
      dates_2016 <- gsub(pattern = "-", replacement = "/", x = seq(from = as.Date("2016-01-01"), 
                                                                   to = as.Date("2016-12-31"), by = "days"))
      temps <- temps %>% select(-X) %>% filter(city == input$citySelect) %>% mutate(date_2016 = lubridate::ymd(paste("2016", month, day, sep = "-")))
      base <- temps %>% ggplot(aes(x = date_2016)) + 
        theme_classic() +
        theme(panel.grid.major.x = element_line(colour = c("black", rep("grey", times = 11)), linetype = c(0, rep(2, times = 11))),
              axis.text.x = element_text(hjust = -0.9), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.y = element_line(colour = "darkgrey")) +
        scale_x_date(date_breaks = "1 month", 
                     date_labels = "%b", 
                     expand=c(0,0)) + 
        scale_y_continuous(breaks = seq(-50, 120, by = 20), limits= c(-50, 120)) + 
        xlab("") +
        ylab("")
      
      text_layer_description <- annotate("text", 
                                         label = "Bars represent the range between the daily high and low.",
                                         x = lubridate::ymd("2016-07-24"),
                                         y = -28, hjust = "center", size = 2.3)
      
      base <- base + text_layer_description
      
      ############# RECORD TEMPS LAYER #############
      data_record <- temps %>% select(-date, -year) %>% 
        filter(actual_ave_rec == "Record", mean_max_min != "Mean Temperature") %>% 
        group_by(month, day) %>% spread(key = mean_max_min, value = value)
      
      layer_record <- geom_rect(data = data_record, aes(xmin = date_2016, xmax = date_2016+1, ymin = data_record$`Min Temperature`, ymax = data_record$`Max Temperature`), fill = "#E6E5DD", alpha = 0.8)
      
      legend_layer_record <- geom_rect(aes(xmin = lubridate::ymd("2016-07-22"), 
                                           xmax =  lubridate::ymd("2016-07-27"), 
                                           ymin = -15, 
                                           ymax = 15), 
                                       fill = "#E6E5DD", alpha = 0.8)
      text_layer_record <-  annotate("text",
                                     label = c("Record Low", "Record High"), 
                                     x =  c(lubridate::ymd("2016-07-22"), lubridate::ymd("2016-07-22")), 
                                     y = c(-15, 15), 
                                     hjust = "right", 
                                     size = 2.3)
      
      records_plot <- base + 
        layer_record + legend_layer_record + text_layer_record
      
      
      ############# AVERAGE TEMPS LAYER #############
      data_average <- temps %>% select(-date, -year) %>% 
        filter(actual_ave_rec == "Historical Average", mean_max_min != "Mean Temperature") %>% 
        group_by(month, day) %>% spread(key = mean_max_min, value = value)
      
      layer_average <- geom_rect(data = data_average, aes(xmin = date_2016, xmax = date_2016+1, 
                                                          ymin = data_average$`Min Temperature`, ymax = data_average$`Max Temperature`), 
                                 fill = "#BCACAC", alpha = 0.8)
      
      legend_layer_average <- geom_rect(aes(xmin = lubridate::ymd("2016-07-24")-0.5, 
                                            xmax =  lubridate::ymd("2016-07-25")+0.5, 
                                            ymin = -10, 
                                            ymax = 10), 
                                        fill = "#BCACAC", alpha = 0.8)
      
      text_layer_average <-  annotate("text",
                                      label = "Range of Historical Averages", 
                                      x =  lubridate::ymd("2016-07-19"), 
                                      y = 0, 
                                      hjust = "right", 
                                      size = 2.3)
      
      line_layer_average_ver <- geom_segment(aes(x = lubridate::ymd("2016-07-20"), 
                                                 xend = lubridate::ymd("2016-07-20"), 
                                                 y = -10, 
                                                 yend = 10), 
                                             size = 0.01)
      
      line_layer_average_hor1 <- geom_segment(aes(x = lubridate::ymd("2016-07-20"),
                                                  xend = lubridate::ymd("2016-07-22"),
                                                  y = 10,
                                                  yend = 10),
                                              size = 0.01)
      
      line_layer_average_hor2 <- geom_segment(aes(x = lubridate::ymd("2016-07-20"),
                                                  xend = lubridate::ymd("2016-07-22"),
                                                  y = -10,
                                                  yend = -10),
                                              size = 0.01)
      
      averages_plot <- base + layer_average + 
        legend_layer_average + text_layer_average + 
        line_layer_average_ver + line_layer_average_hor1 + line_layer_average_hor2
      
      
      records_averages_plot <- records_plot + layer_average + 
        legend_layer_average + text_layer_average + 
        line_layer_average_ver + line_layer_average_hor1 + line_layer_average_hor2
      
      
      ############# ACTUAL TEMPS PLOT #############
      data_actual <- temps %>% 
        filter(actual_ave_rec == "Actual", year == input$yearSlider) %>% 
        group_by(month, day) %>% spread(key = mean_max_min, value = value)
      
      layer_actual <- geom_rect(data = data_actual, aes(xmin = date_2016, xmax = date_2016+1, 
                                                        ymin = data_actual$`Min Temperature`, ymax = data_actual$`Max Temperature`), 
                                fill = "maroon")
      layer_white_ylines <- geom_hline(yintercept = seq(-40, 100, by = 20), colour = "white", lwd = 0.1)
      
      
      
      legend_layer_actual <- geom_rect(aes(xmin = lubridate::ymd("2016-07-24")-0.05, 
                                           xmax =  lubridate::ymd("2016-07-25")+0.15, 
                                           ymin = -5, 
                                           ymax = 12), 
                                       fill = "maroon")
      
      
      text_layer_actual <-  annotate("text",
                                     label = c("Actual Low", "Actual High"), 
                                     x =  c(lubridate::ymd("2016-07-29"), lubridate::ymd("2016-07-29")), 
                                     y = c(-5, 12), 
                                     hjust = "left", 
                                     size = 2.3)
      
      line_layer_actual_hor1 <- geom_segment(aes(x = lubridate::ymd("2016-07-24"),
                                                 xend = lubridate::ymd("2016-07-28"),
                                                 y = -5,
                                                 yend = -5),
                                             size = 0.01)
      
      line_layer_actual_hor2 <- geom_segment(aes(x = lubridate::ymd("2016-07-24"),
                                                 xend = lubridate::ymd("2016-07-28"),
                                                 y = 12,
                                                 yend = 12),
                                             size = 0.01)
      
      
      actual_plot <- base + layer_actual + legend_layer_actual + text_layer_actual + line_layer_actual_hor1 + line_layer_actual_hor2
      
      all_plot <- records_plot + 
        layer_average + 
        legend_layer_average + text_layer_average + 
        line_layer_average_ver + line_layer_average_hor1 + line_layer_average_hor2 + 
        layer_white_ylines +
        layer_actual + legend_layer_actual + text_layer_actual + line_layer_actual_hor1 + line_layer_actual_hor2
      
      
      records_actual_plot <- records_plot + 
        layer_white_ylines +
        layer_actual + legend_layer_actual + text_layer_actual + line_layer_actual_hor1 + line_layer_actual_hor2
      
      averages_actual_plot <- averages_plot + 
        layer_white_ylines +
        layer_actual + legend_layer_actual + text_layer_actual + line_layer_actual_hor1 + line_layer_actual_hor2
      
      
      #records_plot
      #averages_plot
      #actual_plot
      #records_averages_plot
      #records_actual_plot
      #averages_actual_plot
      #all_plot
      display_plot <- base
      if(length(input$checkLayer) == 0){
        display_plot <- base
      }
      else if(length(input$checkLayer) == 3){
        display_plot <- all_plot
      }
      else if(length(input$checkLayer) == 1 & input$checkLayer[1] == "3"){
        display_plot <- actual_plot
      }
      else if(length(input$checkLayer) == 1 & input$checkLayer[1] == "2"){
        display_plot <- averages_plot
      }
      else if(length(input$checkLayer) == 1 & input$checkLayer[1] == "1"){
        display_plot <- records_plot
      }
      else if(input$checkLayer[1] == "1" & input$checkLayer[2] == "2"){
        display_plot <- records_averages_plot
      }
      else if(input$checkLayer[1] == "1" & input$checkLayer[2] == "3"){
        display_plot <- records_actual_plot
      }
      else if(input$checkLayer[1] == "2" & input$checkLayer[2] == "3"){
        display_plot <- averages_actual_plot
      }
      else{
        display_plot <- base
      }
      
      
      
      ### add in if statement to scrape current temperature
      if(length(input$checkLayer) > 0 & input$currentTemp == TRUE){
        state <- gsub(".*\ ", "", input$citySelect)  ## getting state from the first entry in temps$city
        city <- gsub(",.*", "", input$citySelect)  ## getting city from the first entry in temps$city
        city <- gsub("\\.", "", city)  ## getting rid of "." if it exists in a city i.e. St. Louis
        city <- gsub(" ", "_", city)  ## replacing a space with "_" i.e. St_Louis
        
        url <- paste0("https://www.wunderground.com/US/", state, "/", city, ".html")
        html <- read_html(url)
        
        cur_temp <- html %>% html_nodes("#curTemp .wx-value") %>% html_text() %>% parse_number()
        cur_date <- lubridate::ymd(paste0("2016", "-", lubridate::month(lubridate::today()), "-", lubridate::day(lubridate::today())))
        
        cur_point_layer <- geom_point(aes(x = cur_date, y = cur_temp), color = "black")
        
        cur_point_legend <-  annotate("text",
                                      label = paste0("Current Temperature in ", input$citySelect), 
                                      x =  cur_date, 
                                      y = 110, 
                                      hjust = "left", 
                                      size = 3)
        cur_point_legend_line <- geom_segment(aes(x = cur_date,
                                                 xend = cur_date,
                                                 y = 110,
                                                 yend = cur_temp + 2),
                                              size = 0.05, arrow = arrow(length = unit(0.3,"cm")))
        display_plot <- display_plot + cur_point_layer + cur_point_legend + cur_point_legend_line
      }
      
      
      return(display_plot)
    })
    
    ####### SINGLE YEAR TABLE ########
    output$tempsplot = renderPlot({
      gg()
    })

    
#################### SPAG PLOT THINGS BELOW HERE ##############################  
    
    ####### REACTIVE FOR SINGLE YEAR TABLE #######
    gg_spag <- reactive({ 
      temps_spag <- temps %>% 
        select(-X) %>% filter(city == input$citySelectSpag, 
                              actual_ave_rec == "Actual", 
                              mean_max_min == "Mean Temperature") %>% 
        mutate(date_2016 = lubridate::ymd(paste("2016", month, day, sep = "-")))
      
      base_spag <- temps_spag %>% filter(year < 2016 & year > 1950) %>%
        ggplot(aes(x = date_2016, y = value, group = year)) + 
        geom_line(colour = "grey") + 
        theme_classic() +
        theme(panel.grid.major.x = element_line(colour = c("black", rep("grey", times = 11)), linetype = c(0, rep(2, times = 11))),
              axis.text.x = element_text(hjust = -0.9), 
              axis.line.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.line.y = element_line(colour = "darkgrey")) +
        scale_x_date(date_breaks = "1 month", 
                     date_labels = "%b", 
                     expand=c(0,0)) + 
        scale_y_continuous(breaks = seq(-50, 120, by = 20), limits= c(-50, 120)) + 
        xlab("") +
        ylab("")
      
      layer_2016 <- geom_line(data = temps_spag[temps_spag$year == 2016,], aes(x = date_2016, y = value), colour = "steelblue")
   
      layer_2017 <- geom_line(data = temps_spag[temps_spag$year == 2017,], aes(x = date_2016, y = value), colour = "steelblue", lwd = 1.5)
      
      legend_layer_grey <- geom_rect(aes(xmin = lubridate::ymd("2016-07-13"), 
                                           xmax =  lubridate::ymd("2016-07-27"), 
                                           ymin = -5, 
                                           ymax = -4.5), 
                                       fill = "grey")
      
      legend_layer_maroon <- geom_rect(aes(xmin = lubridate::ymd("2016-07-13"), 
                                         xmax =  lubridate::ymd("2016-07-27"), 
                                         ymin = -9, 
                                         ymax = -8.5), 
                                     fill = "steelblue")
      
      legend_layer_blue <- geom_rect(aes(xmin = lubridate::ymd("2016-07-13"), 
                                         xmax =  lubridate::ymd("2016-07-27"), 
                                         ymin = -13, 
                                         ymax = -11.5), 
                                     fill = "steelblue")
      
      text_layer_grey <-  annotate("text",
                                     label = c("Mean Daily Temperatures", "1950 - 2015", "2016", "2017"), 
                                     x =  c(lubridate::ymd("2016-07-13"), lubridate::ymd("2016-07-28"), lubridate::ymd("2016-07-28"), lubridate::ymd("2016-07-28")), 
                                     y = c(0 , -4.75, -8.75, -12.5), 
                                     hjust = "left", 
                                     size = c(3.5, 2.3, 2.3, 2.3))
      
      spag_plot <- base_spag + layer_2016 + layer_2017 + legend_layer_grey+ legend_layer_maroon + legend_layer_blue + text_layer_grey
      
      return(spag_plot)  
      
    })
    
    
    ####### SPAG PLOT ########
    output$spagplot = renderPlot({
      gg_spag()
    })    
    
    })
    
