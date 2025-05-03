library(shiny)
library(shinyjs)
library(DT)
library(ggplot2)
library(png)
library(shinythemes)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinythemes::shinytheme("readable"),
  
  titlePanel("New York citybike trips in 2022"),
  
  sidebarPanel(
    selectInput("chart", "Select chart",
                c("Trips by bike type" = "Trips", 
                  "Distance" = "dis",
                  "Average speed" = "avgs",
                  "Average speed depending on the distance" = "avgs2dis", 
                  "Trips intensity during the day" = "Hours"),
                width = "80%"
    ),
    
    conditionalPanel(
      condition = "input.chart == 'Trips'",
      checkboxInput("show_table1", "Show data table", value = FALSE)
      ),
    
    conditionalPanel(
      condition = "input.chart == 'dis'",
      selectizeInput("station", label = "Start station",
                     choices = c("Select start station name" = "", unique(Distance$start_station_name)),
                     multiple = FALSE,
                     options = list(maxOptions = 100),
                     width = "80%")
      ),
    
    conditionalPanel(
      condition = "input.chart == 'avgs'",
      selectInput("plot_type", "Select plot type",
                  c("box", "function"),
                  width = "80%"),
      
      conditionalPanel(
        condition = "input.plot_type == 'function'",
        sliderInput("num_obs", "Select number of observations", min = 10000, max = 45000, value = 10000, step = 5000, width = "80%"),
        actionButton("sample", "Sample again"),
        checkboxInput("show_points", "Show points", value = FALSE)),
      
      conditionalPanel(
        condition = "input.plot_type == 'box'",
        checkboxInput("show_table2", "Show data table", value = FALSE))
        
      ),
    
    conditionalPanel(
      condition = "input.chart == 'avgs2dis'",
      dateInput("date", "Select date",
                value = "2022-01-01", min = "2022-01-01", max = "2022-12-31", weekstart = 1,
                width = "80%")
      ),
    
    conditionalPanel(
      condition = "input.chart == 'Hours'",
      selectInput("day", "Select weekday",
                  c("Monday" , "Tuesday", "Wendesday", "Thursday", "Friday", "Saturday", "Sunday"),
                  width = "80%"),
      checkboxInput("show_combined","Show plot not depending on weekday", value = FALSE),
      sliderInput("flatten", "Flatten the curve", min = 1, max = 25, value = 1, step = 4, width = "60%")
      
    ),
    
  width = 3),
  
  mainPanel(
    plotOutput("plot", brush = "plot_brush", width = 1200, height = 625),
    DT::dataTableOutput("table"),
    width = 9
  )
)


server <- function(input, output){
  
  updateSelectizeInput(inputId = "station", choices = list(unique(Distance$start_station_name)), server = TRUE)
  
  Distance_subset <- reactive({
    req(input$station)
    Distance %>% filter(start_station_name == input$station)
  })
  
  AvgSpeed_subset <- reactive({
    input$sample
    
    req(input$num_obs)
    n <- input$num_obs
    
    AvgSpeed %>% 
      group_by(rideable_type) %>% sample_n(size = n, replace = TRUE) %>%
      add_count(avg_speed, rideable_type, sort = TRUE, name = "number_of_trips") %>% 
      select(rideable_type, avg_speed, number_of_trips) %>%
      distinct() 
  })
  
  Avgs2dis_subset <- reactive({
    req(input$date)
    Avgs2dis$date <- strftime(Avgs2dis$date, format = "%F")
    Avgs2dis %>% 
      filter(date == input$date) %>%
      select(rideable_type, start_hour, end_hour, start_station_name,
             end_station_name, travelled_distance, avg_speed)
  })
  
  Avgs2dis_brushed <- reactive({
    req(input$plot_brush)
    brushedPoints(Avgs2dis_subset(), brush = input$plot_brush) %>% 
      arrange(desc(avg_speed)) -> X
    rownames(X) <- NULL
    X
  })
  
  Hours_subset <- reactive({
    req(input$day)
    req(input$flatten)
    k <- input$flatten
    day <- input$day

    moving_average <- function(Hours, k, b = TRUE){
      if(b){
      w <- c(0, cumsum(Hours %>% pull(day)))
      }
      else{
        w <- c(0, cumsum(Hours %>% pull(avg_overall)))
      }
      (w[(k+1):length(w)] - w[1:(length(w) - k)]) / k
    }
    
    as.data.table(Hours)[(1 + floor(k / 2)):(nrow(Hours) - floor(k / 2))] %>% transmute(time_of_day = as.POSIXct(time_of_day, tz = "GMT", format = "%T"),
                                                                                        d = moving_average(Hours, k),
                                                                                        Overall = moving_average(Hours, k, FALSE)) %>%
      arrange(desc(d)) -> X
    colnames(X)[2] <- input$day
    as.data.frame(X)
  })
    
  output$table <- renderDataTable(
    {
    if(input$chart == "Trips" & input$show_table1){
      DT::datatable(TripsT,
                    options = list(pageLength = 3),
                    rownames = FALSE,
                    caption = "Fractions (as percent) of each bike type trips in different months")
    }
    else{
      if(input$chart == "dis"){
        req(input$station)
        st <- input$station
        order_of_types <- c("classic", "electric", "docked")
        
        Distance_subset() %>% group_by(rideable_type) %>% mutate(avg_distance = round(mean(travelled_distance), 2),
                                                                 numb_of_trips = n(),
                                                                 med = round(median(travelled_distance), 2),
                                                                 sd = round(sd(travelled_distance), 2),
                                                                 .keep = "unused") %>% select(rideable_type, numb_of_trips, avg_distance, med, sd) %>% 
          distinct() %>% arrange(sapply(rideable_type, function(x) which(x == order_of_types))) -> DT
        
        DT::datatable(DT,
                      options = list(pageLength = 3),
                      rownames = FALSE,
                      colnames = c("Bike type",
                                   "Number of started trips",
                                   "Average travelled distance [km]",
                                   "Median [km]",
                                   "Standard deviation"),
                      caption = paste0("Info about station: ", st)
                      )
      }
      else{
        if(input$chart == "avgs" & input$plot_type == "box" & input$show_table2){
          DT::datatable(AvgSpeed_summary,
                        options = list(pageLength = 3),
                        rownames = FALSE,
                        colnames = c("Bike type", 
                                     "Mean average speed [km/h]",
                                     "Median [km/h]",
                                     "Standard deviation")
                        )
        }
        else{
          if(input$chart == "avgs2dis" & nrow(Avgs2dis_brushed()) >= 1){
            DT::datatable(Avgs2dis_brushed(),
                          options = list(pageLength = 3),
                          rownames = TRUE,
                          colnames = c("Bike type", "Started at", "Ended at",
                                       "From station", "To station",
                                       "Approx. distance [km]", "Approx. average speed [km/h]"),
                          caption = "Info about brushed points")
          }
        }
      }
    }
    }
  )
  
  output$plot <- renderPlot(
    {
    if(input$chart == "Trips"){
      ggplot(Trips, aes(x = month, y = number_of_trips, fill = factor(rideable_type, levels = c("classic", "electric", "docked")))) + 
        geom_bar(stat = "identity", width = 0.55, alpha = 0.7) +
        scale_x_discrete(limits = c("January", "February", "March", "April", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
        scale_y_continuous(breaks = seq(0, 2 * (10^6), 0.5 * (10^6)),
                           minor_breaks = seq(0, 2.5 * (10^6), 10^5),
                           labels = c("0", "0.5M", "1M", "1.5M", "2M")) +
        scale_fill_manual(values = c("classic" = "blue",
                                     "electric" = "green3",
                                     "docked" = "orangered")) +
        labs(x = "", y = "Number of trips", fill = "Bike type:") +
        theme(axis.title.y = element_text(size = 20, color = "gray20"),
              axis.text.x = element_text(size = 20, angle = 45, hjust = 0.9), axis.text.y = element_text(size = 15),
              axis.ticks = element_blank(),
              axis.line = element_line(color = "darkgray", linewidth = 1, linetype = "solid", lineend = "round"),
              legend.key.size = unit(1.5, "cm"), legend.position = c(0.1, 0.8),
              legend.title = element_text(size = 25, color = "gray20"), legend.text = element_text(size = 20, face = "italic", color= "gray25"),
              legend.background = element_rect(color = "gray15", linetype = "solid", size = 1),
              panel.grid.major.x = element_blank(),
              plot.margin = margin(l = 0.5, b = 0, r = 1, t = 0.5,  unit = "cm")) 
    }
    else{
      if(input$chart == "dis"){
        Distance_subset() %>% filter(rideable_type != "docked") -> Distance_sub
        
        ggplot(Distance_sub, aes(x = factor(rideable_type, levels = c("classic", "electric")), y = travelled_distance, color = rideable_type)) +
          geom_boxplot(alpha = 0.3, size = 1.2, varwidth = TRUE,  fill = c("blue", "green3"), outlier.size = 2, outlier.alpha = 0.7) +
          scale_color_manual(values = c("classic" = "blue",
                                        "electric" = "green3")) +
          scale_y_continuous(breaks = seq(0, ceiling(max(Distance_sub$travelled_distance)), 2.5),
                             minor_breaks = seq(0, ceiling(max(Distance_sub$travelled_distance)), 0.5)) +
          labs(x = "", y = "Approx. distance [km]", color = "Bike type:") +
          theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
                axis.title.y = element_text(size = 20, color = "gray20"), axis.text.y = element_text(size = 15),
                axis.line = element_line(color = "darkgray", linewidth =  1, linetype = "solid", lineend = "round"),
                legend.key.size = unit(1.5, "cm"), legend.position = c(0.92, 0.8),
                legend.title = element_text(size = 25, color = "gray20"), legend.text = element_text(size = 20, face = "italic", color = "gray25"),
                legend.background = element_rect(color = "gray15", linetype = "solid", size = 1),
                panel.grid.major.x = element_blank(),
                plot.margin = margin(l = 0.5, b = 0.5, r = 1, t = 0.5,  unit = "cm"))
      }
      else{
        if(input$chart == "avgs"){
          if(input$plot_type == "box"){
            pic <- readPNG("AvgSpeed_boxplot.png")
            plot.new()
            grid::grid.raster(pic)
          }
          else{
            ggplot(AvgSpeed_subset(), aes(x = avg_speed, y = number_of_trips, color = factor(rideable_type, levels = c("classic", "electric", "docked")))) + 
              geom_smooth(se = FALSE, method = "gam", size = 1.4) +
              scale_x_continuous(breaks = seq(0, 30, 5),
                                 minor_breaks = seq(0, 30, 1)) +
              scale_y_continuous(n.breaks = 8) +
              scale_color_manual(values = c("classic" = "blue",
                                            "electric" = "green3",
                                            "docked" = "orangered")) +
              labs(x = "Approx. average speed [km/h]", y = "Number of trips", color = "Bike type:",
                   caption = paste0("'Number of trips' values for points in each colour group sum to ", input$num_obs)) +
              theme(axis.title.x = element_text(size = 20, color = "gray20"), axis.title.y = element_text(size = 20, color = "gray20"),
                    axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
                    axis.ticks = element_blank(),
                    axis.line = element_line(color = "darkgray", linewidth =  1, linetype = "solid", lineend = "round"),
                    legend.key.size = unit(1.5, "cm"), legend.position = c(0.92, 0.8),
                    legend.title = element_text(size = 25, color = "gray20"), legend.text = element_text(size = 20, face = "italic", color = "gray25"),
                    legend.background = element_rect(color = "gray15", linetype = "solid", size = 1),
                    plot.caption = element_text(size = 12, color = "gray15", face = "italic"),
                    plot.margin = margin(l = 0.5, b = 0.5, r = 1, t = 0.5,  unit = "cm")) -> AvgSpeed_function_plot
            
            if(input$show_points){
              AvgSpeed_function_plot + geom_point(size = 1.5, alpha = 0.7, shape = 16) -> AvgSpeed_function_plot
            }
            
            AvgSpeed_function_plot
          }
        }
        else{
          if(input$chart == "avgs2dis"){
            Avgs2dis_sub <- Avgs2dis_subset()
            
            ggplot(Avgs2dis_sub, aes(x = travelled_distance, y = avg_speed, color = rideable_type)) + 
              geom_point(alpha = 0.8, size = 3, shape = 16) +
              geom_rug(colour = "gray10", size = 0.5) +
              scale_x_continuous(breaks = seq(1, ceiling(max(Avgs2dis_sub$travelled_distance)), 2.5),
                                 minor_breaks = seq(1, ceiling(max(Avgs2dis_sub$travelled_distance)), 0.5)) +
              scale_y_continuous(breaks = seq(5, ceiling(max(Avgs2dis_sub$avg_speed)), 2.5),
                                 minor_breaks = seq(5, ceiling(max(Avgs2dis_sub$avg_speed)), 0.5)) +
              scale_colour_manual(values = c("classic" = "blue",
                                             "electric" = "green3",
                                             "docked" = "orangered")) +
              labs(x = "Approx. distance [km]", y = "Approx. average speed [km/h]", color = "Bike type:",
                   caption = "Showing only the trips with at least 1 km distance and 5 km/h average speed") +
              theme(axis.title.x = element_text(size = 20, color = "gray20"), axis.title.y = element_text(size = 20, color = "gray20"),
                    axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.ticks = element_blank(),
                    axis.line = element_line(color = "darkgray", linewidth =  1, linetype = "solid", lineend = "round"),
                    legend.key.size = unit(1.5, "cm"), legend.position = c(0.92, 0.25),
                    legend.title = element_text(size = 25, color = "gray20"), legend.text = element_text(size = 20, face = "italic", color = "gray25"),
                    legend.background = element_rect(colour = "gray15", linetype = "solid", size = 1),
                    plot.caption = element_text(size = 12, color = "gray15", face = "italic"),
                    plot.margin = margin(l = 0.5, b = 0.5, r = 1, t = 0.5,  unit = "cm"))
          }
          else{
            Hours_sub <- Hours_subset()
            day <- input$day
            peak_at <- Hours_sub$time_of_day[[1]]
            peak <- Hours_sub[day][1, 1]
            
            ggplot(Hours_sub) +
              geom_line(aes_string(x = "time_of_day", y = day, color = "'day'"), linewidth = 1.2) +
              geom_vline(xintercept = peak_at, color = "gray10", linetype = "dashed", linewidth = 0.9) +
              annotate(geom = "label", x = peak_at, y = 5.5, 
                       label = paste0("peak at ", strftime(peak_at, format = "%T", tz = "GMT")), size = 6, color = "gray15", label.padding = unit(0.5, "lines")) +
              scale_x_datetime(date_labels = "%H:%M", date_minor_breaks = "hours") + 
              scale_y_continuous(limits = c(0, 5.5), expand = c(0, 0.2), 
                                 breaks = seq(0, ceiling(peak), 0.5),
                                 minor_breaks = seq(0, ceiling(peak), 0.25)) +
              scale_color_manual(values = c(day = "darkblue",
                                            "Overall" = "red"),
                                 labels = c(day, "Overall")) +
              labs(x = "Hour", y = "Number of trips", caption = "With accuracy to one minute") +
              theme(axis.title.x = element_text(size = 20, color = "gray20"), axis.title.y = element_text(size = 20, color = "gray20"),
                    axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),
                    axis.ticks = element_blank(),
                    axis.line = element_line(color = "darkgray", linewidth =  1, linetype = "solid", lineend = "round"),
                    legend.key.size = unit(1.5, "cm"), legend.position = c(0.92, 0.87),
                    legend.title = element_blank(), legend.text = element_text(size = 20, face = "italic", color = "gray25"),
                    legend.background = element_rect(color = "gray15", linetype = "solid", size = 1),
                    plot.caption = element_text(size = 15, color = "gray15", face = "italic"),
                    plot.margin = margin(l = 0.5, b = 0.5, r = 1, t = 0.5,  unit = "cm")) -> Hours_plot
            
            if(input$show_combined){
              Hours_plot + geom_line(aes(x = time_of_day, y = Overall, color = "Overall"), linewidth = 1.2) -> Hours_plot
            }
              
            Hours_plot
          }
        }  
      }
    }
  }
  )
}

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

shinyApp(ui, server)
