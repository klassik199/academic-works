library(data.table)
library(DT)
library(dplyr)
library(dashboardthemes)
library(ggplot2)
library(plotly)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(fresh)

###########################################################################################################################################

df_series_top100 <- read.csv("C:/Users/Dell/OneDrive/Dokumenty/R/WdED/Projekt2/series_dataframe_top100.csv")

df_series_top500 <- read.csv("C:/Users/Dell/OneDrive/Dokumenty/R/WdED/Projekt2/series_dataframe_top500.csv")

df_series_top1000 <- read.csv("C:/Users/Dell/OneDrive/Dokumenty/R/WdED/Projekt2/series_dataframe_top1000.csv")

###########################################################################################################################################

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#040404",
    maroon = "#9d4877"
  ),
  adminlte_sidebar(
    width = "425px",
    dark_bg = "#2d2e29",
    dark_hover_bg = "#5c5e54",
    dark_submenu_bg = "#2d2e29",
  ),
  adminlte_global(
    content_bg = "#F5F5F5",
    box_bg = "#FFF", 
  )
)

###########################################################################################################################################

ui <- dashboardPage(
  
  dashboardHeader(
    
    title = div(h3("What makes a TV Show popular", style="margin: 0; font-size: 24px; font-weight: bold; color: white;"),
                h4("and how it affects its rating", style="margin: 0; font-size: 20px; font-weight: bold; color: white;")),
    titleWidth = 425
  ),

  dashboardSidebar(
    
    sidebarMenu(
      
      id = "plots_menu",
      menuItem("Correlation analysis", tabName = "corr_plots", icon = icon("diagram-project")),
      menuItem("In depth look at most popular series ", tabName = "ser_plots", icon = icon("chart-line")),
      menuItem("TV shows throughout the decades", tabName = "dec_plots", icon = icon("timeline"),
               menuSubItem("Overall comparison", tabName = "overall", icon = icon("chart-gantt")),
               menuSubItem("Additional statistics", tabName = "additional", icon = icon("chart-column"))
    ),
    
    conditionalPanel(
      
      condition = "input.plots_menu == 'corr_plots'",
      helpText(div(h4("Note: Here you can check for dependencies between", style="font-size: 14.5px"),
                   h4("different characteristics of most popular series and", style="font-size: 14.5px"), 
                   h4("see summaries for them in terms of series genre.", style="font-size: 14.5px"), 
                   style = " text-align: left; padding: 13px; color: white;")),
      selectInput("variable1", "Select pair of variables to check for dependency",
                  choices = c("Average rating ~ Average votes per episode" = "rat_votes",
                              "Average votes per episode  ~ Average number of episodes per season" = "votes_ep",
                              "Average rating ~ Average number of episodes per season" = "rat_ep"),
                  width = "85%"),
      sliderInput("range", "Select range of most popular series",
                  min = 1, 
                  max = 500,
                  value = c(1, 50),
                  step = 1,
                  width = "85%"),
      checkboxInput("trend", "Show LOESS trend line",
                    value = F,
                    width = "85%")),
      
    conditionalPanel(
      condition = "input.plots_menu == 'ser_plots'",
      helpText(div(h4("Note: Here you can examine the course of variability", style="font-size: 14.5px"),
                   h4("for two characteristics throughout the certain series.", style="font-size: 14.5px"), 
                   h4("Below list contains of 100 most popular series with", style="font-size: 14.5px"), 
                   h4("at least 5 and at most 20 seasons.", style="font-size: 14.5px"), 
                   style = "text-align: left; padding: 13px; color: white;")),
      radioButtons("sort", "Sorting",
                   choices = c("Alphabetical order" = "alph",
                               "By most popular" = "pop"),
                   inline = T,
                   width = "85%"),
                     
      conditionalPanel(
        condition = "input.sort == 'alph'",
        selectizeInput("series1", "Series title",
                       choices = c("Select series title" = "", unique(df_series_top100$seriesTitle)),
                       multiple = F,
                       options = list(maxOptions = 100),
                       width = "85%")),

      conditionalPanel(
        condition = "input.sort == 'pop'",
        selectizeInput("series2", "Series title",
                       choices = c("Select series title" = "", unique((df_series_top100 %>% 
                                                                            arrange(desc(avgVotes)))$seriesTitle)),
                       multiple = F,
                       options = list(maxOptions = 100),
                       width = "85%")),

     selectInput("variable2", "Select variable to analyze",
                 choices = c("Average rating per episode" = "avg_rat", 
                             "Average votes per episode" = "avg_votes"),
                 width = "85%"),
     sliderInput("curve", "Smooth the curve",
                 min = 1,
                 max = 11,
                 value = 1,
                 step = 2,
                 width = "85%"),
     helpText(div(h4("Note: Above numbers mean factors in moving", style="font-size: 14.5px; "), 
                  h4("average smoothing method.", style="font-size: 14.5px; "), 
                 style="text-align: left; padding: 12px; color: white;"))
    ),
    
    conditionalPanel(
      condition = "input.plots_menu == 'overall'",
      helpText(div(h4("Note: Here you can compare the distributions for two", style="font-size: 14.5px"),
                   h4("characteristics between the decades when series was", style="font-size: 14.5px"), 
                   h4("first released and see the summary statistics table. ", style="font-size: 14.5px"), 
                   style = "text-align: left; padding: 13px; color: white;")),
      selectInput("variable3", "Select variable to analyze",
                  choices = c("Average rating" = "avg_rat", 
                              "Avgerage episode runtime" = "avg_runtime"),
                  width = "85%"),
      radioButtons("plot_type", "Plot type",
                   choices = c("Box plot" = "box",
                               "Violin plot" = "violin"),
                   inline = T,
                   width = "85%"),
      
      conditionalPanel(
        condition = "input.plot_type == 'violin'",
        checkboxInput("points", "Show points",
                 value = F,
                 width = "85%")
      )
      
    ),
    
    conditionalPanel(
      condition = "input.plots_menu == 'additional'",
      helpText(div(h4("Note: Here you can explore different characteristics", style="font-size: 14.5px"),
                   h4("histograms separetaly regarding to decades. You can", style="font-size: 14.5px"), 
                   h4("also set them together with cummulative histogram", style="font-size: 14.5px"), 
                   h4("and see how much does certain decade take part in it.", style="font-size: 14.5px"), 
                   style = "text-align: left; padding: 13px; color: white;")),
      selectInput("decade", "Select decade",
                  choices = c("1950s",
                              "1960s",
                              "1970s",
                              "1980s",
                              "1990s",
                              "2000s",
                              "2010s"),
                  selected = "2010s",
                  width = "85%"),
      selectInput("variable4", "Select variable to analyze",
                  choices = c("Average rating" = "avg_rat", 
                              "Number of seasons" = "num_seasons",
                              "Emission length in years" = "emission"),
                  width = "85%"),
      checkboxInput("summary", "Show cummulative histogram",
               value = F,
               width = "85%")
    )
    ),
      
    width = 425
  
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML(".sidebar-menu li a { font-size: 20px!important; color: #ffffff!important; }
                                  
                               .skin-blue .sidebar-menu > li.active > a,
                               .skin-blue .sidebar-menu > li:hover > a {
                                border-left-color: #000; }
                              
                               .sidebar-menu  li:hover a {border-left-color: #000;}
                              
                               .sidebar-menu .treeview-menu li.active a {background-color: #5c5e54 !important;}
                              
                               .sidebar-menu .treeview-menu li:hover a {background-color: #5c5e54 !important;}
                              
                              "))),
    
    use_theme(mytheme),
   
    fluidRow(allign = "center", 
      
      box(uiOutput("infobox"), width = 11),
      
      tabBox(id = "tabset",
             tabPanel(id ="tab1", title = h5("Plot", style="font-size: 20px; font-weight: bold; color: black;"),
                      
                      withSpinner(plotlyOutput("plot", height = 760),
                                  type = getOption("spinner.type", 5),
                                  size = getOption("spinner.size", 0.7),
                                  color = getOption("spinner.color", "#616C77"),
                                  color.background = getOption("spinner.color.background", "#6A605A"))
             ),
                      
             tabPanel(id = "tab2", title = h5("Summary table", style="font-size: 20px; font-weight: bold; color: black;"),
                      
                      DT::dataTableOutput("table")),
             
             width = 11)
      
    )
    
  ),
  
  chooseSliderSkin("Flat", "#ed5655")
  
)


############################################################################################################################################################

server <- function(input, output) {

  updateSelectizeInput(inputId = "series1", choices = list(unique(df_series_top100$seriesTitle)), 
                       server = TRUE)
  
  updateSelectizeInput(inputId = "series2", choices = list(unique((df_series_top100 %>% 
                                                                     arrange(desc(avgVotes)))$seriesTitle)), 
                       server = TRUE)
  

  df_series_sub1 <- reactive({
    req(input$range)
 
    df_series_top500 %>% 
      arrange(desc(avgVotes)) %>% 
      slice(input$range[1]:input$range[2])
  })
  
  df_series_sub1_table <- reactive({
    df <- req(df_series_sub1()) %>% 
      group_by(baseGenre) %>% 
      mutate(avgRatingGenre = round(mean(seriesAvgRating), 2),
             avgVotesGenre = round(mean(avgVotes), 0),
             medRatGenre = median(seriesAvgRating),
             avgEpisodesPerSeasonGenre = round(mean(avgEpPerSeason), 1),
             mostPopSeriesGenre = first(seriesTitle),
             numSeriesGenre = n()) %>% 
      select(baseGenre, numSeriesGenre, avgRatingGenre, medRatGenre, avgVotesGenre, avgEpisodesPerSeasonGenre, mostPopSeriesGenre) %>% 
      distinct(baseGenre, .keep_all = T) %>% 
      arrange(desc(numSeriesGenre))
    
    rownames(df) <- NULL
    
    df
    
  })
  
  series <- reactive({
    if(req(input$sort) == "alph"){
      req(input$series1)
      input$series1
    }
    else{
      if(req(input$sort) == "pop"){
        req(input$series2)
        input$series2
      }
    }
  })
  
  df_series_sub2 <- reactive({
    req(series())
  
    df_series_top100 %>% 
      filter(seriesTitle == series()) 
  })
  
  df_series_sub2_table <- reactive({
    df <- req(df_series_sub2()) %>% 
      group_by(seasonNumber) %>% 
      mutate(avgVotesSeason = round(mean(votes), 0),
             avgRatingSeason = round(mean(avgRating), 2),
             medRatingSeason = median(avgRating),
             totalVotesSeason = sum(votes),
             numEpisodesSeason = max(episodeNumber),
             releaseYearSeason = min(releaseYear)) %>% 
      select(seasonNumber, numEpisodesSeason, avgRatingSeason, medRatingSeason, avgVotesSeason, totalVotesSeason, releaseYearSeason) %>% 
      distinct(seasonNumber, .keep_all = T)
    
    rownames(df) <-NULL
    
    df
    
  })
  
  df_series_sub3 <- reactive({
    req(input$decade)
    decade <- input$decade
    
    df <- df_series_top1000 %>% 
      filter(seriesStartDecade == decade) %>% 
      mutate(avgRatingBins = paste0("[", floor(4 * seriesAvgRating) / 4, ", ", floor(4 * seriesAvgRating)  / 4 + 0.25, ")"))
    
    req(input$variable4)
    if(input$variable4 == "avg_rat"){
      var <- "avgRatingBins"
      othervar <- "seriesAvgRating"
    }
    
    if(input$variable4 == "num_seasons"){
      var <- "numSeasons"
      othervar <- "lengthYears"
    }
    if(input$variable4 == "emission"){
      var <- "lengthYears"
      othervar <- "numSeasons"
    }
    
    df %>% 
      group_by(get(var)) %>% 
      arrange(desc(totalVotes)) %>% 
      mutate(count = n(),
             mostPopSeries = first(seriesTitle),
             otherVar = round(mean(get(othervar)), 1)) %>% 
      ungroup %>% 
      select(seriesTitle, seriesAvgRating, avgRatingBins, totalVotes, avgVotes, numSeasons, avgRuntime, 
             seriesStartYear, lengthYears, count, mostPopSeries, otherVar)
  }) 
  
  df_series_sub3_table <- reactive({
    df <- req(df_series_sub3()) %>% 
      group_by(seriesStartYear) %>% 
      arrange(desc(totalVotes)) %>% 
      mutate(mostPopSeries = first(seriesTitle),
             avgRatingYear = round(mean(seriesAvgRating), 2),
             avgVotesYear = round(mean(avgVotes), 0),
             avgTotalVotesYear = round(mean(totalVotes), 0),
             avgNumSeasonsYear = round(mean(numSeasons), 1),
             avgRuntimeYear = round(mean(avgRuntime), 1),
             avgLengthYear = round(mean(lengthYears), 1),
             numSeriesYear = n()) %>% 
      ungroup %>% 
      select(seriesStartYear, numSeriesYear, avgRatingYear, avgVotesYear, avgTotalVotesYear,
             avgNumSeasonsYear, avgRuntimeYear, avgLengthYear, mostPopSeries) %>% 
      distinct(seriesStartYear, .keep_all = T) %>% 
      arrange(seriesStartYear)
    rownames(df) <- NULL
    
    df

  })
  
  
  output$infobox <- renderUI({
    if(req(input$plots_menu == "ser_plots")){
      req(series())
      list(
        infoBox("Title", value = series(), icon = icon("tv"), color = "red", fill = T, width = 4), 
        infoBox("Average rating", value = df_series_sub2()[1, "seriesAvgRating"], 
                icon = icon("star"), color = "olive", fill = T, width = 4),
        infoBox("Average votes per episode", value = df_series_sub2()[1, "avgVotes"], 
                icon = icon("circle-user"), color = "navy", fill = T, width = 4),
        infoBox("Total votes", value = df_series_sub2()[1, "totalVotes"], 
                icon = icon("users"), color = "purple", fill = T, width = 4),
        infoBox("Emission years", value = paste0(df_series_sub2()[1, "seriesStartYear"], " - ", df_series_sub2()[1, "seriesEndYear"]), 
                icon = icon("calendar"), color = "yellow", fill = T, width = 4),
        infoBox("Genres", value = df_series_sub2()[1, "genres"], 
                icon = icon("video"), color = "maroon", fill = T, width = 4)
      )
    }
    else{
      div()
    }
  })
  
  
  output$plot <- renderPlotly(
    {
    if(req(input$plots_menu) == "corr_plots"){
      
      if(input$variable1 == "rat_votes"){
        x_var <- "avgVotes"
        y_var <- "seriesAvgRating"
        title <- "Average ranking depending on average votes per episode"
        x_lab <- "Average votes per episode"
        y_lab <- "Average ranking"
        breaks <- seq(5, 10, 1)
        limits <- c(5, 10)
        fill <- "#BEE5B3"
      }
      if(input$variable1 == "votes_ep"){
        x_var <- "avgEpPerSeason"
        y_var <- "avgVotes"
        title <- paste0("Average votes per episode","<br>",
                        "depending on average number of episodes per season")
        x_lab <- "Average number of episodes per season"
        y_lab <- "Average votes per episode"
        breaks <- breaks_extended(6)
        limits <- c(min(df_series_sub1()$avgVotes), max(df_series_sub1()$avgVotes))
        fill <- "#D1DAE9"
      }
      if(input$variable1 == "rat_ep"){
        x_var <- "avgEpPerSeason"
        y_var <- "seriesAvgRating"
        title <- "Average ranking depending on average number of episodes per season"
        x_lab <- "Average number of episodes per season"
        y_lab <- "Average ranking"
        breaks <- seq(5, 10, 1)
        limits <- c(5, 10)
        fill <- "#BEE5B3"
      }
      
      pdf(NULL)
      
      plot_aes <- aes(x = get(x_var), y = get(y_var), color = baseGenre, 
                      text = paste("Title: ", seriesTitle, "<br>",
                                   x_lab, ": ", get(x_var), "<br>",
                                   y_lab, ": ", get(y_var), "<br>",
                                   "Base genre: ", baseGenre))
      
      g <- df_series_sub1() %>% 
          ggplot(plot_aes) +
          geom_point(alpha = 0.9, size = 3, shape = 16) +
          scale_x_continuous(breaks = breaks_extended(8)) +
          scale_y_continuous(limits = limits, breaks = breaks) +
          scale_color_manual(values = c("Action" = "#FF9900",
                                        "Adventure" = "#00C367",
                                        "Animation" = "#EA6B73",
                                        "Biography" = "#990099",
                                        "Comedy" = "#2C69B0",
                                        "Crime" = "#B10318",
                                        "Drama" = "#024018",
                                        "Horror" = "#000000",
                                        "Thriller" = "#5D6772",
                                        "Western" = "#7F3B08")) +
          labs(x = x_lab, y = y_lab, color = "Base genre:", 
               title = title) +
          theme(axis.title.x = element_text(size = 15, color = "gray10"), axis.title.y = element_text(size = 15, color = "gray10"),
                axis.text.x = element_text(size = 12, color = "gray5"), axis.text.y = element_text(size = 12, color = "gray5"), 
                axis.ticks = element_blank(),
                axis.line = element_line(color = "gray35", linewidth = 1.4, linetype = "solid", lineend = "round"),
                legend.key.size = unit(1, "cm"),legend.title = element_text(size = 15, color = "gray10", face = "bold"), 
                legend.text = element_text(size = 12, color = "gray15"),
                legend.background = element_rect(colour = "gray35", linetype = "solid", linewidth = 1, fill = "#F5F5F5"),
                panel.grid.major.x = element_line(color = "#D6D6D6", linewidth = 1, linetype = "solid"),
                panel.grid.major.y = element_line(color = "#D6D6D6", linewidth = 1, linetype = "solid"),
                panel.background = element_rect(linetype = "solid", linewidth = 1, color = "darkgray", fill = "#F5F5F5"),
                plot.background = element_rect(fill = fill, linetype = "solid", linewidth = 1, color = "black"),
                plot.margin = margin(l = 1, b = 1, r = 1.5, t = 1,  unit = "cm"),
                plot.title = element_text(size = 18, color = "gray10", face = "bold"))
      
      if(input$trend){
        g <- g +
          geom_smooth(method = "loess", se = F, linewidth = 1, color = "gray15", 
                      inherit.aes = F, aes(x = get(x_var), y = get(y_var)),
                      formula = y ~ x) +
          geom_ribbon(stat = "smooth", method = "loess", alpha = .15, level = 0.7, 
                      inherit.aes = F, aes(x = get(x_var), y = get(y_var)),
                      formula = y ~ x)
      }
      
      p <- ggplotly(g, tooltip = "text") %>% 
        layout(annotations = list(x = 1, y = -0.1, 
                                  text = "Showing only series with at least 10000 total votes", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=12, color="#404040", family = "Arial")))
      dev.off()
      
      p
      
    }
    else{
      if(req(input$plots_menu) == "ser_plots"){
        
        if(input$variable2 == "avg_votes"){
          y_var <- "votes"
          y_lab <- "Number of votes"
          title <- paste0("Number of votes per episodes throughout the '", req(series()),"' series")
          breaks <- breaks_extended(6)
          limits <- c(min(df_series_sub2()$votes), max(df_series_sub2()$votes))
          color <- "#001F3F"
          point_color <- "#000C19"
          fill <- "#D1DAE9"
        }
        if(input$variable2 == "avg_rat"){
          y_var <- "avgRating"
          y_lab <- "Average rating"
          title <- paste0("Average rating per episodes throughout the '", req(series()), "' series")
          breaks <- seq(3, 10, 1)
          limits <- c(2.5, 10)
          color <- "#255C43"
          point_color <- "#0B1C14"
          fill <- "#BEE5B3"
        }
        
        df_sub <- df_series_sub2()
         
        req(input$curve)
        
        k <- input$curve
        
        moving_average <- function(df_sub, k){
          if(input$variable2 == "avg_votes"){
            w <- c(0, cumsum(df_sub %>% pull(votes)))
          }
          if(input$variable2 == "avg_rat"){
            w <- c(0, cumsum(df_sub %>% pull(avgRating)))
          }
          
          (w[(k+1):length(w)] - w[1:(length(w) - k)]) / k
          
        }
        
        df <- as.data.table(df_sub)[(1 + floor(k / 2)):(nrow(df_sub) - floor(k / 2))] %>% 
          mutate(y_var_moved = moving_average(df_sub, k)) %>% 
          arrange(desc(y_var_moved)) 
        
        df_sub <- as.data.frame(df)

        text_aes <- aes(text = paste("Season: ", seasonNumber, "<br>",
                                     "Episode: ", episodeNumber, "<br>",
                                     "Title: ", episodeName, "<br>",
                                     y_lab, ": ", get(y_var)))
        
        season_breaks_vec <- df_series_sub2() %>% 
          group_by(seasonNumber) %>% 
          mutate(seasonBreaks = case_when(episodeNumber == last(episodeNumber) ~ totalEpisodeNumber,
                                          T ~ 0)) %>% 
          filter(seasonBreaks != 0 & seasonBreaks != numEpisodes) %>% 
          ungroup() %>% 
          select(seasonBreaks) %>% 
          pull + 0.5
        
        pdf(NULL)
        
        g <- df_sub %>% 
          ggplot(aes(x = totalEpisodeNumber, y = y_var_moved)) +
          geom_line(linewidth = 0.8, color = color) +
          suppressWarnings(geom_point(text_aes, size = 1.1, shape = 16, color = point_color)) +
          geom_vline(xintercept = season_breaks_vec, linetype = "dashed", alpha = 0.85, color = "gray10", linewidth = 0.4) +
          scale_x_continuous(breaks = breaks_extended(12), expand = c(0.02, 0.02)) +
          scale_y_continuous(limits = limits, breaks = breaks) +
          labs(title = title,
               x = "Episode number (totally)",
               y = y_lab) +
          theme(axis.title.x = element_text(size = 15, color = "gray10"), axis.title.y = element_text(size = 15, color = "gray10"),
                axis.text.x = element_text(size = 12, color = "gray5"), axis.text.y = element_text(size = 12, color = "gray5"), 
                axis.ticks = element_blank(),
                axis.line = element_line(color = "gray35", linewidth = 1.4, linetype = "solid", lineend = "round"),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "#D6D6D6", linewidth = 1, linetype = "solid"),
                panel.background = element_rect(linetype = "solid", linewidth = 1, color = "darkgray", fill = "#F5F5F5"),
                plot.background = element_rect(fill = fill, linetype = "solid", linewidth = 1, color = "black"),
                plot.margin = margin(l = 1, b = 1, r = 1.5, t = 1,  unit = "cm"),
                plot.title = element_text(size = 18, color = "gray10", face = "bold"))
        
        
        p <- ggplotly(g, tooltip = "text") %>% 
          layout(annotations = list(x = 1, y = -0.1, 
                                    text = "Dashed vertical lines indicate breaks between another seasons", 
                                    showarrow = F, xref='paper', yref='paper', 
                                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                    font=list(size=12, color="#404040", family = "Arial")))
        
        dev.off()
        
        p
        
      }
      else{
        if(req(input$plots_menu) == "overall"){
          
          if(input$variable3 == "avg_rat"){
            y_var <- "seriesAvgRating"
            y_lab <- "Average rating"
            title <- "Average rating distribution for series released in different decades"
            breaks <- seq(5.5, 9, 0.5)
            limits <- c(5.2, 9.2)
            color_points <- "#204120"
            color_plot <- "#202e14"
            fill_plot <- "#8ad076"
            fill <- "#BEE5B3"
          }
          if(input$variable3 == "avg_runtime"){
            y_var <- "avgRuntime"
            y_lab <- "Average episode runtime (min)"
            title <- paste0("Average episode runtime distribution", "<br>", 
                      "for series released in different decades")
            breaks <- breaks_extended(9)
            limits <- c(0, 105)
            color_points <- "#541035"
            color_plot <- "#390b24"
            fill_plot <- "#D83A8F"
            fill <- "#f1bbd9"
          }
          
          pdf(NULL)
          
          g <- df_series_top1000 %>% 
            ggplot(aes(x = seriesStartDecade, y = get(y_var), color = seriesStartDecade, fill = seriesStartDecade))
          
          if(input$plot_type == "box"){
            g <- g +
              geom_boxplot(alpha = 0.9, outlier.color = color_points, 
                           outlier.size = 2.5, outlier.shape = 16, outlier.alpha = 0.9, 
                           color = color_plot, fill = fill_plot)
          }
          if(input$plot_type == "violin"){
            g <- g +
              geom_violin(alpha = 0.9, color = color_plot, fill = fill_plot)
            
            if(input$points){
              g <- g +
                suppressWarnings(geom_jitter(aes(x = seriesStartDecade, y = get(y_var),
                                text = paste("Title: ", seriesTitle, "<br>",
                                             y_lab, ": ", get(y_var))),
                                shape = 16, color = color_points, fill = color_points, size = 1.7, alpha = 0.8, width = 0.25, linewidth = 1))
            }
          }
          
          g <- g +
            coord_flip() +
            scale_y_continuous(limits = limits, breaks = breaks, expand = c(0, 0)) +
            labs(title = title,
                 y = y_lab,
                 x = "Decade") +
            theme(axis.title.x = element_text(size = 15, color = "gray10"), axis.title.y = element_text(size = 15, color = "gray10"),
                  axis.text.x = element_text(size = 12, color = "gray5"), axis.text.y = element_text(size = 12, color = "gray5"),
                  axis.ticks = element_blank(),
                  axis.line = element_line(color = "gray35", linewidth = 1.4, linetype = "solid", lineend = "round"),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_line(color = "#D6D6D6", linewidth = 1, linetype = "solid"),
                  panel.background = element_rect(linetype = "solid", linewidth = 1, color = "darkgray", fill = "#F5F5F5"),
                  legend.position = "none",
                  plot.background = element_rect(fill = fill, linetype = "solid", linewidth = 1, color = "black"),
                  plot.margin = margin(l = 1, b = 1, r = 1, t = 1.25,  unit = "cm"),
                  plot.title = element_text(size = 18, color = "gray10", face = "bold"))

          
          p <- ggplotly(g, tooltip = "text") %>% 
            layout(annotations = list(x = 1, y = -0.1,
                                      text = "Sample for plot consisted of 1000 most popular series which had at least 2 seasons",
                                      showarrow = F, xref='paper', yref='paper',
                                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                      font=list(size=12, color="#404040", family = "Arial")),
                   hoverlabel = list(bgcolor = color_points))
      
          dev.off()
          
          p
          
        }
        else{
          if(req(input$plots_menu) == "additional"){
            
            if(input$variable4 == "avg_rat"){
              x_var <- "seriesAvgRating"
              other_var <- "seriesAvgRating"
              x_lab <- "Average rating"
              title <- paste0("Average rating histogram for series released in ", input$decade)
              color_plot <- "#204120"
              fill_plot <- "#8ad076"
              fill <- "#BEE5B3"
              plot_aes <- aes(x = get(x_var), text = paste("Average rating in ", avgRatingBins, "<br>",
                                                           "Count: ", count, "<br>",
                                                           "Most popular series: ", mostPopSeries))
              x_var <- "avgRatingBins"
              binwidth <- 0.25
              center <- 0.125
              breaks <- seq(5.5, 9, 0.5)
              limits <- c(5.25, 9.25)
            }
            else{
            
              if(input$variable4 == "num_seasons"){
                x_var <- "numSeasons"
                other_var <- "lengthYears"
                x_lab <- "Number of seasons"
                other_lab <- "Average emission length in years: "
                title <- paste0("Number of seasons histogram for series released in ", input$decade)
                color_plot <- "#561310"
                fill_plot <- "#da3c33"
                fill <- "#f1b5b2"
              }
              if(input$variable4 == "emission"){
                x_var <- "lengthYears"
                other_var <- "numSeasons"
                x_lab <- "Emission length in years"
                other_lab <- "Average number of seasons: "
                title <- paste0("Emission length histogram for series released in ", input$decade)
                color_plot <- "#ad4a02"
                fill_plot <- "#FDAE61"
                fill <- "#fed4ac"
              }
              
              plot_aes <- aes(x = get(x_var), text = paste(x_lab, ": ", get(x_var), "<br>",
                                                           "Count: ", count, "<br>",
                                                           other_lab, otherVar, "<br>", 
                                                           "Most popular series: ", mostPopSeries))
              binwidth <- 1
              center <- 0
              breaks <- breaks_extended(10)
              limits <- c(min(df_series_sub3() %>% 
                                select({{x_var}}) %>% 
                                pull) - 0.5, 
                          max(df_series_sub3() %>% 
                                select({{x_var}}) %>% 
                                pull) + 0.5)
            }
            
            
            pdf(NULL)
            
            y_limits <- c(0, max(df_series_sub3()$count) * 20 / 19)
            
            df <- df_series_top1000 %>% 
              mutate(avgRatingBins = paste0("[", floor(4 * seriesAvgRating) / 4, ", ", floor(4 * seriesAvgRating)  / 4 + 0.25, ")")) %>% 
              group_by(get(x_var)) %>% 
              mutate(count = n(),
                     mostPopSeries = first(seriesTitle),
                     otherVar = round(mean(get(other_var)), 1)) %>% 
              ungroup
            
            if(input$variable4 == "avg_rat"){
              x_var <- "seriesAvgRating"
            }
            
            x_mean <- mean(df_series_sub3() %>%
                             select({{x_var}}) %>%
                             pull)
            
            x_mean_overall <- mean(df %>% 
                                     select({{x_var}}) %>% 
                                     pull)
            
            g <- df_series_sub3() %>% 
              ggplot(plot_aes) +
              geom_histogram(color = color_plot, fill = fill_plot, alpha = 0.95, 
                             binwidth = binwidth, center = center, linewidth = 0.8, closed = "left") +
              geom_vline(aes(xintercept = x_mean), linetype = "dashed", alpha = 0.99, color = "gray10", linewidth = 0.5)
            
            if(input$summary){
              g <- g +
                geom_histogram(data = df, aes(x = get(x_var)), 
                               color = color_plot, fill = fill_plot, alpha = 0.5, 
                               binwidth = binwidth, center = center, linewidth = 0.8, closed = "left") +
                geom_vline(aes(xintercept = x_mean_overall), linetype = "dashed", alpha = 0.5, color = "gray10", linewidth = 0.5)
              
              if(input$variable4 != "avg_rat"){
                limits <- c(min(df %>% 
                                  select({{x_var}}) %>% 
                                  pull) - 0.5, 
                            max(df %>% 
                                  select({{x_var}}) %>% 
                                  pull) + 0.5)
              }
              
              y_limits <- c(0, max(df$count) * 20 / 19)
            }                          

            g <- g +
              scale_x_continuous(limits = limits, breaks = breaks, expand = c(0.05, 0.05)) +
              scale_y_continuous(limits = y_limits, breaks = breaks_extended(6), expand = c(0, 0)) +
              labs(title = title,
                   x = x_lab,
                   y = "Count") +
              theme(axis.title.x = element_text(size = 15, color = "gray10"), axis.title.y = element_text(size = 15, color = "gray10"),
                    axis.text.x = element_text(size = 12, color = "gray5"), axis.text.y = element_text(size = 12, color = "gray5"),
                    axis.ticks = element_blank(),
                    axis.line = element_line(color = "gray35", linewidth = 1.4, linetype = "solid", lineend = "round"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color = "#D6D6D6", linewidth = 1, linetype = "solid"),
                    panel.background = element_rect(linetype = "solid", linewidth = 1, color = "darkgray", fill = "#F5F5F5"),
                    plot.background = element_rect(fill = fill, linetype = "solid", linewidth = 1, color = "black"),
                    plot.margin = margin(l = 1, b = 1, r = 1, t = 1.25,  unit = "cm"),
                    plot.title = element_text(size = 18, color = "gray10", face = "bold"))
            
            
            p <- ggplotly(g, tooltip = "text") %>% 
              layout(annotations = list(x = 1, y = -0.1,
                                        text = paste0("Data for plot consisted of ", nrow(df_series_sub3()), " series. Dashed vertical line indicates the mean value"),
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=12, color="#404040", family = "Arial")),
                     hoverlabel = list(bgcolor = color_plot))
            
            dev.off()
            
            p
            
          }
        }
      }
      
    }
    
  })
  
  output$table <- DT::renderDataTable({
    if(req(input$plots_menu) == "corr_plots"){
      req(input$range)
      DT::datatable(df_series_sub1_table(),
                    options = list(pageLength = 10),
                    rownames = F,
                    colnames = c("Base genre", "Number of series", "Average rating", "Median of ratings",
                                 "Average votes per episode", "Average episodes per season",
                                 "Most popular series"),
                    caption = paste("Showing info about series ", input$range[1], " - ", input$range[2], "from most popular ones list"))
      
    }
    else{
      if(req(input$plots_menu) == "ser_plots"){
        req(series())
        DT::datatable(df_series_sub2_table(),
                      options = list(pageLength = 8),
                      rownames = F,
                      colnames = c("Season", "Number of episodes", "Average rating", "Median of ratings",
                                   "Average votes per episode", "Total votes", "Release year"),
                      caption = paste("Showing info about '", series(), "' series seasons"))
      }
      else{
        if(req(input$plots_menu) == "additional"){
          req(input$decade)
          DT::datatable(df_series_sub3_table(),
                        options = list(pageLength = 10),
                        rownames = F,
                        colnames = c("Year", "Number of series released", "Average rating", "Average votes per episode", "Average total votes per series",
                                     "Average number of seasons per series","Average episode runtime (min)", 
                                     "Average emission length in years", "Most popular series"),
                        caption = paste("Detailed info about series released in ", input$decade))
        }
        else{
          if(req(input$plots_menu) == "overall"){
            df <- df_series_top1000 %>% 
              group_by(seriesStartDecade) %>% 
              arrange(desc(totalVotes)) %>% 
              mutate(mostPopSeries = first(seriesTitle),
                     avgRatingDecade = round(mean(seriesAvgRating), 2),
                     avgVotesDecade = round(mean(avgVotes), 0),
                     avgTotalVotesDecade = round(mean(totalVotes), 0),
                     avgNumSeasonsDecade = round(mean(numSeasons), 1),
                     avgRuntimeDecade = round(mean(avgRuntime), 1),
                     mostPopGenre = names(which.max(table(baseGenre))),
                     numSeriesDecade = n()) %>% 
              select(seriesStartDecade, numSeriesDecade, avgRatingDecade, avgVotesDecade, 
                     avgTotalVotesDecade, avgNumSeasonsDecade, avgRuntimeDecade, mostPopGenre, mostPopSeries) %>% 
              distinct(seriesStartDecade, .keep_all = T) %>% 
              arrange(seriesStartDecade)
            
            DT::datatable(df,
                          options = list(pageLength = 8),
                          rownames = F,
                          colnames = c("Decade", "Number of series released", "Average rating", "Average votes per episode", "Average total votes per series",
                                       "Average number of seasons per series", "Average episode runtime (min)", "Most common genre", "Most popular series"),
                          caption = paste("Summary info about 1000 most popular series which had at least 2 seasons"))
          }
        }
      }
    }
    
  })
}

###########################################################################################################################################

shinyApp(ui, server)
