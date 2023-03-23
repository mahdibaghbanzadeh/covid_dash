library(shiny)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(highcharter)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  df = reactive({
    df = fread('owid-covid-data.csv')
    df = df %>% filter(continent != "")
    print(summary(df$population))
    df[is.na(df)] = 0
    df
  })
  output$tab = renderTable({
    str(df())
  })
  output$date_slice <- renderUI({
    min_date = min(df()$date)
    max_date = max(df()$date)
    dateRangeInput(
      inputId = 'date_range',
      label = 'Select date range',
      min = min_date,
      max = max_date,
      start = '2022-01-01',
      end = '2022-06-01'
    )
  })
  output$pop_slice <- renderUI({
    min_pop = min(df()$population)
    max_pop = max(df()$population)
    sliderInput(
      inputId = 'pop_range',
      label = 'Select popelation range',
      min = min_pop,
      max = max_pop,
      value = c(min_pop, max_pop)
    )
  })
  output$country_slice <- renderUI({
    selectInput(
      inputId = 'country',
      label = 'Select Countries',
      multiple = TRUE,
      selected = 'Iran',
      choices = unique(df()$location)
    )
  })
  
  output$world_map_prev <- renderHighchart({
    req(input$pop_range)
    map_data = list(
      'World' = 'custom/world-highres2',
      'Asia' = 'custom/asia',
      'Africa' = 'custom/africa',
      'Europe' = 'custom/europe',
      'North America' = 'custom/north-america',
      'Oceania' = 'custom/oceania',
      'South America' = 'custom/south-america'
    )
    print(input$pop_range)
    df = df() %>%
      filter(between(date, input$date_range[1], input$date_range[2])) %>%
      filter(between(
        population,
        as.numeric(input$pop_range[1]),
        as.numeric(input$pop_range[2])
      )) %>%
      filter(input$continent == 'World' |
               continent == input$continent) %>%
      select(iso_code, location, new_cases, population) %>%
      group_by(iso_code, location) %>%
      summarise(
        total_cases_per = sum(new_cases) * 100 / max(population),
        population = max(population),
        new_cases = sum(new_cases)
      ) %>%
      data.frame()
    
    hcmap(
      map_data[input$continent],
      #"custom/world-highres2",
      data = df,
      value = "total_cases_per",
      joinBy = c("iso-a3", "iso_code"),
      name = "Prevalence",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}",
        fontSize = 10
      ),
      borderColor = "#FAFAFA",
      borderWidth = 0.5,
      tooltip =  list(valueDecimals = 1,
                      valuePrefix = '% ')
    ) %>%
      hc_colorAxis(minColor = "#F5DEB3",
                   # minimum color for the color axis
                   maxColor = "#FF4500" # maximum color for the color axis
                     ) %>%
                     hc_title(
                       text = paste0(
                         'Prevalence of COVID-19 in ',
                         input$continent,
                         '<br>',
                         paste(input$date_range, collapse = '-')
                       ),
                       margin = 20,
                       align = "center",
                       style = list(color = "#033C5A", useHTML = TRUE)
                     ) %>%
                       hc_mapNavigation(enabled = TRUE) %>%
                       hc_exporting(enabled = TRUE, # always enabled
                                    filename = "custom-file-name")
                     
  })
    
    output$cases <- renderHighchart({
      req(input$country)
      
      df = df() %>%
        filter(between(date, input$date_range[1], input$date_range[2])) %>%
        filter(between(
          population,
          as.numeric(input$pop_range[1]),
          as.numeric(input$pop_range[2])
        )) %>%
        filter(location %in% input$country) %>%
        select(location, date, new_cases_per_million) %>%
        data.frame()
      
      df %>% hchart("line",
                    hcaes(x = date, y = new_cases_per_million, group = location)) %>%
        hc_title(
          text = 'Daily cases (per million) of COVID-19',
          margin = 20,
          align = "center",
          style = list(color = "#033C5A", useHTML = TRUE)
        ) %>%
        hc_exporting(enabled = TRUE, # always enabled
                     filename = "custom-file-name")
    })
    
    output$death <- renderHighchart({
      req(input$country)
      
      df = df() %>%
        filter(between(date, input$date_range[1], input$date_range[2])) %>%
        filter(between(
          population,
          as.numeric(input$pop_range[1]),
          as.numeric(input$pop_range[2])
        )) %>%
        filter(location %in% input$country) %>%
        select(location, date, new_deaths_per_million) %>%
        data.frame()
      
      df %>% hchart("line",
                    hcaes(x = date, y = new_deaths_per_million, group = location)) %>%
        hc_title(
          text = 'Daily deaths (per million) of COVID-19',
          margin = 20,
          align = "center",
          style = list(color = "#033C5A", useHTML = TRUE)
        ) %>%
        hc_exporting(enabled = TRUE, # always enabled
                     filename = "custom-file-name")
    })
    
    output$packed_bubble <- renderHighchart({
      req(input$pop_range)
      df = df() %>%
        filter(between(date, input$date_range[1], input$date_range[2])) %>%
        filter(between(
          population,
          as.numeric(input$pop_range[1]),
          as.numeric(input$pop_range[2])
        )) %>%
        select(continent, location, total_cases_per_million) %>%
        group_by(continent, location) %>%
        summarise(total_cases_per_million = round(max(total_cases_per_million), 2)) %>%
        data.frame()
      
      hc <- hchart(
        df,
        "packedbubble",
        hcaes(
          name = location,
          value = total_cases_per_million,
          group = continent
        )
      )
      
      q95 <- as.numeric(quantile(df$total_cases_per_million, .95))
      
      hc %>%
        hc_tooltip(useHTML = TRUE,
                   pointFormat = "<b>{point.name}:</b> {point.value}") %>%
        hc_plotOptions(
          packedbubble = list(
            maxSize = "150%",
            zMin = 0,
            layoutAlgorithm = list(
              gravitationalConstant =  0.05,
              splitSeries =  TRUE,
              # TRUE to group points
              seriesInteraction = TRUE,
              dragBetweenSeries = TRUE,
              parentNodeLimit = TRUE
            ),
            dataLabels = list(
              enabled = TRUE,
              format = "{point.name}",
              filter = list(
                property = "y",
                operator = ">",
                value = q95
              ),
              style = list(
                color = "black",
                textOutline = "none",
                fontWeight = "normal"
              )
            )
          )
        ) %>%
        hc_title(
          text = paste0(
            'Distribution of total cases (per million)',
            '<br>',
            paste(input$date_range, collapse = '-')
          ),
          margin = 20,
          align = "center",
          style = list(color = "#033C5A", useHTML = TRUE)
        ) %>%
        hc_exporting(enabled = TRUE, # always enabled
                     filename = "custom-file-name")
      
    })
})
  