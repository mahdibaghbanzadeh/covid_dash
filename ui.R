library(shiny)
library(shinydashboard)
library(highcharter)

dashboardPage(
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    # Add a CSS class to the sidebar content
    tags$head(
      tags$style(
        HTML(
          ".sidebar {
          position: fixed;
          width: inherit;
          height: 100%;
          background-color: #033C5A;
          }
          .skin-blue .main-header .navbar{
              background-color: #000000;
              background-image: url('https://avatars.githubusercontent.com/u/54924260?s=200&v=4');
              background-repeat: no-repeat;
              background-position: right;
              background-size: 3%;
          }
          .skin-blue .main-header .logo {
          background-color: #AA9868;
          color: #fff;
          border-bottom: 0 solid transparent;
          }
          .irs--shiny .irs-bar {
              top: 25px;
              height: 8px;
              border-top: 1px solid #AA9868;
              border-bottom: 1px solid #AA9868;
              background: #AA9868;
          }"
          )
      )
    ),
    div(
      class = "sidebar-content",
      selectInput(
        inputId = 'continent',
        label = 'Select Continenct',
        choices = c(
          'World',
          'Asia',
          'Africa',
          'Europe',
          'North America',
          'Oceania',
          'South America'
        )
      ),
      uiOutput('country_slice'),
      uiOutput('date_slice'),
      uiOutput('pop_slice'),
      p("Data Source:\n", a("ourworldindata.org",
                            href="https://ourworldindata.org/covid-cases",
                            target="_blank"))
    )
  ),
  dashboardBody(
    fluidRow(column(
      width = 12,
      highchartOutput('world_map_prev', height = '600px')
    )),
    fluidRow(
      column(width = 6, highchartOutput('cases')),
      column(width = 6, highchartOutput('death'))
    ),
    fluidRow(column(
      width = 12, highchartOutput('packed_bubble', height = '600px')
    ))
  )
)
