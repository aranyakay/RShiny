library(shinydashboard)
library(shiny)
library(shinyjs)
library(DT)

# choices_state <- c("National",  
#                  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
#                  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA" ,"ME",
#                  "MD", "MA", "MI", "MN", "MS", "MO" ,"MT", "NE", "NV", "NH",
#                  "NJ", "NM", "NY", "NC", "ND" ,"OH", "OK", "OR", "PA", "RI",
#                  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WI", "WV",
#                  "WY")
choices_state <- list("National" = "National",
                    "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ",
                    "Arkansas" = "AR", "California" = "CA", "Colorado" = "CO",
                    "Connecticut" = "CT", "Delaware" = "DE", 
                    "District of Columbia" = "DC", "Florida" = "FL",
                    "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
                    "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
                    "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA",
                    "Maine" = "ME", "Maryland" = "MD", "Massachusetts" = "MA",
                    "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                    "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE",
                    "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ",
                    "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC",
                    "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
                    "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI",
                    "South Carolina" = "SC", "South Dakota" = "SD",
                    "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
                    "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA",
                    "West Virginia" = "WV","Wisconsin" = "WI", "Wyoming" = "WY")

choices_model <- c("Passenger Vehicle - IIHS", 
                   "Passenger Vehicle - ESC",
                   "Passenger Vehicle - NCAP",
                   "Pedestrian", 
                   "Motorcycle",
                   "Intersection")


shinyUI(
  
  fluidPage(
    dashboardPage(
      dashboardHeader(title = "Fatality Forecasting"),
      dashboardSidebar(
        useShinyjs(),
        selectInput("ModelInput", "Models",
                    choices = choices_model,
                    selected= "Passenger Vehicle 1"),
        selectInput("stateInput", "Region",
                    choices = choices_state,
                    selected= "National")
        ,div(style="padding: 12px 15px 0px 15px;"
             ,uiOutput(outputId="model_param"))
        ,div(style="margin-left:16px;font-weight: bold; ", "Attributes")
        ,uiOutput(outputId="buttons")
        ),
      
      dashboardBody(
        useShinyjs(),
        fluidRow(
          column(4,
          tags$style(HTML(".irs-single {font-size: 0.9em; top: -1px;}")),
          box(
            div(style="display:inline-block;width:50%;text-align: right;",
                 actionButton("button3", label = "Forecast"
                              ,style="padding:4px; width: 100%;"))
            ,div(style="display:inline-block;width:49%;text-align: left;"
                ,uiOutput(outputId="reset_all")
                )
            ,div(htmlOutput("betweenBtnSld1"))
            ,splitLayout(cellWidths = c("70%", "30%"),
              div(
                tags$head(tags$style(
                          HTML(".irs-slider {height: 80px}
                                .irs-min {visibility: hidden !important;}
                                .irs-max {visibility: hidden !important;}"))),
                style="display:inline-block;width:99%;
                       text-align:center;font-size:90%;",
                uiOutput(outputId="sliders", inline=T))
              ,div(
                tags$head(tags$style(HTML(".small-box {height: 58px;
                                          background-color: white !important;
                                          color: #000000 !important;
                                          box-shadow: 0 0px 0px;
                                          font-size: 0.3em;
                                          }"))),
                style="display:inline-block;width:99%;text-align:center;",
                uiOutput("more")
                )
              ),width = 12
            ) # end first box
          )
          ,column(8,
          tabBox(side = "left", selected = uiOutput(outputId="tab_full_name")
                  ,tabPanel(uiOutput(outputId="tab_full_name")
                            ,uiOutput(outputId="plot.ui"))
                  ,tabPanel("Attributes",
                            uiOutput(outputId="plot.Attb"))
                  ,tabPanel("By State",
                            div(DT::dataTableOutput("table3")
                                ,style = "font-size:110%;text-align:center;"))
                  ,width = 12
                  ) # end of tabBox
          ,box(
            div( DT::dataTableOutput("table2")
                ,style = "font-size:120%;text-align:center;"
                ),width = 12
            )
          )
        ) # end dashboardBody fluidRow
        ) # end dashboardBody
      ) # end dashboardPage
    ) # end fluidPage
) # end shinyUI
