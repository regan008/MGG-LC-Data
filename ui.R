library(shiny)
library(plotly)
library(DT)

navbarPage("GG MGG Explorer", id="nav",       
           tabPanel("Interactive map",
                    div(class="outer",
                        fluidRow(
                          column(8,
                                 div(style = "width: 800px;",  # Set a fixed width for the map
                                     plotlyOutput("map")
                                 ),
                                 div(id="custom_legend",
                                     style = "margin-top: 20px;",  # Add a top margin to move the legend down
                                     div(class="legend_item",
                                         div(class="legend_color", style="background-color: #0F8554; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"),
                                         div(class="legend_text", "Gaia's Guide", style="display: inline-block;")
                                     ),
                                     div(class="legend_item",
                                         div(class="legend_color", style="background-color: #6F4070; width: 20px; height: 20px; display: inline-block; margin-right: 5px;"),
                                         div(class="legend_text", "Bob Damron's Address Book", style="display: inline-block;")
                                     )
                                 ),
                                 div(style = "margin-top: 20px;",  # Add a top margin to move the selectInput down
                                     selectInput("cityvalue", "Select Value", choices = unique(mgg.gg.data$geocode.value))
                                 )
                          ),
                          column(4,
                                 checkboxInput("gg", "Gaia's Guide", TRUE),
                                 checkboxInput("mgg", "Bob Damron's Address Book", TRUE),
                                 checkboxInput("noncontiguous", "Include non-contiguous states", FALSE)
                          )   
                        ),
                        DTOutput('dtable')
                    )
           )
)