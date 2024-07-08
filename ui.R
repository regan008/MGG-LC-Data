library(shiny)
library(plotly)
library(DT)
navbarPage("GG MGG Explorer", id="nav",       
    tabPanel("Interactive map",
        div(class="outer",
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        
        plotlyOutput("map"),
                                  
        checkboxInput("gg", "Gaia's Guide", TRUE),
        
        checkboxInput("mgg", "Bob Damron's Address Book", TRUE),
        
        checkboxInput("noncontiguous", "Include non-contiguous states", FALSE),
        
        selectInput("cityvalue", "Select Value", choices = unique(mgg.gg.data$geocode.value)),

        DTOutput('dtable')
           ),
),
)

#5F4690,#1D6996,#38A6A5,#0F8554,#73AF48,#EDAD08,#E17C05,#CC503E,#94346E,#6F4070,#994E95,#666666
