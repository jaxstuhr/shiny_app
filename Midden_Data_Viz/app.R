library(shiny)
library(tidyverse)
library(here)
library(readxl)
library(shinythemes)

# Pull in data
midden_data = read_xlsx(here("data", "midden_data.xlsx"))
sectors = c("Chemicals", "Iron and Steel", "Aluminum", "Food and Bevarage", "Cement", "Electronics", "Pulp and Paper")

# Define UI for application that draws a histogram
ui <- fluidPage(
     theme = shinytheme("sandstone"),
    navbarPage("Industrial Decarbonization: BAT Efficiency Potentials", 
    
       ### START Tab 1                   
       tabPanel("Background on Data and Summary of Analyses"),
       ### End Tab 1
       
       ### Start Tab 2
       tabPanel("Sample Tab", 
                sidebarLayout(
                    sidebarPanel(
                        "Widgets go here", 
                        checkboxGroupInput(inputId = "pick_species",
                                           label = "Choose Species: ",
                                           choices = unique(starwars$species)
                                           ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Output goes here", 
                        plotOutput("sw_plot")
                    )
                ) # END OF SIDEBAR LAYOUT
                ), # END OF TABPANEL "thing 1"
       ### End Tab 2
       
       ### Start Tab 3
       tabPanel("Energy Consumption Scenario Comparison",
                sidebarLayout(
                    sidebarPanel(
                        "Widgets go here",
                        sliderInput(inputId = "adpt_rate",
                                    label = "BAT Adoption Rates",
                                    min = 0,
                                    max = 100,
                                    value = 50
                                    ), # end slider input
                        checkboxGroupInput(inputId = "pick_end_uses",
                                           label = "Select End Uses: ",
                                           choices = unique(midden_data$main_output_name)
                                           ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Output goes here",
                        plotOutput("decarb_plot")
                        )
                    ) # END OF SIDEBAR LAYOU
                ), # END OF TABPANEL "thing 1"),
       ### End Tab 3
       
       ### Start Tab 4
       tabPanel("Nation Wide Energy Consumption Heat Map",
                sidebarLayout(
                    sidebarPanel(
                        "Sector Selector Radio Butons", 
                        radioButtons(inputId = "pick_sectors",
                                           label = "Choose Sectors: ",
                                           choices = sectors
                        ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Heat Map Output Here", 
                        plotOutput("heat_map")
                    )
                ) # END OF SIDEBAR LAYOUT), 
       ), # END OF TABPANEL "heat map"),
       ### End Tab 4
       
       ### Start Tab 5
       tabPanel("Energy Savings Potential by End Use")
       ### End Tab 5
       
          ) # END OF NAVBAR PAGE
) # end of fluidPAGE

# Define server logic required to draw a histogram

server = function(input, output) {
    
    ### Start Tab 2 Reactive and Output ### 
    sw_reactive = reactive({
        starwars %>% 
            filter(species %in% input$pick_species)
    }) # END "sw_reactive"
    output$sw_plot = renderPlot(
        ggplot(data = sw_reactive(), aes(x = mass, y = height)) + 
            geom_point(aes(color = species))
    ) # END "output$sw_plot
    ### End Tab 2 Reactive and Output ### 
    
    ### Start Tab 3 Reactive and Output ### 
    decarb_reactive = reactive({
        midden_data %>%
            filter(main_output_name %in% input$pick_end_uses)
    }) # END "decarb_reactive"
    output$decarb_plot = renderPlot(
        ggplot(data = decarb_reactive(), aes(x = main_output_name, y = input_1_amount)) +
            geom_col(aes(color = main_output_name))
    ) # END "output$sw_plot
    ### End Tab 3 Reactive and Output ### 
    
    ### Start Tab 4 Reactive and Output ### 
    heat_map_reactive = reactive({
        midden_data %>%
            filter(main_output_name %in% input$sectors)
    }) # END "heat_map_reactive"
    output$heat_map = renderPlot(
        ggplot() 
    )
    ### End Tab 4 Reactive and Output ### 
    
}

# Run the application 
shinyApp(ui = ui, server = server)

