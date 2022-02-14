library(shiny)
library(tidyverse)
library(here)
library(readxl)
library(shinythemes)

# Pull in data
sample_mecs = read_xlsx(here("data", "sample_mecs_data.xlsx"))
long_mecs = sample_mecs %>% 
    gather(fuel, value, 4:9)

# Define UI for application that draws a histogram
ui <- fluidPage(
     theme = shinytheme("sandstone"),
    navbarPage("US Industrial Decarbonization: BAT Efficiency Potentials", 
    
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
                                           choices = unique(long_mecs$end_use)
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
       tabPanel("Energy Reduction Potential by End Use for Selected Sector",
                sidebarLayout(
                    sidebarPanel(
                        "Sector Selector Radio Butons", 
                        radioButtons(inputId = "select_sector",
                                           label = "Choose Sector:",
                                           choices = unique(long_mecs$naics_code)
                        ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Pie Chart Output Here", 
                        plotOutput("pie_chart")
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
        long_mecs %>%
            filter(end_use %in% input$pick_end_uses) %>% 
            filter(scenario == "Differential") %>% 
            group_by(end_use, fuel) %>% 
            summarise(value = sum(value) * input$adpt_rate)
    }) # END "decarb_reactive"
    output$decarb_plot = renderPlot(
        ggplot(data = decarb_reactive(), aes(x = end_use, y = value, fill = fuel)) +
            geom_col(position = "dodge")
    ) # END "output$sw_plot
    ### End Tab 3 Reactive and Output ### 
    
    ### Start Tab 4 Reactive and Output ### 
    pie_chart_reactive = reactive({
        long_mecs %>%
            filter(naics_code == input$select_sector) %>% 
            filter(scenario == "Differential") %>% 
            filter(end_use != "TOTAL FUEL CONSUMPTION") %>% 
            group_by(end_use) %>% 
            summarise(value = sum(value))
    }) # END "heat_map_reactive"
    output$pie_chart = renderPlot(
        ggplot(data = pie_chart_reactive(), aes(x = "", y = value, fill = end_use)) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) + 
            theme_void() +
            geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 4 Reactive and Output ### 
    
}

# Run the application 
shinyApp(ui = ui, server = server)

