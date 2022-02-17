library(shiny)
library(tidyverse)
library(here)
library(readxl)
library(shinythemes)

# Pull in data
sample_mecs = read_xlsx(here("data", "sample_mecs_data.xlsx"))
long_mecs = sample_mecs %>% 
    gather(fuel, value, 4:9)
top_20 = sample_mecs %>% 
    filter(scenario == "Current Energy Consumption") %>% 
    filter(end_use == "TOTAL FUEL CONSUMPTION") %>% 
    mutate(total = net_electricity_demand + residual_fuel_oil + distillate_fuel_oil_and_diesel_fuel + 
               nat_gas + hgl_excluding_ng + coal_excluding_coke_and_breeze) %>% 
    slice_max(order_by = total, n = 20) %>% 
    gather(fuel, value, 4:10)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    navbarPage("US Industrial Decarbonization: BAT Efficiency Potentials", 
    
       ### START Tab 1                   
       tabPanel("Background on Data and Summary of Analyses", 
                sidebarLayout(
                    sidebarPanel(
                        strong("Data Background"),
                        p("This analyses visualizes the total potential energy savings available
                          to the US manufacturing sector through adoption of best available energy-effficiency
                          measures. The primary data source is the Energy Information Administations 
                          (EIA) Manufacturing Energy Consumption Surveys (MECS). These surveys provide the energy 
                          consumption by and use and fuel type for all manufacturing sectors (NAICS 311-339). Data from
                          these surveys were compiled to characterize the current (2018) energy consumption scenario."),
                        p("Currently, the Best Available Technology (BAT) adoption scenario is based on placeholder
                          coefficients for the consumpion reduction potential by end use and fuel type. Work is being
                          conducted to generate estimates for these coefficients be thermodynamic modelling of relevant 
                          systems and a review of literature on energy efficiency options."),
                        p("The figure to the right displays the total energy consumption for the 20 most energy 
                          intensive manufacturing sectors. Subsequent tabs display this consupmtion sorted by end use 
                          and fuel type for user-defined sectors and adoption rates. A characterization of energy-reduction
                          potential by fuel type for defined end uses is provided in the final tab."),
                        p("Data collection and analyses was performed by Jaxon Stuhr of the UCSB Bren School."),
                        strong("Data Citation:"),
                        p("cite MECS, energetics footprints, energy star, other literature here")
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Current Total Energy Usage of Top 20 Sectors",
                        plotOutput("total_consumption")
                    )
                ) #end of sidebar layout
        ), # end of tabPanel 1
       ### End Tab 1
       
       ### Start Tab 2
       tabPanel("Current Energy Consumption by Sector",
                sidebarLayout(
                    sidebarPanel(
                        "Fuel Consumption",
                        checkboxGroupInput(inputId = "pick_fuel_types",
                                           label = "Select Fuel Types: ",
                                           choices = unique(top_20$fuel)
                        ), # END CHECKBOX GROUP INPUT
                        "Top 20 Energy Consuming NAICS Sectors",
                        radioButtons(inputId = "selected_naics_code_1",
                                           label = "Select Sectors:",
                                           choices = unique(top_20$naics_code)
                        ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Current Total Energy Usage of Top 20 Sectors",
                        plotOutput("consumption_plot")
                    )
                ) # END OF SIDEBAR LAYOU
       ), # END OF TABPANEL "thing 1"),
       ### End Tab 3
       
       ### Start Tab 3
       tabPanel("Energy Consumption Scenario Comparison by End Use",
                sidebarLayout(
                    sidebarPanel(
                        "Choose BAT Adopion Rate",
                        sliderInput(inputId = "adpt_rate_1",
                                    label = "BAT Adoption Rates",
                                    min = 0,
                                    max = 100,
                                    value = 50
                                    ), # end slider input
                        "Top 20 Energy Consuming NAICS Sectors",
                        radioButtons(inputId = "selected_naics_code_2",
                                     label = "Select Sectors:",
                                     choices = unique(top_20$naics_code)
                        )
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Energy Consumption Reduction Potentialby End Use",
                        plotOutput("decarb_plot")
                        )
                    ) # END OF SIDEBAR LAYOU
                ), # END OF TABPANEL "thing 1"),
       ### End Tab 3
       
       ### Start Tab 4
       tabPanel("Energy Consumption Scenario Comparison by Fuel Type",
                sidebarLayout(
                    sidebarPanel(
                        "Choose BAT Adopion Rate",
                        sliderInput(inputId = "adpt_rate_2",
                                    label = "BAT Adoption Rates",
                                    min = 0,
                                    max = 100,
                                    value = 50
                        ), # end slider input
                        "Top 20 Energy Consuming NAICS Sectors",
                        radioButtons(inputId = "selected_naics_code_3",
                                     label = "Select Sectors:",
                                     choices = unique(top_20$naics_code)
                        )
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Energy Consumption Reduction Potential by Fuel Type",
                        plotOutput("fuel_plot")
                    )
                ) # END OF SIDEBAR LAYOU
       ), # END OF TABPANEL "thing 1"),
       ### End Tab 4
       
       ### Start Tab 5
       tabPanel("Energy Reduction Potential by Fuel Type for Selected End Use",
                sidebarLayout(
                    sidebarPanel(
                        "Sector Selector Radio Butons", 
                        radioButtons(inputId = "select_end_use",
                                           label = "Choose End Use:",
                                           choices = unique(long_mecs$end_use)
                        ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        "Reduction Potential Makeup by Fuel Types for Selected End Use", 
                        plotOutput("pie_chart")
                    )
                ) # END OF SIDEBAR LAYOUT), 
       ), # END OF TABPANEL "heat map"),
       ### End Tab 5
       
    ) # END OF NAVBAR PAGE
) # end of fluidPAGE

# Define server logic required to draw a histogram

server = function(input, output) {
    
    ### Start Tab 1 Reactive and Output ### 
    output$total_consumption = renderPlot(
        ggplot(data = top_20, aes(x = naics_code, y = value)) + 
            geom_col(position = "dodge", fill = "darkgreen", color = "black") 
        #  geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 1 Reactive and Output ### 
    
    ### Start Tab 2 Reactive and Output ### 
    total_consumption_reactive = reactive({
        top_20 %>%
            filter(naics_code == input$selected_naics_code_1) %>%
            filter(fuel %in% input$pick_fuel_types)
    }) # END "heat_map_reactive"
    output$consumption_plot = renderPlot(
        ggplot(data = total_consumption_reactive(), aes(x = fuel, y = value)) + 
            geom_col(position = "dodge", fill = "darkgreen", color = "black") 
        #  geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 2 Reactive and Output ### 
    
    ### Start Tab 3 Reactive and Output ### 
    decarb_reactive = reactive({
        long_mecs %>%
            filter(naics_code == input$selected_naics_code_2) %>% 
            filter(scenario %in% c("Current Energy Consumption", "Adoption of BATs")) %>% 
            group_by(end_use, scenario) %>% 
            summarise(value = sum(value) * input$adpt_rate_1)
    }) # END "decarb_reactive"
    output$decarb_plot = renderPlot(
        ggplot(data = decarb_reactive(), aes(x = end_use, y = value, fill = scenario)) +
            geom_col(position = "dodge")
    ) # END "output$sw_plot
    ### End Tab 3 Reactive and Output ### 
    
    ### Start Tab 4 Reactive and Output ### 
    fuel_type_reactive = reactive({
        long_mecs %>%
            filter(naics_code == input$selected_naics_code_3) %>% 
            filter(scenario %in% c("Current Energy Consumption", "Adoption of BATs")) %>% 
            group_by(fuel, scenario) %>% 
            summarise(value = sum(value) * input$adpt_rate_2)
    }) # END "decarb_reactive"
    output$fuel_plot = renderPlot(
        ggplot(data = fuel_type_reactive(), aes(x = fuel, y = value, fill = scenario)) +
            geom_col(position = "dodge")
    ) # END "output$sw_plot
    ### End Tab 4 Reactive and Output ### 
    
    ### Start Tab 5 Reactive and Output ### 
    pie_chart_reactive = reactive({
        long_mecs %>%
            filter(end_use == input$select_end_use) %>% 
            filter(scenario == "Differential") %>% 
            group_by(fuel) %>% 
            summarise(value = sum(value))
    }) # END "heat_map_reactive"
    output$pie_chart = renderPlot(
        ggplot(data = pie_chart_reactive(), aes(x = "", y = value, fill = fuel)) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) + 
            theme_void() 
          #  geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 5 Reactive and Output ### 
    
}

# Run the application 
shinyApp(ui = ui, server = server)

