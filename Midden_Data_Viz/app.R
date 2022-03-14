library(shiny)
library(tidyverse)
library(here)
library(readxl)
library(shinythemes)
library(forcats)
library(broom)
library(janitor)
library(sf)
library(tmap)

# Pull in data
sample_mecs = read_xlsx(here("data", "sample_mecs_data.xlsx"))

naics_labels = read_xlsx(here("data", "naics_codes.xlsx")) %>% 
    select(naics_code = "2017 NAICS US   Code", name = "2017 NAICS US Title")

long_mecs = sample_mecs %>% 
    gather(fuel, value, 4:9)  %>% 
    merge(naics_labels, by = "naics_code")
    
top_15 = sample_mecs %>% 
    filter(scenario == "Current Energy Consumption") %>% 
    filter(end_use == "TOTAL FUEL CONSUMPTION") %>% 
    mutate(total = net_electricity_demand + residual_fuel_oil + distillate_fuel_oil_and_diesel_fuel + 
               nat_gas + hgl_excluding_ng + coal_excluding_coke_and_breeze) %>% 
    slice_max(order_by = total, n = 15) %>% 
    gather(fuel, value, 4:10) %>% 
    merge(naics_labels, by = "naics_code") %>% 
    mutate(name = fct_reorder(name, value, .fun='sum'))


us_regions_sf = read_sf(here("data", "us_regions", "jaxon_conus.shp"))

regions_sf_sub = us_regions_sf %>%
    clean_names() %>% 
    select(region = name)

regional_mecs = read_xlsx(here("data", "regional_mecs_data.xlsx"))

regional_totals = regional_mecs %>% 
    filter(end_use == "TOTAL FUEL CONSUMPTION") %>% 
    mutate(net_electricity_demand = as.numeric(net_electricity_demand), 
           residual_fuel_oil = as.numeric(residual_fuel_oil), 
           diesel_fuel = as.numeric(diesel_fuel),
           gas = as.numeric(gas),
           natural_gas = as.numeric(natural_gas),
           coke_and_breeze = as.numeric(coke_and_breeze)) 

full_sf = merge(regions_sf_sub, regional_totals, by = "region") %>% 
    gather(fuel, value, 3:8)


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
                          consumption by end-use and fuel type for all manufacturing sectors (NAICS 311-339). Data from
                          these surveys were compiled to characterize the current (1518) energy consumption scenario."),
                        p("Currently, the Best Available Technology (BAT) adoption scenario is based on placeholder
                          coefficients for the consumpion reduction potential by end use and fuel type. Work is being
                          conducted to generate estimates for these coefficients be thermodynamic modelling of relevant 
                          systems and a review of literature on energy efficiency improvement options."),
                        p("The first figure (Tab 2) shows energy consumption by fuel type for the four US census regions. Subsequent tabs display 
                          the current (1518) energy consumption by sector for the most intensive sectors, savings potential by end use and fuel type 
                          with a user-defined BAT adoption rate, and a pite-chart of fuel_types with the greatest savings potential for a given end use."),
                        p("Data collection and analyses was performed by Jaxon Stuhr of the UCSB Bren School."),
                        strong("Data Citation:"),
                        p("cite MECS, energetics footprints, energy star, other literature here")
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        img(src = "industry_pic.jpg")
                        #"Current Total Energy Usage of Top 15 Sectors",
                        #plotOutput("total_consumption")
                    )
                ) #end of sidebar layout
        ), # end of tabPanel 1
       ### End Tab 1
       
       ### Start Tab 2
       tabPanel("Energy Consumption by Census Region and Fuel",
                sidebarLayout(
                    sidebarPanel(
                        "Fuel Consumption",
                        checkboxGroupInput(inputId = "region_fuel",
                                           label = "Select Fuel Types: ",
                                           choices = unique(full_sf$fuel),
                                           selected = full_sf$fuel[1]
                        )
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        strong("Energy Consumption by Census Region and Fuel"),
                        plotOutput("region_plot")
                    )
                ) # END OF SIDEBAR LAYOU
       ), # END OF TABPANEL "thing 1"),
       ### End Tab 3
       
       ### Start Tab 2
       tabPanel("Current Energy Consumption by Sector",
                sidebarLayout(
                    sidebarPanel(
                        "Fuel Consumption",
                        checkboxGroupInput(inputId = "pick_fuel_types",
                                           label = "Select Fuel Types: ",
                                           choices = unique(top_15$fuel),
                                           selected = top_15$fuel[1]
                        )
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        strong("Current Total Energy Consumption of Top 15 NAICS Sectors"),
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
                        "Top 15 Energy Consuming NAICS Sectors",
                        radioButtons(inputId = "selected_naics_code_2",
                                     label = "Select Sectors:",
                                     choices = unique(top_15$name)
                        )
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        strong("Energy Consumption Reduction Potential by End Use"),
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
                        "Top 15 Energy Consuming NAICS Sectors",
                        radioButtons(inputId = "selected_naics_code_3",
                                     label = "Select Sectors:",
                                     choices = unique(top_15$name)
                        )
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        strong("Energy Consumption Reduction Potential by Fuel Type"),
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
                        selectInput(inputId = "select_end_use",
                                           label = "Choose End Use:",
                                           choices = unique(long_mecs$end_use)
                        ) # END CHECKBOX GROUP INPUT
                    ), # END OF SIDEBAR PANEL
                    mainPanel(
                        strong("Reduction Potential Makeup by Fuel Types for Selected End Use"), 
                        plotOutput("pie_chart")
                    )
                ) # END OF SIDEBAR LAYOUT), 
       ), # END OF TABPANEL "heat map"),
       ### End Tab 5
       
    ) # END OF NAVBAR PAGE
) # end of fluidPAGE

# Define server logic required to draw a histogram

server = function(input, output) {
    
    ### Start Tab 2 Reactive and Output ### 
    region_reactive = reactive({
        full_sf %>% 
            filter(fuel %in% input$region_fuel)  %>% 
            group_by(region) %>% 
            summarise(value = sum(value))
    }) # END "heat_map_reactive"
    output$region_plot = renderPlot(
        ggplot() + 
            geom_sf(data = region_reactive(), aes(fill = value))  + 
           # geom_sf_label(data = full_sf, aes(label = region)) +
            theme_minimal() + 
            labs(x = "", y = "") +
            scale_fill_gradientn(colors = c("blue","purple"))
        #  geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 2 Reactive and Output ### 
    
    ### Start Tab 2 Reactive and Output ### 
    total_consumption_reactive = reactive({
        top_15 %>%
            filter(fuel %in% input$pick_fuel_types)  %>% 
            mutate(name = fct_reorder(name, desc(value), .fun = 'sum'))
    }) # END "heat_map_reactive"
    output$consumption_plot = renderPlot(
        ggplot(data = total_consumption_reactive(), aes(x = name, y = value, fill = fuel)) + 
            geom_col(position = "stack", color = "black") +
            theme_minimal() +
            coord_flip() + 
            labs(x = "NAICS Sector", y = "Energy Consumption \n [TBtu]", fill = "Fuel Type")
        #  geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 2 Reactive and Output ### 
    
    ### Start Tab 3 Reactive and Output ### 
    decarb_reactive = reactive({
        long_mecs %>%
            filter(name == input$selected_naics_code_2) %>% 
            filter(scenario %in% c("Current Energy Consumption", "Adoption of BATs")) %>% 
            group_by(end_use, scenario) %>% 
            summarise(value = sum(value) * input$adpt_rate_1)
    }) # END "decarb_reactive"
    output$decarb_plot = renderPlot(
        ggplot(data = decarb_reactive(), aes(x = end_use, y = value, fill = scenario)) +
            geom_col(position = "dodge", color = "black") + 
            theme_minimal() +
            coord_flip() +
            labs(x = "End Use", y = "Energy Consumption \n [TBtu]", fill = "Scenario")
    ) # END "output$sw_plot
    ### End Tab 3 Reactive and Output ### 
    
    ### Start Tab 4 Reactive and Output ### 
    fuel_type_reactive = reactive({
        long_mecs %>%
            filter(name == input$selected_naics_code_3) %>% 
            filter(scenario %in% c("Current Energy Consumption", "Adoption of BATs")) %>% 
            group_by(fuel, scenario) %>% 
            summarise(value = sum(value) * input$adpt_rate_2)
    }) # END "decarb_reactive"
    output$fuel_plot = renderPlot(
        ggplot(data = fuel_type_reactive(), aes(x = fuel, y = value, fill = scenario)) +
            geom_col(position = "dodge", color = "black") + 
            theme_minimal() +
            coord_flip() +
            labs(x = "Fuel Type", y = "Energy Consumption \n [TBtu]", fill = "Scenario")
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
            geom_bar(stat="identity", width=1, color="black") +
            coord_polar("y", start=0) + 
            theme_minimal() +
            labs(x = "", y = "Energy Consumption \n [TBtu]",  fill = "Fuel Type")
          #  geom_text(aes(label = value), color = "white", size=6) 
    )
    ### End Tab 5 Reactive and Output ### 
    
}

# Run the application 
shinyApp(ui = ui, server = server)

