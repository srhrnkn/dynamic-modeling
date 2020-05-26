
#

library(shiny)
library(tidyverse)
library(magrittr)

#todo: add functions for holds placement
#build additional lag for nonhold checkouts


# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Single Title Holds Model"),

    # Sidebars with a slider input
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "holds_ratio",
                        "Holds Ratio:",
                        min = 0,
                        max = 50,
                        value = 3,),
            sliderInput(inputId = "initial_buy",
                        "Initial license purchase:",
                        min = 0,
                        max = 50,
                        value = 3,
                        round = T),
            sliderInput(inputId = "checkout_duration",
                    "Checkout duration:",
                    min = 0,
                    max = 50,
                    value = 21,
                    round = T),
            sliderInput(inputId = "prob_nh",
                        "Probability of nonhold checkout:",
                        min = 0,
                        max = 1,
                        value = .1),
            sliderInput(inputId = "simtime",
                        "Days to project",
                        min = 0,
                        max = 730,
                        value = 180),
            sliderInput(inputId = "top_holds_placed",
                        "Top daily holds placed value",
                        min = 0,
                        max = 200,
                        value = 20),
            selectInput(inputId = "holds_placed_function",
                        label = "Hold placement pattern",
                        choices = list(`taper` = "taper",
                                       `bell` = "bell",
                                       `steady` = "steady"))
    ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("modelPlot") #,
           #DT::dataTableOutput("hmodel")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$modelPlot <- renderPlot({
        
        holds_ratio <- input$holds_ratio
        initial_buy <- input$initial_buy
        checkout_duration <- input$checkout_duration
        prob_nh <- input$prob_nh
        simtime <- input$simtime
        top_holds_placed <- input$top_holds_placed
        bell <- function(x,max_val){
            max_val*exp(-(x-5-10)^2/30)}
        taper <- function(x,max_val){
            #shift right
            x <- x-20
            x <- x/20
            (c*exp(-5*(x))/(c*exp(a*(x))+(1-.7)))*max_val}
        steady <- function(x,max_val){
            max_val
        }
        
        holds_placed_function <- get(input$holds_placed_function)
        attributes(holds_placed_function)$name <- input$holds_placed_function
        
        for(i in 1:simtime){
            if(i==1){
                hmodel <- tibble(day = i,
                                 licenses_avail=0,
                                 licenses_out=0,
                                 holds_placed = 0,
                                 nonhold_checkouts = 0,
                                 checkins = 0,
                                 holds_filled = 0,
                                 hold_count = 0,
                                 total_licenses = licenses_avail+licenses_out,
                                 current_ratio = hold_count/total_licenses,
                                 licenses_bought = initial_buy
                )
            }
            else{
                temph <- tail(hmodel,1)
                
                temph %<>% 
                    mutate(
                        day = i,
                        #calc holds placed for this day
                        holds_placed = holds_placed_function(x = i,max_val=top_holds_placed),
                        #calc new avail licenses at start of step based on vals from previous step
                        licenses_avail = licenses_avail + licenses_bought + checkins -
                            holds_filled - nonhold_checkouts,
                        #calc new checked out licenses based on vals from previous step
                        licenses_out = licenses_out + holds_filled + nonhold_checkouts - checkins,
                        #checkouts from nonhold if any are available -use the lowest of today's and yesterday's value
                        nonhold_checkouts = round(prob_nh*min(c(licenses_avail,hmodel$licenses_avail[i-1])),0),
                        #check in titles from [checkout_duration] days ago
                        checkins = replace_na(hmodel$holds_filled[max(i-checkout_duration,1)],0) +
                            replace_na(hmodel$nonhold_checkouts[max(i-checkout_duration,1)],0),
                        #calc new holds filled
                        holds_filled = pmin(hold_count,licenses_avail),
                        #calc total checkouts
                        checkouts=holds_filled+nonhold_checkouts,
                        #calc new total holds
                        hold_count = hold_count + holds_placed - holds_filled,
                        #licenses that exist at this step
                        total_licenses = licenses_avail+licenses_out,
                        #current hold ratio
                        current_ratio = hold_count/total_licenses,
                        #buy new licenses if necessary to stay under ratio
                        licenses_bought = ceiling(pmax(hold_count/holds_ratio-total_licenses,0))
                    )
                
                
                hmodel %<>% bind_rows(temph)
                
                rm(temph)
            }
        }
        
        hmodel %>% select(day,total_licenses,licenses_avail,licenses_out,holds_placed,holds_filled,current_ratio,licenses_bought,hold_count,checkins,nonhold_checkouts,checkouts) %>% 
            pivot_longer(cols = -day,names_to = "measure",values_to = "value") %>% 
            mutate(measure=factor(measure,levels=c(
                "holds_placed",
                "licenses_out",
                "hold_count",
                "total_licenses",
                "current_ratio",
                "licenses_bought",
                "licenses_avail",
                "nonhold_checkouts",
                "checkins",
                "checkouts",
                "holds_filled"
                
            ))) %>% 
            ggplot(aes(x=day,y=value)) + geom_line() + facet_wrap(~measure,scales = "free_y",ncol=2,dir = "v") + 
            theme_minimal() +
            labs(title = "Hold simulation",
                 subtitle = paste0("holds ratio: ",holds_ratio,"; initial buy: ",initial_buy,"; checkout duration: ",checkout_duration,
                                   "\n probability of nonhold checkout: ",paste0(round(prob_nh* 100, digits = 2), "%"),'; holds placed behavior: ',attributes(holds_placed_function)$name,
                                   "; simulation time: ",simtime," days"))
    },height = 800)
    
    # output$hmodel = DT::renderDataTable({
    #     hmodel
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
