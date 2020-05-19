#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Single Title Holds Model"),

    # Sidebar with a slider input for number of bins 
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
                        value = 365)
    ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("modelPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$modelPlot <- renderPlot({
        
        holds_ratio <- input$holds_ratio
        initial_buy <- input$initial_buy
        checkout_duration <- input$checkout_duration
        prob_nh <- input$prob_nh
        simtime <- input$simtime
        holds_placed_function <- function(i){pmax(round(20-i,0),0)}
        attributes(holds_placed_function)$name <- "straightline"
        
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
                        holds_placed = holds_placed_function(i),
                        #calc new avail licenses at start of step based on vals from previous step
                        licenses_avail = licenses_avail + licenses_bought + checkins -
                            holds_filled - nonhold_checkouts,
                        #calc new checked out licenses based on vals from previous step
                        licenses_out = licenses_out + holds_filled + nonhold_checkouts - checkins,
                        #checkouts from nonhold if any are available
                        nonhold_checkouts = round(prob_nh*licenses_avail,0),
                        #check in titles from checkout duration days ago
                        checkins = replace_na(hmodel$holds_filled[max(i-checkout_duration,1)],0) +
                            replace_na(hmodel$nonhold_checkouts[max(i-checkout_duration,1)],0),
                        #calc new holds filled
                        holds_filled = pmin(hold_count,licenses_avail),
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
        
        hmodel %>% select(day,total_licenses,licenses_out,holds_placed,holds_filled,current_ratio,licenses_bought) %>% 
            pivot_longer(cols = -day,names_to = "measure",values_to = "value") %>% 
            ggplot(aes(x=day,y=value)) + geom_line() + facet_wrap(~measure,scales = "free_y",ncol=2) + 
            theme_minimal() +
            labs(title = "Hold simulation",
                 subtitle = paste0("holds ratio: ",holds_ratio,"; initial buy: ",initial_buy,"; checkout duration: ",checkout_duration,
                                   "\n probability of nonhold checkout: ",STRAD::format_perc(prob_nh),'; holds placed behavior: ',attributes(holds_placed_function)$name,
                                   "; simulation time: ",simtime," days"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
