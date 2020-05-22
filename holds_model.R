library(tidyverse)
library(magrittr)

# H holds
# HP holds placed
# HF holds filled
# LA Licenses available
# LO Licenses checked out
# LB Licenses bought
# 
# r holds ratio 
# d checkout duration
# x time to license expiration
# p_nh probability  of a title being checked out from browsing


#define functions to calculate holds placed

#start at top value, decrease in straight line
holds_placed_function_straightline <- function(i,top,slope){pmax(round(top+i*slope,0),0)}

#start at top value, taper off
holds_placed_function <- function(i,top){round(top*exp(((1-i)+1)/5),0)}
#increase then decrease
holds_placed_function_parabola <- function(i,a,b,c){a*(i^2)+(b*i)+c}

#reverse s shape
#increase then decrease
holds_placed_function_rev_s <- function(i,a,c){c(exp(a*i))/(c(exp(a*i))+ (1-c))}




#build and peak


#parameters
holds_ratio <- 3L
initial_buy <- 3L
checkout_duration <- 21L
prob_nh <- 0
simtime <- 52*7
top_holds_placed <- 20L


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
        holds_placed = holds_placed_function(i,top_holds_placed),
        #calc new avail licenses at start of step based on vals from previous step
        licenses_avail = licenses_avail + licenses_bought + checkins -
          holds_filled - nonhold_checkouts,
        #calc new checked out licenses based on vals from previous step
        licenses_out = licenses_out + holds_filled + nonhold_checkouts - checkins,
        #checkouts from nonhold if any are available
        nonhold_checkouts = round(prob_nh*min(c(licenses_avail,hmodel$licenses_avail[i-1])),0),
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
  ggplot(aes(x=day,y=value)) + geom_line() + facet_wrap(~measure,scales = "free_y") + 
  theme_minimal() +
  labs(title = "Hold simulation",
       subtitle = paste0("holds ratio: ",holds_ratio,"; initial buy: ",initial_buy,"; checkout duration: ",checkout_duration,
                         ": probability of nonhold checkout: ",STRAD::format_perc(prob_nh),'; holds placed behavior: ',attributes(holds_placed_function)$name,
                         "; simulation time: ",simtime," days"))
