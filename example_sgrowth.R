#S-shaped growth example from ch 3 of Duggan

library(tidyverse)
library(deSolve)
library(gridExtra)
library(patchwork)

# Set the time period and step. Define the stocks and auxiliaries.
START <- 0
FINISH <- 100
STEP <- 0.25
simtime <- seq(START, FINISH, by = STEP)
stocks <- c(sStock = 100)
auxs <- c(aCapacity = 10000, 
          aRef.Availability = 1,
          aRef.GrowthRate = 0.10)

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    aAvailability <- 1 - sStock / aCapacity
    aEffect <- aAvailability / aRef.Availability
    aGrowth.Rate <- aRef.GrowthRate * aEffect
    fNet.Flow <- sStock * aGrowth.Rate
    dS_dt <- fNet.Flow
    return(list(c(dS_dt), NetFlow = fNet.Flow,
                GrowthRate = aGrowth.Rate, 
                Effect = aEffect,
                Availability = aAvailability))
  })
}

# Create the data frame
sModel <- data.frame(ode(y = stocks, times = simtime, func = model,
                         parms = auxs, method = "euler"))


stockTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, sStock), color = "purple") 
  

availTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, Availability), color = "blue")

netflowTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, NetFlow), color = "red")

growthrateTimePlot <- sModel %>% 
  ggplot() +
  geom_line(aes(time, GrowthRate), color = "green")

stockTimePlot + netflowTimePlot + availTimePlot + growthrateTimePlot


#how is this different than a loop? - gets the same answer
lModel <- data.frame(time=simtime,
                 sStock=NA_real_, 
                 NetFlow=NA_real_,
                 GrowthRate=NA_real_,
                 Effect=NA_real_,
                 Availability=NA_real_)
lModel$sStock[1] <- stocks

for(i in 1:length(simtime)){
  lModel$Availability[i] <- 1 - lModel$sStock[i] / auxs["aCapacity"]
  lModel$Effect[i] <- lModel$Availability[i] / auxs["aRef.Availability"]
  lModel$GrowthRate[i] <- auxs["aRef.GrowthRate"] * lModel$Effect[i]
  lModel$NetFlow[i] <- lModel$sStock[i] * lModel$GrowthRate[i]
  lModel$sStock[i+1] <- lModel$sStock[i] + lModel$NetFlow[i]/4

}


model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    aAvailability <- 1 - sStock / aCapacity
    aEffect <- aAvailability / aRef.Availability
    aGrowth.Rate <- aRef.GrowthRate * aEffect
    fNet.Flow <- sStock * aGrowth.Rate
    dS_dt <- fNet.Flow
    return(list(c(dS_dt), NetFlow = fNet.Flow,
                GrowthRate = aGrowth.Rate, 
                Effect = aEffect,
                Availability = aAvailability))
  })
}

#write the funcion in more idiomatic R
start_time <- 0
end_time <- 100
step_val <- 0.25
simtime <- seq(start_time, end_time, by = step_val)
stock <- 100
capacity <- 10000
ref_availability <-  1
ref_growth_rate <-  0.10

model_alt <- function(){
    availability <- 1 - stock / capacity
    effect <- availability / ref_availability
    growth_rate <- ref_growth_rate * effect
    net_flow <- stock * growth_rate
    dS_dt <- net_flow
    return(list(c(dS_dt), NetFlow = net_flow,
                GrowthRate = growth_rate, 
                Effect = effect,
                Availability = availability))
}

aModel <- data.frame(ode(y = stock, times = simtime, func = model_alt,
                         parms = c(capacity=capacity,ref_availability=ref_availability,ref_growth_rate=ref_growth_rate), 
                         method = "euler"))
