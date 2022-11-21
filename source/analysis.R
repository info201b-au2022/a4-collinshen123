library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(usmap)

# The functions might be useful for A4
source("../source/a4-helpers.R")


incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

in_2018 <- incarceration%>%
  select(year, state, female_pop_15to64, male_pop_15to64)
  filter(str_detect(year, "2018"))%>%
  group_by(state)%>%
  summarise(female_pop_15to64 = sum(female_pop_15to64), male_pop_15to64 = sum(male_pop_15to64) )
 

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

county_max_jail_pop <- incarceration %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

year_max_jail_pop <- incarceration %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(year)

county_max_male_juvenile_pop <- incarceration %>%
  filter(male_juvenile_jail_pop == max(male_juvenile_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

year_max_male_juvenile_pop <- incarceration %>%
  filter(male_juvenile_jail_pop == max(male_juvenile_jail_pop, na.rm = TRUE)) %>%
  pull(year)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

# This function returns relevant data frame
get_year_jail_pop <- function() {
  total_pop_df <- incarceration %>%
    select(year, total_jail_pop)
  return(total_pop_df)
}

# This function renders Jail Population Increase bar chart to match figure 1
plot_jail_pop_for_us <- function() {
  total_pop_df <- get_year_jail_pop() # calls relevant data frame
  
  jail_pop_plot <- ggplot(data = total_pop_df) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)") +
    xlab(label = "Year") + 
    scale_y_continuous(name = "Total Jail Population", labels = c("0", "200,000", "400,000", "600,000", "800,000"))
  return(jail_pop_plot)
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


# This function takes the state codes and returns the state's full name
get_full_state_name <- function(states) {
  state_abv <- read.csv(file = "/Users/frank/Documents/Collin/code/info201/assignments/a4-collinshen123/source/state_names_and_codes.csv",stringsAsFactors = FALSE)
  state_names <- state_abv %>%
    filter(Code %in% states, na.rm = TRUE) %>% # filters abv df for state input
    pull(State) # returns vector of full state names
  return(state_names)
}

# This function returns data frame with relevant values
df_state_jail_pop <- function(states) {
  state_jail_pop_df <- incarceration %>%
    filter(state %in% states, na.rm = TRUE) %>% # filters data for state input
    select(state, year, total_jail_pop)
  return(state_jail_pop_df)
}
# This function plots jail pop by state
plot_jail_pop_by_states <- function(states) {
  state_labels <- get_full_state_name(states) # prepares label names for states
  
  state_jail_pop_df <- df_state_jail_pop(states) # calls relevant data frame
  
  state_jail_pop_plot <- ggplot(data = state_jail_pop_df) +
    geom_smooth(mapping = aes( x = year, y = total_jail_pop, color = state), se = FALSE) +
    labs(title = "Jail Populations by State from 1970 to 2018", x = "Year", y = "Total Jail Population",
         caption = "This chart represents the trends in jailing population by state from 1970 to 2018.") +
    scale_color_discrete(name = "States", labels = state_labels) # adds state labels
  return(state_jail_pop_plot)
}
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

inequality_df <- function() {
  inequality_df <- incarceration %>%
    select(region, year, male_juvenile_jail_pop)
  return(inequality_df)
}

inequality_plot <- function() {
  inequality_df <- inequality_df() 
  ineq_plot <- ggplot(data = inequality_df) +
    geom_smooth(mapping = aes(x = year, y = male_juvenile_jail_pop, color = region), se = FALSE) + 
    labs(title = "Male Juvenile Jailing rates by region of US (1970-2018)", 
         x = "Year", y = "Male Jevnile Jailing Rate", color = "Region", 
         caption = "The regions are geographical regions of the United States.")  
  return(ineq_plot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


# this function returns relevant data frame for map
county_ineq_df <- function(states, years) {
  jail_county_ineq_df <- incarceration %>%
    filter(year == years, state == states) %>% 
    mutate(subregion = tolower(str_replace(county_name, " County", ""))) %>%
    select(male_juvenile_jail_pop, subregion)
  
  state_abv <- read.csv(file = "/Users/frank/Documents/Collin/code/info201/assignments/a4-collinshen123/source/state_names_and_codes.csv", stringsAsFactors = FALSE)
  
  state_name <- state_abv %>%
    filter(Code == states) %>% # filter df for state input
    summarise(state = tolower(State)) %>% # lower case state for accurate join
    pull(state)
  
  county_ineq_df <- map_data("county") %>%
    filter(region == state_name) %>% # filters map data for state input
    left_join(jail_county_ineq_df, by = "subregion") # joins by county
  
  return(county_ineq_df)
}

# returns the heat map of states based on their pretrial jailing rates
plot_county_ineq <- function(states, years) {
  county_ineq_df <- county_ineq_df(states, years) # calls relevant data frame
  title_state <- get_full_state_name(states) # gets full state name from abv input
  
  # blank_theme creates a clean map rendering
  blank_theme <- theme_bw() + 
    theme( 
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
  
  ggplot(county_ineq_df) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = male_juvenile_jail_pop,),color = "white",size = .1) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Green", limits=c(0, 25), breaks=seq(0,25,by=5)) + 
    labs(title = paste("Male juvenile jailing rates in", title_state), subtitle = paste("Rates in", years), fill = "Male Juvenile Jaling Rate") +
    blank_theme 
}


## Load data frame ---- 


