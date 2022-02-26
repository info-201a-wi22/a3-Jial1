library("ggplot2")
library("dplyr")
library("ggmap")
library("tidyverse")
library("leaflet")
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

data_used <- data %>%
  filter(state == "CA") %>%
  select("year","county_name", "fips", "total_prison_pop","black_prison_pop", "white_prison_pop", "white_male_prison_pop", "black_male_prison_pop", "white_female_prison_pop","black_female_prison_pop") 

options(scipen = 999)
state_summary <- data_used %>%
  drop_na() %>%
  group_by(year) %>%
  summarize(
    mean_total_prison = mean(total_prison_pop),
    mean_black__prison = mean(black_prison_pop),
    mean_white_prison = mean(white_prison_pop),
    diff_race_prison = mean(black_male_prison_pop) - mean(white_male_prison_pop),
    diff_race_female = mean(black_female_prison_pop) - mean(white_female_prison_pop)
  ) %>%
  arrange(-year)


#graph
#data_for_graph <- data.frame( x = data_used$year, 
                              #y = c(data_used$black_male_prison_pop, data_used$black_female_prison_pop, 
                                    #data_used$white_male_prison_pop, data_used$white_female_prison_pop),
                              #group = c(rep("black male", nrow(data_used)),
                                        #rep("black female", nrow(data_used)),
                                        #rep("white male", nrow(data_used)),
                                        #rep("white female", nrow(data_used))))

#data_one <- melt(data_used,  id = 'year')

#data_for_graph <- data_one %>%
  #drop_na() %>%
  #filter(county_name == "Los Angeles County" | county_name == "San Diego County" | 
          # county_name == "San Bernardino County" | county_name == "Riverside County" | 
           #county_name == "Orange County" | county_name == "Sacramento County" | 
           #county_name == "Santa Clara County" | county_name ==  "Fresno County" | 
           #county_name == "Kern County" | county_name ==  "Alameda County")

data_for_graph <- data_used %>%
  group_by(year) %>%
  drop_na() %>%
  summarize(sum_black_male_count = sum(black_male_prison_pop),
            sum_black_female_count = sum(black_female_prison_pop),
            sum_white_male_count = sum(white_male_prison_pop),
            sum_white_female_count = sum(white_female_prison_pop))

graph1 <- ggplot(data_for_graph, aes(x = year)) +
  geom_line(aes(y = sum_black_male_count, color = "black male")) + 
  geom_line(aes(y = sum_black_female_count, color = "black famle")) +
  geom_line(aes(y = sum_white_male_count, color = "white male")) +
  geom_line(aes(y = sum_white_female_count, color = "white female")) +
  ggtitle("Total number of peopLe in prison bewteen black and white races in California") + 
  xlab("Year") +
  ylab("Count")
graph1


data_for_graph2 <- data_for_graph %>%
  select(year, sum_black_male_count, sum_white_male_count)

data_for_graph2 <- data.frame( x = data_for_graph2$year,
                               y = c(data_for_graph2$sum_black_male_count, data_for_graph2$sum_white_male_count),
                               group = c(rep("black male", nrow(data_for_graph2)),
                                         rep("white male", nrow(data_for_graph2))))

graph2 <- ggplot(data_for_graph2, aes(x, y, fill = group )) +
  geom_col(position = "dodge") + ggtitle("Total number of male in prison bewteen black and white races in California") + 
  xlab("Year") +
  ylab("Count")
graph2


data_for_graph3 <- data_used %>%
  drop_na() %>%
  filter(year == max(year)) %>%
  select(county_name, black_male_prison_pop)

data_for_graph3$county_name <- gsub("County","",as.character(data_for_graph3$county_name))
data_for_graph3$county_name <- tolower(data_for_graph3$county_name)

data_for_graph5 <- data.frame( x = data_for_graph3$county_name,
                               y = c(data_for_graph3$black_male_prison_pop),
                               group = c(rep("black male", nrow(data_for_graph3))))
                        
colnames(data_for_graph5) <- c('county','count','race')
county_shape <- map_data("county") %>%
  rename(county = subregion) %>% # rename for joining
  left_join(data_for_graph5, by="county") # join eviction data


blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

ggplot(county_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = race, fill = count),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "count")+
  blank_theme 
