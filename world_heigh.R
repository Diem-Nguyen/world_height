# Load some R packages: 

library(rvest)
library(tidyverse)
library(hrbrthemes)
library(ggthemes) 

############################################################
# Collect data from url
############################################################
url <- "http://worldpopulationreview.com/countries/average-height-by-country/" 
data <-  read_html(url) %>% 
  html_table() %>% 
  .[[1]] %>% 
  select(X2:X7) %>% 
  slice(-1)

# Rename for all columns: 
names(data) <- c('country', 'male_cm', 'female_cm',  "male_ft", "female_ft", "population")

# Prepare data
data <- data %>% 
  mutate_at(vars("male_cm", "female_cm"), as.numeric)


data <- data %>% 
  # fiter NA values
  filter(!is.na(male_cm) & !is.na(female_cm)) %>% 
  mutate(label_male = as.character(round(male_cm,1)),
         label_female = as.character(round(female_cm,1)),
         label_male = case_when(nchar(label_male) < 5 ~ paste0(label_male, ".0") , TRUE ~ label_male),
         label_female = case_when(nchar(label_female) < 5 ~ paste0(label_female, ".0") , TRUE ~ label_female))

data <- data %>% 
  arrange(male_cm) %>% 
  mutate(country = factor(country, levels = country)) # quan trong va bat buoc vi no se ve theo thu tu


############################################################
# Visualize
############################################################   

my_plot1 <- data %>% 
  mutate(endpoint = female_cm - 3.3) %>% 
  top_n(30, wt = male_cm)  %>% 
  ggplot(aes(x = country)) +
  geom_segment(aes(y = female_cm, yend = male_cm, x = country, xend = country)) + # de len truoc vi no se dep hon
  geom_point(aes(x = country, y = male_cm, color = 'Male'), size = 3) +
  geom_point(aes(x = country, y = female_cm, color = 'Female'), size = 3) +
  coord_flip() +
  theme(legend.position = c(0.93, 0.5)) + 
  theme_wsj() +
  scale_colour_wsj()+
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.line.x = element_blank())+
  scale_y_continuous(limits = c(157, 190), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 10)) +
  geom_text(aes(x = country, y = female_cm, label = label_female), hjust = 1.3, size = 3.2) +
  geom_text(aes(x = country, y = male_cm, label = label_male), hjust = -0.3, size = 3.2) +
  geom_segment(aes(y = 158, yend = endpoint, x = country, xend = country), linetype = 3) +
  theme(legend.title = element_blank())+
  labs(x = NULL, y = NULL, 
       title = "Average Height Of The 30 Highest Countries By Sex In 2019", 
       subtitle = "Unit of measurement: centimeters", 
       caption = "Data Source: http://worldpopulationreview.com") +
  theme(plot.margin = unit(c(1, 1.2, 1, 1), "cm")) +
  theme(plot.title  = element_text(size = 20)) + 
  theme(plot.subtitle = element_text(size = 12)) + 
  theme(plot.caption = element_text(size = 10, face = "italic")) + 
  theme(legend.text = element_text( size = 11, face = "bold"))


# average height of the whole country

data$avg_height <- (data$male_cm + data$female_cm)/2
my_plot2 <- data %>% 
  arrange(avg_height) %>% 
  mutate(country = factor(country, levels = country)) %>% 
  mutate(avg_height_label = round(avg_height, 1) %>% as.character()) %>% 
  top_n(-30, wt = avg_height)  %>% 
  ggplot() +
  geom_bar(aes(x = country, y = avg_height), stat = "identity", fill = "steelblue") +
  coord_flip() +
  geom_text(aes(x = country, y = avg_height, label = avg_height_label), hjust = -0.2, size = 4, color = 'steelblue') +
  theme_wsj() +
  scale_color_wsj()+
  theme(panel.grid = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(axis.line.x = element_blank()) +
  theme(axis.text.y = element_text(size = 11, color = 'steelblue')) +
  theme(legend.title = element_blank()) +
  labs(x = NULL, y = NULL, 
       title = "Average Height Of The 30 Shortest Countries In 2019", 
       subtitle = "Unit of measurement: centimeters \n", 
       caption = "Data Source: http://worldpopulationreview.com") +
  theme(plot.margin = unit(c(1, 1.5, 1, 1), "cm")) +
  theme(plot.title  = element_text(size = 20)) + 
  theme(plot.subtitle = element_text(size = 14)) + 
  theme(plot.caption = element_text(size = 11, face = "italic")) 

  







  
  
 
  
