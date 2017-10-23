library(tidyverse)
library(snakecase)
library(lubridate)
library(scales)
library(ggmap)
library(ggthemes)

#Read in data
requests <- read_csv("Data/311_Service_Requests_-_Graffiti_Removal.csv")

########## Pre-process Data ##########
#Change camel to snake cases
names(requests) <- to_any_case(names(requests) , case = "snake", postprocess = "_")

#Change date into correct format
requests$creation_date <- mdy(requests$creation_date)
requests$completion_date <- mdy(requests$completion_date)

#Dedup 
requests <- requests %>% 
  filter(status %in% c("Open", "Completed"),
         community_area %in% c(seq(1,77,1))) %>%
  mutate(year = year(creation_date),
         month = month(creation_date),
         day = day(creation_date),
         weekday = wday(creation_date, label = TRUE),
         yearmonth = format(as.Date(creation_date), "%Y-%m"),
         yearquarter = paste0(year,"/Q",quarter(creation_date)),
         response_time = as.numeric(completion_date - creation_date)) %>%
  filter(year>2010 & year<2018)

########## Graph1 - Graffiti frequency on the map##########
chicago <- get_map(location = 'chicago', zoom = 11)
freq <- requests %>% 
  group_by(longitude,latitude) %>%
  summarise(count=n()) %>%
  filter(count>30, !is.na(count)) 
ggmap(chicago) + 
  geom_point(data = freq, aes(x = longitude, y = latitude, alpha = count), color="red", size=2) +
  guides(alpha=guide_legend(title="Request\nFrequency")) +
  labs(title = "Graffiti Concentrate Along MTA Lines", 
       subtitle = "More preventive actions should be taken along blue and orange lines",
       caption = "Data from Chicago Data Portal") +
  theme(plot.title = element_text(size=18, face = "bold"),
        plot.subtitle = element_text(size=14),
        plot.caption = element_text(size = 12, hjust=1),
        legend.title = element_text(size = 12),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())

########## Graph2 - heatmap to show monthly request frequency 2011 ~ 2016 ##########
requests %>% 
  filter(year < 2017) %>%
  group_by(year, month) %>% 
  summarise(month_count = n()) %>%
  ggplot(aes(x=month, y=year, fill=month_count)) + 
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_y_continuous(breaks = c(seq(2011, 2016, 1))) +
  scale_x_continuous(breaks = c(seq(1, 12, 3)), 
                     labels = c("Jan","Apr","Jul","Oct")) +
  labs(x = "Month", y = "Year", 
       fill = "Number of Graffiti\nRemoval Requests",
       title = "Annual Number of Requests Peaked in 2013", 
       subtitle = "Graffiti removal requests concentrate in spring and summer",
       caption = "Data from Chicago Data Portal") +
  coord_fixed(ratio=1) +
  theme(plot.background = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background = element_blank(),
               panel.border = element_blank(),
               axis.line = element_blank(),
               axis.ticks = element_blank(),
               plot.title = element_text(size=18, face="bold"),
               plot.subtitle = element_text(size=14),
               plot.caption = element_text(size=12),
               axis.text = element_text(size=12),
               axis.title.y=element_text(size=12, angle = 0, face = "bold.italic"),
               axis.title.x=element_text(size=12, angle = 0, face = "bold.italic", 
                                         hjust = 0.95),
               legend.title=element_text(size=12, vjust = 0.5),
               legend.position="bottom")
  
########## Graph3 - Responding time 2011 ~ 2017##########
requests %>% 
  group_by(yearquarter) %>% 
  summarise(average_response = (mean(response_time)),
            median_response = (median(response_time))) %>%
  ggplot(aes(x=yearquarter)) +
  geom_col(aes(y=average_response), fill = "steel blue") +
  geom_point(aes(y=median_response), color = "grey", size=5) +
  scale_x_discrete(breaks=c("2011/Q1","2012/Q1","2013/Q1","2014/Q1","2015/Q1", "2016/Q1","2017/Q1"))+
  scale_y_continuous(breaks = c(seq(0,10,2))) +
  labs(x = "Year/Quarter", y = "Average Responding Time (Days)", 
       title = "Average Responding Time Decreased Sharply in 2014", 
       subtitle = "More than half requests are completed within one day in recent 3 years",
       caption = "Grey point shows the median responding time of the quarter\nData from Chicago Data Portal") +
  theme_linedraw() +
  theme(plot.title = element_text(size=18, face = "bold"),
        plot.subtitle = element_text(size=14),
        plot.caption = element_text(size=12),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

########## Graph4 - Requests per population vs demographic information ##########
#Read in community data and preprocess
community <- read_csv("data/chicago-community-areas.csv")
names(community) <- to_any_case(names(community) , case = "snake", postprocess = "_")
community$majority <- ifelse(
  community$latinos > 0.5, "Hispanic", "Non-Hispanic")
#community$majority <- factor(community$majority, levels=c("Hispanic", "African-American", "White", "No Majority"))

requests_by_community <- requests %>% 
  filter(year == 2016) %>%
  group_by(community_area) %>% 
  summarise(by_community_count = n()) %>% 
  left_join(community) %>% 
  mutate(requests_per_10000 = 10000 * by_community_count / population)

ggplot(data = requests_by_community, aes(x = income, y = requests_per_10000)) +
  geom_point(aes(color=majority, size = population),
             alpha = 1) +
  scale_color_manual(values=c("#4682B4","#D3D3D3")) +
  scale_x_continuous(breaks = seq(15000, 90000, by = 15000)) +
  scale_y_continuous(breaks = seq(0, 2500, by = 500), limits = c(0,2500)) +
  labs(x = "Median Income", y = "Requests Volume per 10,000 Population", 
       color = "Majority Race",
       size = "Population",
       title = "Graffiti Removal Requests Volume by Community in 2016", 
       subtitle = "Hispanic neighborhoods present a remarkably higher volume of graffiti removal requests",
       caption = "Data from Chicago Data Portal and American Community Survey") +
       theme_minimal() +
       theme(plot.title = element_text(size=18, face = "bold",hjust=0.3),
        plot.subtitle = element_text(size=14, hjust=0.3),
        plot.caption = element_text(hjust=0.5, size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 

########## Graph5 - Hispanic Community Closer Look ##########
requests_by_community_year <- requests %>% 
  filter(year < 2017) %>%
  group_by(year, community_area) %>% 
  summarise(by_community_year_count = n()) %>%
  left_join(community) %>%
  filter(majority == "Hispanic") 

#Assign orders to communities by total volume
requests_by_community_total <- requests_by_community_year %>%
  group_by(name) %>% 
  summarise(all_year_total = sum(by_community_year_count)) %>% 
  arrange(desc(all_year_total))
order <- requests_by_community_total[["name"]]
requests_by_community_year$name = factor(requests_by_community_year$name, levels=order, ordered=TRUE)

ggplot(data = requests_by_community_year,
       mapping = aes(x = year, y = by_community_year_count)) + 
  geom_line(alpha=0.5) + 
  geom_point(color = "steel blue") +
  facet_wrap(~ name, nrow = 4) +
  scale_x_continuous(breaks = c(2011, 2013, 2015)) +
  scale_y_continuous(breaks = c(seq(0, 10000, 5000))) +
  labs(x = "Year", y = "Number of Requests", 
       title = "Graffiti Issue Persists in Most Hispanic Communities", 
       subtitle = "Requests from South Lawndale dropped significantly in 2015",
       caption = "Neighborhoods ranked by total number of requests 2011 to 2016\nData from Chicago Data Portal and American Community Survey") +
  theme_linedraw() +
  theme(plot.title = element_text(size=18, face = "bold"),
        plot.subtitle = element_text(size=14),
        plot.caption = element_text(size=12, hjust=1),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


