#https://www.lincolninst.edu/research-data/data-toolkits/fiscally-standardized-cities/search-database
#Download Complete Dataset (Excel file)
#  https://www.lincolninst.edu/sites/default/files/content/FiSC/fisc_full_dataset_2016_update.xlsx

library(here)
library(tidyverse)
library(readxl)
library(RColorBrewer)

here::here()


#Get Data from the interweb
FiSC_Data = rio::import("https://www.lincolninst.edu/sites/default/files/content/FiSC/fisc_full_dataset_2016_update.xlsx", which = "Dataset")  #Download directly from internet
#20 largest cites
rm(Largest_Cities)
Largest_Cities <- read_csv("largest_cities.csv")


FiSC_Data_subset <- FiSC_Data %>% 
  select(year, id_city, rev_total_city, police_city, education_services_city)
FiSC_Data_subset <- inner_join(FiSC_Data_subset, Largest_Cities, by = c("id_city" = "id_city"))
FiSC_Data_subset <- FiSC_Data_subset %>% 
  group_by(year) %>% 
  summarise(Revenue = sum(rev_total_city), 
            ExpndPolice = sum(police_city), 
            ExpndEducation = sum(education_services_city)) %>% 
  mutate(Expd_Pct_Police = round((ExpndPolice / Revenue), digits = 3)) %>% 
  mutate(Expd_Pct_Education = round((ExpndEducation / Revenue), digits = 3)) %>% 
  select(year, Expd_Pct_Police, Expd_Pct_Education) %>% 
  pivot_longer(year, names_to = "Which", values_to = "Pct_of_Total_Revenue")
str(FiSC_Data_subset)

#, breaks = seq(0, 0.14, by = 0.02)
FiSC_Data_subset %>% 
mutate(Year = as.factor(year)) %>%
  ggplot(aes(x = Year, y = Pct_of_Total_Revenue, group = Which)) +
  geom_line(aes(col = Which), size=1.5) + 
  scale_color_manual(values = brewer.pal(8,"Set1")) + 
  scale_y_continuous(labels = function(x) paste0(sprintf("%.0f", x*100),"%"), limits = c(0, 0.14), breaks = seq(0, 0.14, by = 0.02)) + 
  labs(y="Percentage", x = "Year", caption = "Source: https://www.lincolninst.edu/research-data/") + 
  theme(axis.text.x = element_text(face="bold", angle = 50, hjust = 1), 
        axis.text.y = element_text(face="bold"), 
        plot.title = element_text(size = 18, face = "bold")) +  
  ggtitle("Expenditure as a % of Total Revenue - Top 20 US Cities")

