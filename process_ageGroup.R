library(tidyverse)
setwd("countries")
csv_files <- list.files(pattern = "\\.csv$")
for(file in csv_files) {
       data <- read_csv(file)     
       data_long <- data %>% rename_at(vars(3:length(data)),~paste0("X",.x))%>% pivot_longer(cols = starts_with("X"), names_to = "age", values_to = "value") %>% mutate(age=str_remove(age,"^X"))
       data_long <- data_long %>% mutate(age = gsub("\\+", "", age)) %>% mutate(age = as.integer(age))
      
       dataSummarised <- data_long %>% 
               group_by(Year) %>% 
               summarise(sum_value = sum(value),
                                            sum_over_64 = sum(value[age > 64]),
                                             sum_below_15 = sum(value[age < 15]),
                                             sum_between_15_64 = sum(value[age >= 15 & age <= 64]),
                                             pct_over_64 = sum(value[age > 64])/sum(value) * 100)
  
          # create a summary file with the name based on the original file name
           summary_file <- paste0("./output/summary_", file)
           write.csv(dataSummarised, summary_file, row.names = FALSE)
      }