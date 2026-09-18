
#install.packages("readxl")


#install.packages("tidyverse")
#install.packages("rlist")

library(readxl)
library(tidyverse)
library(rlist)

#######################################################################################
#
#        Code for constructing scanner data for tobacco products in Japan
#        See accompanying documentation for notes on data
#       
#
#######################################################################################

# Specify 13 regions
regions <- c( "HOKKAIDO", "TOHOKU", "KANTO", "KEIHIN", "SHIN'ETSU", "HOKURIKU", "TOKAI", "KINKI",
             "CHUGOKU", "SHIKOKU", "KYUSHU", "OKINAWA", "TOTAL")

# List to hold region level data
region_list <- list()

###################################################################
# Loop to read in data from excel and combine
# There are two time periods which are combined in long format
###################################################################
for(region in regions){
  # Read in the data for period 1
  time_1 <- read_excel("/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/SRI+ Tobacco Products_20170102-20190113.xlsx", sheet = paste0(region), skip = 12, na = "-", col_names = FALSE)
  time_1 <- time_1 %>% filter(...8!="NA") # These rows have no sku code
  time_1 <- time_1 %>% pivot_longer(!...1:...14, names_to = "period", values_to = "value")
  colnames(time_1) <- c("region", "sales_company", "manufacturer", "brand", "sku", "data_item",
                        "hierarchy", "sku_code", "form", "flavor", "tar", "nicotine", "length", "type",
                        "period", "value")
  time_1 <- time_1 %>% filter(period != "...15") %>% mutate(period = str_sub(period, start = 4)) # Period 15 is a total of all weeks
  time_1$week <- as.numeric(time_1$period) - 15 
  
  # Read in the dates
  dates <- read_excel("/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/dates_tobacco.xlsx")
  
  dates$dates_id <- seq(1, 106, 1)
  
  # Combine data and dates and remove anything after week 77
  time_1 <- left_join(time_1, dates, by = c("week" = "dates_id"))
  time_1$date <- as.Date(time_1$date)
  time_1 <- time_1 %>% filter(week <=  77) # This is June 18, 2017 @@@@ 2018(?)
  
  # Read in the data for period 2
  time_2 <- read_excel("/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/SRI+ Tobacco Products_20180625-20200705.xlsx", sheet = paste0(region), skip = 12, na = "-", col_names = FALSE)
  
  time_2 <- time_2 %>% filter(...8!="NA") # These rows have no skue code
  time_2 <- time_2 %>% pivot_longer(!...1:...14, names_to = "period", values_to = "value")
  colnames(time_2) <- c("region", "sales_company", "manufacturer", "brand", "sku", "data_item",
                        "hierarchy", "sku_code", "form", "flavor", "tar", "nicotine", "length", "type",
                        "period", "value")
  time_2 <- time_2 %>% filter(period != "...15") %>% mutate(period = str_sub(period, start = 4)) # Period 15 is a total of all week
  time_2$week <- as.numeric(time_2$period) + 62 # Sets the first week to week 78
  
  # Read in the dates
  dates <- read_excel("/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/dates2_tobacco.xlsx")
  
  dates$dates_id <- seq(78, 183, 1)
  
  # Combine data and dates
  time_2 <- left_join(time_2, dates, by = c("week" = "dates_id"))
  time_2$date <- as.Date(time_2$date)
  
  # Combine data from both periods and append data frame to list
  full_data <- rbind(time_1, time_2)
  region_list[[paste0(region)]] <- full_data
}

# Bind all data frames in the list and save to disk
tobacco_1 <- do.call("rbind", region_list)

save(tobacco_1, file = "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/japan_tobacco_2.RData")
write_csv(tobacco_1, file = "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/japan_tobacco_2.csv")
tobacco_1 <- read_csv("/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/japan_tobacco_2.csv")



# Create group IDs
tobacco_1 <- tobacco_1 %>% group_by(date) %>% mutate(week_id = cur_group_id()) %>% ungroup()
tobacco_1 <- tobacco_1 %>% group_by(region) %>% mutate(region_id = cur_group_id()) %>% ungroup()
tobacco_1 <- tobacco_1 %>% group_by(sku) %>% mutate(sku_id = cur_group_id()) %>% ungroup()
tobacco_1 <- tobacco_1 %>% group_by(sku_code) %>% mutate(sku_code_id = cur_group_id()) %>% ungroup()
tobacco_1 <- tobacco_1 %>% group_by(sku, sku_code) %>% mutate(brand_n = cur_group_id()) %>% ungroup()

# Select relevant variables
tobacco_1 <- tobacco_1 %>% dplyr::select(region, region_id, sku, sku_id, brand_n, type, data_item, value, date, week_id)

# Wide format
wide_tob_1 <- pivot_wider(tobacco_1, names_from = data_item, values_from = value)
names(wide_tob_1) <- c("region", "region_id", "sku", "sku_id", "brand_n", "type", "date", "week_id", "value", "quantity", "volume", "price")


# Arrange data for final export
# Data sets should be labeled by product
wide_tob_1 <- wide_tob_1 %>% mutate(size = 1, multi = 1) # This is included just to conform with SAS code
wide_tob_1 <- wide_tob_1 %>% arrange(region_id, brand_n, week_id)
write_csv(wide_tob_1, file = "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/wide_tob_2.csv")

# Product data sets
smoke <- wide_tob_1 %>% filter(type == "SMOKE")
heat <- wide_tob_1 %>% filter(type == "HEAT")
smokeless <- wide_tob_1 %>% filter(type == "SMOKELESS")
cut <- wide_tob_1 %>% filter(type == "CUT")

smoke1 <- smoke %>% dplyr::select(sku, sku_id, brand_n, multi, size, week_id, value, quantity, price, region_id)
names(smoke1) <- c("sku", "sku_id", "brand_n", "multi", "size", "W", "totrev", "qt", "price", "region")
write_csv(smoke1, "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/smoke_market2.csv", na = "")

heat1 <- heat %>% dplyr::select(sku, sku_id, brand_n, multi, size, week_id, value, quantity, price, region_id)
names(heat1) <- c("sku", "sku_id", "brand_n", "multi", "size", "W", "totrev", "qt","price", "region")
write_csv(heat1, "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/heat_market2.csv", na = "")

smokeless1 <- smokeless %>% dplyr::select(sku, sku_id, brand_n, multi, size, week_id, value, quantity, price, region_id)
names(smokeless1) <- c("sku", "sku_id", "brand_n", "multi", "size", "W", "totrev", "qt", "price", "region")
write_csv(smokeless1, "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/smokeless_market2.csv", na = "")

cut1 <- cut %>% dplyr::select(sku, sku_id, brand_n, multi, size, week_id, value, quantity, price, region_id)
names(cut1) <- c("sku", "sku_id", "brand_n", "multi", "size", "W", "totrev", "qt", "price", "region")
write_csv(cut1, "/Users/sus/Library/CloudStorage/OneDrive-UniversityofGeorgia/1 UGA/1 PhD/1 Spring24/Tobacco/cut_market2.csv", na = "")


