Behaviour_data <- read.csv("QVI_purchase_behaviour.csv")

View(Behaviour_data)


#package installation

install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("stringr")


#import excel file(install package first)
install.packages("readxl")
library(readxl)

Transaction_data <- read_excel("QVI_transaction_data.xlsx")
 
head(Transaction_data)

str(Behaviour_data)
str(Transaction_data)

# Summary statistics
summary(Behaviour_data)
summary(Transaction_data)

# Missing values 
colSums(is.na(Transaction_data))

colSums(is.na(Behaviour_data))

#No missing values in both data


# Checking for duplicates
sum(duplicated(Transaction_data))
sum(duplicated(Behaviour_data))

# Removing duplicates
library(tidyverse)
Transaction_data <- distinct(Transaction_data)

head(Transaction_data$DATE)

# Converting Excel numeric date to actual date
Transaction_data$DATE <- as.Date(Transaction_data$DATE, origin = "1899-12-30")

unique(Transaction_data$PROD_NAME)

# Count products containing salsa
Transaction_data %>%
  filter(str_detect(PROD_NAME, "salsa|Salsa"))

Transaction_data <- Transaction_data %>%
  filter(!str_detect(PROD_NAME, "salsa|Salsa"))

Transaction_data <- Transaction_data %>%
  mutate(PACK_SIZE = str_extract(PROD_NAME, "\\d+")) %>%
  mutate(PACK_SIZE = as.numeric(PACK_SIZE))


head(Transaction_data$PACK_SIZE)

Transaction_data <- Transaction_data %>%
  mutate(BRAND = word(PROD_NAME, 1))

unique(Transaction_data$BRAND)

#Fixing inconsistent spellings
Transaction_data$BRAND <- recode(Transaction_data$BRAND,
                             "RED" = "RRD")

Transaction_data %>%
  ggplot(aes(x = PROD_QTY)) +
  geom_boxplot()
view(Transaction_data)

#customers purchasing unusually high quantities
Transaction_data %>%
  group_by(LYLTY_CARD_NBR) %>%
  summarise(total_qty = sum(PROD_QTY)) %>%
  arrange(desc(total_qty))

Transaction_data <- Transaction_data %>%
  filter(LYLTY_CARD_NBR != 226000)

# merge data
merged_data <- Transaction_data %>%
  left_join(Behaviour_data, by = "LYLTY_CARD_NBR")

head(merged_data)

merged_data <- merged_data %>%
  mutate(TOTAL_SALES = PROD_QTY * TOT_SALES)

#Analyzing Sales by Customer Segment

segment_sales <- merged_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(
    total_sales = sum(TOT_SALES),
    total_customers = n_distinct(LYLTY_CARD_NBR),
    avg_sales = mean(TOT_SALES)
  )

segment_sales

#Top-Selling Brands
brand_sales <- merged_data %>%
  group_by(BRAND) %>%
  summarise(total_sales = sum(TOT_SALES)) %>%
  arrange(desc(total_sales))

brand_sales

#Analyzing Pack Size Preferences

pack_sales <- merged_data %>%
  group_by(PACK_SIZE) %>%
  summarise(total_sales = sum(TOT_SALES)) %>%
  arrange(desc(total_sales))

pack_sales


# transactions per day
transactions_over_time <- transactions_by_day %>%
  group_by(DATE) %>%
  summarise(num_transactions = n())

#Visuals

#Sales by Life Stage

merged_data %>%
  group_by(LIFESTAGE) %>%
  summarise(total_sales = sum(TOT_SALES)) %>%
  ggplot(aes(x = reorder(LIFESTAGE, total_sales),
             y = total_sales)) +
  geom_col() +
  coord_flip()

#Sales by Premium Segment

merged_data %>%
  group_by(PREMIUM_CUSTOMER) %>%
  summarise(total_sales = sum(TOTAL_SALES)) %>%
  ggplot(aes(x = PREMIUM_CUSTOMER,
             y = total_sales)) +
  geom_col()

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Plot transactions over time

transactions_over_time %>%
  ggplot(aes(x = DATE, y = num_transactions)) +
  geom_line() +
  labs(
    title = "Transactions Over Time",
    x = "Date",
    y = "Number of Transactions"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Top Brands
brand_sales %>%
  top_n(10, total_sales) %>%
  ggplot(aes(x = reorder(BRAND, total_sales),
             y = total_sales)) +
  geom_col() +
  coord_flip()

write_csv(Transaction_data, "clean_transactions_data.csv")
write_csv(Behaviour_data, "clean_behaviour_data.csv")
write_csv(merged_data, "merged_data.csv")



---
  ### Key Findings
# Young Singles/Couples generated the highest chip sales.
# Mainstream customers purchased the largest quantity.
# 175g pack sizes were most popular.
# Brand X dominated premium segments.
  
  
### Recommendation
# Target Mainstream Young Singles/Couples.
# Increase promotions on popular pack sizes.
# Place premium brands near high-traffic areas.






