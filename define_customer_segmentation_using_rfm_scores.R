# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    Customer Segmentation - INTRODUCTION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# 1.0 --- EXPLORE THE DATA -------------------------------------
library(tidyverse)

# Load text file into local variable called 'data'
transaction_dat <- read.delim(file = 'data/purchases_transactions.txt', header = FALSE, sep = '\t', dec = '.')

# Display what has been loaded
head(transaction_dat)
summary(transaction_dat)

# Add headers and interpret the last column as a date, extract year of purchase
colnames(transaction_dat) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
transaction_dat$date_of_purchase <- as.Date(transaction_dat$date_of_purchase, "%Y-%m-%d")
# date_of_analysis
date_of_analysis <- as.Date('2016-01-01')
transaction_dat$days_since <- as.numeric(difftime(time1 = date_of_analysis,
                                            time2 = transaction_data$date_of_purchase,
                                            units = "days"))

# Display the data set after transformation
head(transaction_dat)
summary(transaction_dat)


# 1.1 Compute key marketing indicators using SQL language -----------------------------
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers_dat = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          MAX (days_since) AS first_purchase,
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'monetary'
                   FROM transaction_dat 
                      GROUP BY 1")

head(customers_dat)
summary(customers_dat)

hist(customers_dat$recency)
hist(customers_dat$frequency)
hist(customers_dat$monetary)
hist(customers_dat$monetary, breaks = 100)

# 1.2 Quantile

quantile(customers_dat$monetary, probs = seq(0, 1, 0.20)) 

quantile(customers_dat$frequency, probs = seq(0, 1, 0.20))

quantile(customers_dat$recency, probs = seq(0, 1, 0.20))

quantile(customers_dat$first_purchase, probs = seq(0, 1, 0.20))



# 2.0 Calculating RFM scores ----------------------------------------------------------

rfm_data <- customers_dat

# RFM Score
rfm_data <-
    rfm_data %>%
    mutate(R = ntile(desc(recency), 5),
           F = ntile(frequency, 5),
           M = ntile(monetary, 5))

rfm_data$RFM <- rfm_data$R * 100 + rfm_data$F * 10 + rfm_data$M

rfm_data %>% head(10)



# 3.0 Define Customer Segmentation Groups ---------------------------------

# High value and low value
rfm_data$segment <- 'NA'
rfm_data$group <- 'NA'

high_value <- quantile(rfm_data$monetary, probs = 0.80, names = FALSE)

rfm_data$group[which(rfm_data$monetary >= high_value)] <- 'high value'
rfm_data$group[which(rfm_data$monetary < high_value)] <- 'low value'


# high value - segment
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F == 5)] <- 'Champions'

rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F == 4)] <- 'Loyal Customers'
# rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F %in% (4, 5))] <- 'Loyal Customers'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F == 5)] <- 'Loyal Customers'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F == 4)] <- 'Loyal Customers'

# rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R %in% (4, 5) & rfm_data$F == 3)] <- 'Potencial Loyalists'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F == 3)] <- 'Potencial Loyalists'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F == 3)] <- 'Potencial Loyalists'

# rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R %in% (4, 5) & rfm_data$F < 3)] <- 'Need Attention'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 5 & rfm_data$F < 3)] <- 'Need Attention'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 4 & rfm_data$F < 3)] <- 'Need Attention'


rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 3)] <- 'About to Sleep'
rfm_data$segment[which(rfm_data$group == 'high value' & rfm_data$R == 2)] <- 'Hibernating'



# low value - segment
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F == 5)] <- 'Champions'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F == 4)] <- 'Loyal Customers'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F == 5)] <- 'Loyal Customers'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F == 4)] <- 'Loyal Customers'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F == 3)] <- 'Potencial Loyalists'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F == 3)] <- 'Potencial Loyalists'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 5 & rfm_data$F < 3)] <- 'Need Attention'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 4 & rfm_data$F < 3)] <- 'Need Attention'

rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 3)] <- 'About to Sleep'
rfm_data$segment[which(rfm_data$group == 'low value' & rfm_data$R == 2)] <- 'Hibernating'


# without group
rfm_data$segment[which(rfm_data$R == 1)] <- 'Lost'
rfm_data$segment[which(rfm_data$first_purchase <= 180)] <- 'New Customers'


rfm_data %>% 
    head(15) 


# validate the segments
rfm_data %>% 
    # is.na(segment)
    filter(segment == 'NA')


# 3.1 Distribute Customers by Segment -------------------------------------

# re-order segments in factor in a wat that makes sense ----
rfm_data$segment <- factor(x = rfm_data$segment, 
                           levels = c('Lost', 'Hibernating', 'About to Sleep', 'Need Attention', 'New Customers', 'Potencial Loyalists', 'Loyal Customers', 'Champions'))

freqTable <- 
rfm_data %>% 
    # group_by(group) %>% 
    count(segment) %>% 
    # arrange(desc(n)) %>% 
    rename(Segment = segment, Count = n)


freqTable_Group <- 
    rfm_data %>% 
    # group_by(group) %>% 
    count(segment, group) %>% 
    # arrange(desc(n)) %>% 
    rename(Segment = segment, Count = n)

freqTable_Group %>% glimpse()

cust_aggr_dat <-  aggregate(x = rfm_data[, 2:5], by = list(rfm_data$segment, rfm_data$group), mean)



# 4.0 Visualization -------------------------------------------------------

library(highcharter)


# in use - 20191108
hctreemap2(data = freqTable,
           group_vars = "Segment",
           size_var = "Count",
           color_var = "Count") 

# 4.1 Plot Segment size

highchart() %>% 
    hc_add_series(data = freqTable_Group,
                  type = 'column',
                  hcaes(x = Segment, y = Count, group = group),
                  dataLabels = list(align = "center", enabled = TRUE)
                  ) %>% 
    hc_xAxis(categories = unique(freqTable_Group$Segment)) %>% 
    hc_yAxis(
        title = list(text = "No of customers")
        # ,stackLabels = list(
        #     enabled = TRUE,
        #     style = list(
        #         fontWeight = "bold",
        #         color = "#f7a35c",
        #         textOutline = NULL
        #     ),
        #     format = "{total:,.0f}"
        # )
    )
