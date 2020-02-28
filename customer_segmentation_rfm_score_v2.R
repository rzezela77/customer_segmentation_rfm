# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    Customer Segmentation using RFM score
# __________________________________________________________
# //////////////////////////////////////////////////////////


# 1.0 --- EXPLORE THE DATA -------------------------------------
library(tidyverse)
library(pander) # for aesthetics

library(highcharter)

# Load text file into local variable called 'data'
transaction_dat <-
    read.delim(
        file = 'data/purchases_transactions.txt',
        header = FALSE,
        sep = '\t',
        dec = '.'
    )

# Display what has been loaded

# transaction_dat %>% glimpse()
pander(head(transaction_dat))

head(transaction_dat) %>% pander()

summary(transaction_dat) %>% pander()

# Add headers and interpret the last column as a date, extract year of purchase
colnames(transaction_dat) <-
    c('customer_id', 'purchase_amount', 'date_of_purchase')

# convert to date
transaction_dat$date_of_purchase <-
    as.Date(transaction_dat$date_of_purchase, "%Y-%m-%d")

# date_of_analysis
date_of_analysis <- as.Date('2016-01-01')

transaction_dat$days_since <- as.numeric(
    difftime(
        time1 = date_of_analysis,
        time2 = transaction_dat$date_of_purchase,
        units = "days"
    )
)

# Display the data set after transformation
head(transaction_dat) %>% pander()

summary(transaction_dat) %>% pander()


# 1.1 Compute key marketing indicators using SQL language -----------------------------
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers_dat = sqldf(
    "SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          MAX (days_since) AS first_purchase,
                          COUNT(*) AS 'frequency',
                          SUM(purchase_amount) AS 'monetary'
                   FROM transaction_dat
                      GROUP BY 1"
)

head(customers_dat) %>% pander()

summary(customers_dat) %>% pander()

hist(customers_dat$recency)
hist(customers_dat$frequency)
hist(customers_dat$monetary)
hist(customers_dat$monetary, breaks = 50)

# 1.2 Quantile

quantile(customers_dat$monetary, probs = seq(0, 1, 0.20)) %>% pander()

quantile(customers_dat$frequency, probs = seq(0, 1, 0.20)) %>% pander()

quantile(customers_dat$recency, probs = seq(0, 1, 0.20)) %>% pander()

quantile(customers_dat$first_purchase, probs = seq(0, 1, 0.20)) %>% pander()



# 2.0 Calculating RFM scores ----------------------------------------------------------

rfm_data <- customers_dat

# RFM Score
rfm_data <-
    rfm_data %>%
    mutate(
        R = ntile(desc(recency), 5),
        F = ntile(frequency, 5),
        M = ntile(monetary, 5)
    )

rfm_data$RFM <- rfm_data$R * 100 + rfm_data$F * 10 + rfm_data$M

rfm_data %>% head(10) %>% pander()


# 3.0 Define Customer Segmentation Groups ---------------------------------

rfm_data$segment <- NA

rfm_data$segment[which(rfm_data$RFM == 111)] <- 'Lost'
rfm_data$segment[which(rfm_data$RFM > 111)] <- 'Hibernating'

rfm_data$segment[which(rfm_data$RFM >= 222)] <- 'About to Sleep'

rfm_data$segment[which(rfm_data$RFM >= 333)] <-
    'Potential Loyalists'

rfm_data$segment[which(rfm_data$RFM >= 444)] <- 'Champions'


# 2nd round

rfm_data$segment[which(rfm_data$segment == 'Potential Loyalists' &
                           (rfm_data$F >= 4))] <- 'Loyal Customers'

rfm_data$segment[which(rfm_data$segment == 'About to Sleep' &
                           (rfm_data$M >= 4))] <-
    'Need Attention'

rfm_data$segment[which(rfm_data$segment == 'Hibernating' &
                           (rfm_data$F >= 4 & rfm_data$M >= 4))] <-
    'Can not Lose Them'

rfm_data$segment[which(rfm_data$first_purchase <= 180)] <-
    'New Customers'


rfm_data[, -c(1, 6,7,8)] %>% 
    head(30) %>% pander()


# validate the segments

rfm_data[is.na(rfm_data$segment), ]

rfm_data %>%
    filter(is.na(segment)) %>% 
    head(15)

# is.na(rfm_data$segment)
# 
# head(rfm_data[!is.na(rfm_data$segment), ], 15)



# 3.1 Distribute Customers by Segment -------------------------------------

rfm_data$segment %>% unique()
 

# re-order segments in factor in a wat that makes sense ----
rfm_data$segment <-
    factor(
        x = rfm_data$segment,
        levels = c(
            'Lost',
            'Hibernating',
            'Can not Lose Them',
            'About to Sleep',
            'Need Attention',
            'New Customers',
            'Potential Loyalists',
            'Loyal Customers',
            'Champions'
        )
    )


# table of frequency

freqTable <-
    rfm_data %>%
    # group_by(group) %>%
    count(segment) %>%
    # arrange(desc(n)) %>%
    rename(Segment = segment, Count = n)



cust_aggr_dat <-
    aggregate(x = rfm_data[, 2:5],
              by = list(rfm_data$segment),
              mean)


cust_aggr_dat %>% pander()



# 4.0 Plotting ------------------------------------------------------------

hctreemap2(
    data = freqTable,
    group_vars = "Segment",
    size_var = "Count",
    color_var = "Count"
)


# 4.1 plotting segments

highchart() %>%
    hc_add_series(
        data = freqTable,
        type = 'column',
        hcaes(x = Segment, y = Count),
        dataLabels = list(align = "center", enabled = TRUE),
        name = 'Segments'
    ) %>%
    hc_xAxis(categories = unique(freqTable$Segment)) %>%
    hc_yAxis(title = list(text = "No of customers")
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



highchart() %>% 
    hc_add_series(
        data = cust_aggr_dat,
        type = 'scatter',
        hcaes(x = recency, y = monetary, group = Group.1)
    )


highchart() %>% 
    hc_add_series(
        data = cust_aggr_dat,
        type = 'scatter',
        hcaes(x = recency, y = frequency, group = Group.1)
    )


# 5.0 Customers by Orders -------------------------------------------------

customers_dat %>% head()

cust_orders <- 
customers_dat %>% 
    count(frequency) %>% 
    rename(Frequency = frequency, Count = n)


highchart() %>% 
    hc_add_series(
        data = cust_orders,
        type = 'column',
        hcaes(x = Frequency, y = Count),
        name = 'customers by orders',
        dataLabels = list(enabled = TRUE)
    ) %>% 
    hc_xAxis(title = list(text = 'Orders')) %>% 
    hc_yAxis(title = list(text = 'Customers'))


