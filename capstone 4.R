#package
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(data.table)
library(corrplot)
#function
DATE <- function(yyyy,mm,dd) {
  dte  <- as.Date(sprintf("%i-%i-%i",yyyy/mm/dd),format="%Y-%m-%d")
  return(dte)
}
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}
#load data
setwd('/Users/zhangdonglei/Desktop/duke/spring2/capstone')
a = read_excel('u53iacqiggyltqhs.xlsx') # extracted excel from "S&P Global Market Intelligence Data Through Wharton Research Data Services"
b = read_excel('tehk8pscndr65abp.xlsx') # extracted excel from "Product: CRSP Stock (Annual) (crsp_a_stock)"
c = read_excel('st5o6llcpbrmsmcq.xlsx') # extracted excel from "Product: CRSP Stock (Annual) (crsp_a_stock)"
d = read_excel('rssegdcuzge25za5.xlsx') # extracted excel from "Product: WRDS Financial Ratios (base) (wrdsapps_finratio)"
colnames(a)
colnames(b)
colnames(c)
colnames(d)


# # only return, return_sp500, return_wo_div from b
cols_from_b <- c("Return on the S&P 500 Index","Returns without Dividends","Returns","Ticker Symbol","Names Date")
new_b <- b[, cols_from_b, with = FALSE]
ret_data <- na.omit(new_b)
setDT(ret_data)
ret_data[, Year := year(`Names Date`)]
#transfer it to annual return
annual_returns <- ret_data[,.(Annual_Return = (prod(1 + Returns) - 1)), by = .(Year, `Ticker Symbol`)]



#clean sp500 annual return
sp500_annual_returns <- read.csv('sp-500-historical-annual-returns.csv')
setDT(sp500_annual_returns)
sp500_annual_returns[, date := as.Date(date, format = "%d/%m/%Y")]
print(class(sp500_annual_returns$date))
sp500_annual_returns[, Year := year(date)]
print(head(sp500_annual_returns$Year))
####---------------------------------------------
#Current Ratio from 'd'
cols_from_d <- c("Date","Ticker Symbol", "Current Ratio")
new_d0 <- d[, cols_from_d, with = FALSE]
#filter NAs
new_d0_filtered <- na.omit(new_d0)

new_d0_filtered
str(new_d0_filtered)
new_d0_filtered = as.data.table(new_d0_filtered)
new_d0_filtered[, Year := year(`Date`)]
Annual_Current_Ratio <- new_d0_filtered[, .(Annual_Current_Ratio = mean(`Current Ratio`, na.rm = TRUE)), by = .(Year, `Ticker Symbol`)]

###Liquid Ratio (Acid Test) from 'd'
cols_from_d1 <- c("Date","Ticker Symbol", "Quick Ratio (Acid Test)")
new_d <- d[, cols_from_d1, with = FALSE]
new_d_filtered <- na.omit(new_d)

new_d_filtered
str(new_d_filtered)
new_d_filtered = as.data.table(new_d_filtered)
new_d_filtered[, Year := year(`Date`)]
Annual_Quick_Ratio <- new_d_filtered[, .(Annual_Quick_Ratio = mean(`Quick Ratio (Acid Test)`, na.rm = TRUE)), by = .(Year, `Ticker Symbol`)]

#Cash Ratio from 'd'
cols_from_d2 <- c("Date","Ticker Symbol", "Cash Ratio")
new_d2 <- d[, cols_from_d2, with = FALSE]
new_d2_filtered <- na.omit(new_d2)

new_d2_filtered
str(new_d2_filtered)
new_d2_filtered = as.data.table(new_d2_filtered)
new_d2_filtered[, Year := year(`Date`)]
Annual_Cash_Ratio <- new_d2_filtered[, .(Annual_Cash_Ratio = mean(`Cash Ratio`, na.rm = TRUE)), by = .(Year, `Ticker Symbol`)]

####-----------------------
######assigning 1/3 weight to all the 3 ratios to contruct our liquidity factor##
data0 <- merge(Annual_Current_Ratio, Annual_Cash_Ratio, by = c("Year", "Ticker Symbol"))
data <- merge(data0, Annual_Quick_Ratio, by = c("Year", "Ticker Symbol"))
data <- data[!is.na(data$`Ticker Symbol`), ]
head(data)
tail(data)
data <- as.data.table(data)
data[, Liquidity := ((Annual_Cash_Ratio + Annual_Quick_Ratio + Annual_Current_Ratio)/ 3)]
# deal with outliers
mean_liquidity <- mean(data$Liquidity, na.rm = TRUE)
sd_liquidity <- sd(data$Liquidity, na.rm = TRUE)
lower_bound <- mean_liquidity - 2 * sd_liquidity
upper_bound <- mean_liquidity + 2 * sd_liquidity
data <- data[data$Liquidity >= lower_bound & data$Liquidity <= upper_bound, ]
head(data)

#Ranking top 30 and bottom 30 stocks based on liquidity for each year
data[, Rank_top := frank(-Liquidity, ties.method = "random"), by = Year]
head(data)
top_30 <- data[Rank_top <= 30, ]
top_30 <- select(top_30, Year, 'Ticker Symbol', Liquidity, Rank_top )
top_30 <- arrange(top_30, Year, Rank_top)

data[, Rank_bottom := frank(Liquidity, ties.method = "random"), by = Year]
bottom_30 <- data[Rank_bottom <= 30, ]
bottom_30 <- select(bottom_30, Year, 'Ticker Symbol', Liquidity, Rank_bottom )
bottom_30 <- arrange(bottom_30, Year, Rank_bottom)

#data check 
unique(top_30$Year)
unique(bottom_30$Year)

#----------------------------------------------------------------------
# Ensure ret_data, top_30, and bottom_30 are data.table objects
setDT(ret_data)
setDT(top_30)
setDT(bottom_30)

asset_returns_top <- merge(top_30, ret_data, by = c("Ticker Symbol", "Year"))
asset_returns_bottom <- merge(bottom_30, ret_data, by = c("Ticker Symbol", "Year"))

# Calculating premium returns: assuming 'Returns' column has the necessary data
# Adjusting for the S&P 500 performance (Market Neutral Strategy)
asset_returns_top[, Premium_Return := Returns - `Return on the S&P 500 Index`, by = Year]
asset_returns_bottom[, Premium_Return := Returns - `Return on the S&P 500 Index`, by = Year]

# Combining top and bottom returns for overall strategy performance
# Assuming short positions negate the returns
portfolio_performance <- rbindlist(list(
  asset_returns_top[, .(Average_Premium_Return = mean(Premium_Return, na.rm = TRUE)), by = Year],
  asset_returns_bottom[, .(Average_Premium_Return = -mean(Premium_Return, na.rm = TRUE)), by = Year]
))

# Aggregate overall performance by year
overall_performance <- portfolio_performance[, .(Net_Premium_Return = sum(Average_Premium_Return)), by = Year]
str(overall_performance)
overall_performance <- overall_performance[order(Year)]
head(overall_performance)
overall_performance[,real_return:= 1+Net_Premium_Return]
tail(overall_performance)
premium = overall_performance[,prod(real_return)] - 1
premium
####overall_performance:0.472434, which means our strategy to choose portfolio works(we get positive premium return for the past several decades)


#----------------------------------------------------------------------------


##the reason why we weighted 1/3 for each ratios
# Create dataframe for 1/3 ratios
one_third_df <- data.frame("Current Ratio Weight" = 1/3, "Cash Ratio Weight" = 1/3, "Quick Ratio Weight" = 1/3, "Liquidity" = mean(data$Liquidity), "Total Net Premium Return" = sum(overall_performance$Net_Premium_Return))

###generating random weights for liquidity construction######################
#Here we tried multiple weight combinations to see if there is any deterministic pattern of the weight split on returns.  
# Number of combinations to generate
num_combinations <- 1000

# Initialize an empty list to store the combinations
all_combinations <- list()

# Loop to generate combinations
for (i in 1:num_combinations) {
  # Generate two random numbers between 0 and 0.6 (we assigned a weight of 0.4 to liquid ratio)
  rand1 <- runif(1, 0, 1)
  rand2 <- runif(1, 0, 1 - rand1)
  rand3 <- 1 - rand1 - rand2
  
  # Create a list of the three numbers
  combination <- c(rand1, rand2, rand3)
  
  # Append the combination to the list of all combinations
  all_combinations[[i]] <- combination
}

# making a data table for the weights 
random_weights <- as.data.table(all_combinations)
random_weights <- transpose(random_weights)
setnames(random_weights, "V1", "Liquid Ratio Weight")
setnames(random_weights, "V2", "Current Ratio Weight")
setnames(random_weights, "V3", "Cash Ratio Weight")

### analyzing the impact of different weight combinations on total net premium return ###
# Initialize an empty list to store the results
analysis_results <- list()
Annual_Cash_Ratio
# Loop through each combination of weights
for (i in 1:nrow(random_weights)) {
  # Extract weights for the current combination
  liquid_weight <- random_weights[i, "Liquid Ratio Weight"]
  current_weight <- random_weights[i, "Current Ratio Weight"]
  cash_weight <- random_weights[i, "Cash Ratio Weight"]
  
  # Construct liquidity factor using current combination of weights
  data[, Liquidity := ((Annual_Cash_Ratio * cash_weight + Annual_Quick_Ratio * liquid_weight + Annual_Current_Ratio * current_weight))]
  
  # Ranking top 30 and bottom 30 stocks based on liquidity for each year
  data[, Rank_top := frank(-Liquidity, ties.method = "random"), by = Year]
  top_30 <- data[Rank_top <= 30, .(Year, `Ticker Symbol`, Liquidity, Rank_top)]
  top_30 <- arrange(top_30, Year, Rank_top)
  
  data[, Rank_bottom := frank(Liquidity, ties.method = "random"), by = Year]
  bottom_30 <- data[Rank_bottom <= 30, .(Year, `Ticker Symbol`, Liquidity, Rank_bottom)]
  bottom_30 <- arrange(bottom_30, Year, Rank_bottom)
  
  # Merge top and bottom returns with asset returns
  asset_returns_top <- merge(top_30, ret_data, by = c("Ticker Symbol", "Year"))
  asset_returns_bottom <- merge(bottom_30, ret_data, by = c("Ticker Symbol", "Year"))
  
  # Calculating premium returns
  asset_returns_top[, Premium_Return := Returns - `Return on the S&P 500 Index`, by = Year]
  asset_returns_bottom[, Premium_Return := Returns - `Return on the S&P 500 Index`, by = Year]
  
  # Combining top and bottom returns for overall strategy performance
  portfolio_performance <- rbindlist(list(
    asset_returns_top[, .(Average_Premium_Return = mean(Premium_Return, na.rm = TRUE)), by = Year],
    asset_returns_bottom[, .(Average_Premium_Return = -mean(Premium_Return, na.rm = TRUE)), by = Year]
  ))
  
  # Aggregate overall performance by year
  overall_performance <- portfolio_performance[, .(Net_Premium_Return = sum(Average_Premium_Return, na.rm = TRUE)), by = Year]

  # Calculate total net premium return for the combination
  total_net_premium_return <- sum(overall_performance$Net_Premium_Return, na.rm = TRUE)
  
  # Store the analysis results for the current combination of weights
  analysis_results[[i]] <- c(
    "Current Ratio Weight" = current_weight,
    "Cash Ratio Weight" = cash_weight,
    "Quick Ratio Weight" = liquid_weight,
    "Liquidity Factor" = mean(data$`Liquidity`, na.rm = TRUE),
    "Total Net Premium Return" = total_net_premium_return
  )
}

# Combine results into a single data table
analysis_results_df <- as.data.table(do.call(rbind, analysis_results))
analysis_results_df <- rbind(analysis_results_df, one_third_df, use.names = FALSE)
str(analysis_results_df)
analysis_results_df[, `Total Net Premium Return` := as.numeric(`Total Net Premium Return`)]
setorder(analysis_results_df, -`Total Net Premium Return`)
analysis_results_df
# Get max Total Net Premium Return, which is weighted in 1/3 equally 
#Based on this exercise, we see that equally splitting rhe weight between the three ratios for constructing liquidity helps generate highest maximum return
max_return <- analysis_results_df[which.max(analysis_results_df$`Total Net Premium Return`), ]
max_return
#### Visualizing correlation between weights and return

correlation_data <- analysis_results_df[, .(Current_Ratio_Weight = `Current Ratio Weight.Current Ratio Weight`,
                                            Cash_Ratio_Weight = `Cash Ratio Weight.Cash Ratio Weight`,
                                            Quick_Ratio_Weight = `Quick Ratio Weight.Liquid Ratio Weight`,
                                            Total_Net_Premium_Return = `Total Net Premium Return`),]

# Load the corrplot package
library(corrplot)

# Convert all columns of correlation_data to numeric
correlation_data[, c("Current_Ratio_Weight", "Cash_Ratio_Weight", "Quick_Ratio_Weight", "Total_Net_Premium_Return") := lapply(.SD, as.numeric), .SDcols = c("Current_Ratio_Weight", "Cash_Ratio_Weight", "Quick_Ratio_Weight", "Total_Net_Premium_Return")]

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data)

# Print the correlation matrix
print(correlation_matrix)

# Create a color palette for the correlation plot
color_palette <- colorRampPalette(c("#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#E6F598", "#ABDDA4", "#66C2A5"))(n = 100)

plot.new()
dev.off()
# Create the correlation plot
corrplot(correlation_matrix, method = "color", col = color_palette, 
         type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Add number labels in black color
         number.cex = 0.7)

# Add a title to the plot
title(main = "Correlation Matrix", col.main = "black", cex.main = 1.5)



#----------------------------------------------------------------------------------
###Model optimization
results <- list()

# Define the range for the number of stocks to long and short
long_range <- 10:40  # For example, trying different sizes from 20 to 40
short_range <- 10:40 # Similarly for short

# Loop over different sizes of long and short portfolios
for (num_long in long_range) {
  for (num_short in short_range) {
    # Calculate top 'num_long' and bottom 'num_short'
    top_n <- data[order(-Liquidity)][1:num_long]
    bottom_n <- data[order(Liquidity)][1:num_short]
    
    # Merge return data 
    top_n <- merge(top_n, annual_returns, by = c("Year", "Ticker Symbol"))
    bottom_n <- merge(bottom_n, annual_returns, by = c("Year", "Ticker Symbol"))
    
    top_n_avg_return <- top_n[, .(Average_Returns = mean(Annual_Return)), by = Year]
    bottom_n_avg_return <- bottom_n[, .(Average_Returns = mean(Annual_Return)), by = Year]
    portfolio_returns_n <- top_n_avg_return[, Average_Returns] - bottom_n_avg_return[, Average_Returns]
    portfolio_performance_n <- data.table(Year = top_n_avg_return$Year, portfolio_returns_n = portfolio_returns_n)
    portfolio_performance_n <- merge(portfolio_performance_n, sp500_annual_returns, by = "Year")
    portfolio_performance_n[, Premium_Return := portfolio_returns_n - value]
    overall_performance <- sum(portfolio_performance_n$Premium_Return)
    
    # # Compare against S&P 500
    sp500_return <- mean(sp500_annual_returns[Year %in% unique(c(top_n$Year, bottom_n$Year)), value])

    # Store results
    results[[paste("Long", num_long, "Short", num_short)]] <- list(
      Long = num_long,
      Short = num_short,
      Portfolio_Return = sum(portfolio_returns_n),
      SP500_Return = sp500_return,
      Premium_Return = overall_performance
    )
  }
}

# # Find the combination with the maximum premium return
# max_premium <- max(sapply(results, function(x) x$Premium_Return))
# best_strategy <- names(which(sapply(results, function(x) x$Premium_Return) == max_premium))
# 
# # Output the best strategy and its details
# print(paste("Best strategy:", best_strategy))
# print(results[[best_strategy]])

# Find the combination with the maximum premium return 
max_premium <- max(sapply(results, function(x) x$Premium_Return)) 
best_strategy <- names(which(sapply(results, function(x) x$Premium_Return) == max_premium)) 
# Output the best strategy and its details 
print(paste("Best strategy:", best_strategy, 'with premium return:', max_premium))
