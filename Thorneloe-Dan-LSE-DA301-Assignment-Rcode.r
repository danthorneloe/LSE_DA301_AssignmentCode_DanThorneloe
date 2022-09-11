# Import the tidyverse package.
library('tidyverse')

# Determine the working directory.
getwd() 

#change working directory
setwd(dir ='/Users/danth/OneDrive/Documents/Course notes/Course 3/Week 4/LSE_DA301_Week_4_files/Data')

# Import the data set.
sales <- read.csv('turtle_sales.csv', header=T)


# Explore the data set.
head(sales)

#View the data set
# Convert data frame to a tibble.
as_tibble(sales)

# Use the glimpse() function.
glimpse(sales)

# Use the summary() function.
summary(sales)

# year has 2 NAs, rankings has some outliers, max of sales != max global sales, difference is RoW?

#Remove redundant columns as interested in sales by product_id

# Remove the 'sex' and 'region' columns.
sales2 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

#view sales2
summary(sales2)

#scatter plot - 
qplot(Product, Global_Sales, data=sales2)

#histogram of product - shows product unit sales 
qplot(Product, data=sales2)

#convert product code to cat(factor)
sales3 <- mutate(sales2,
                 Product=as.factor(Product))

#aggregate to product total sales by product code
sales4 <- sales3 %>%
  group_by(Product) %>%
  summarise(count = n(),
            sumsales = sum(Global_Sales, na.rm=TRUE))

glimpse(sales4)

#add a new calculated column
# To create a new element by dividing sumsales by product count
sales5 <- mutate(sales4, salesperprod=sumsales/count)

glimpse(sales5)

#visualisae
qplot(Product, salesperprod, data=sales5)

#focus on top times
# Filter a column and specify criteria.
topsellers <- filter(sales5, salesperprod>15)

#Visualise
qplot(Product, salesperprod, data=topsellers,  fill='red', colour=I('red'), geom='col')

#product with zero sales
no_sales <- filter(sales5, salesperprod==0)

glimpse(no_sales)
# no products with no sales

#sales by platfrom 
# group by platform
sales_plat <- sales3 %>%
  group_by(Platform) %>%
  summarise(count = n(),
            sumsales = sum(Global_Sales, na.rm=TRUE))

#order by sum of sales 
sales_plat[order(sales_plat$sumsales, decreasing = TRUE),]

#view
glimpse(sales_plat)

#plot
qplot(Platform, sumsales, data=sales_plat)

#Sum of Global sales - 1887
sales %>%
  summarise(count = n(),
            sumsalesG = sum(Global_Sales, na.rm=TRUE))

#Sum of NA sales - 885
sales %>%
  summarise(count = n(),
            sumsalesNA = sum(NA_Sales, na.rm=TRUE))

#Sum of EU sales - 579
sales %>%
  summarise(sumsalesEU = sum(EU_Sales, na.rm=TRUE))



###################################
#Week 5 Assignment activity: Clean, manipulate, and visualise the data

#Sales3 contains all 3 sales columns 
view(sales3)

#add column containing RoW sales
library(dplyr)
sales3 = sales3 %>%
  mutate(ROW_Sales = (Global_Sales - (NA_Sales+EU_Sales))) 

# check column been added
glimpse(sales3)
         
# Call the function to calculate the mean.
mean(sales3$NA_Sales) 
mean(sales3$EU_Sales) 
mean(sales3$Global_Sales) 
mean(sales3$ROW_Sales)

# variability of data 
# Determine the minimum and maximum value.
min(sales3$NA_Sales)  
min(sales3$EU_Sales) 
min(sales3$Global_Sales) 
min(sales3$ROW_Sales)

max(sales3$NA_Sales)  
max(sales3$EU_Sales) 
max(sales3$Global_Sales) 
max(sales3$ROW_Sales)

# more easily doen with summary command
summary(sales3)

#Determine the impact of sales per product 
#aggregate to produce total sales by product cat
sales_agg <- sales3 %>%
  group_by(Product) %>%
  summarise(count = n(),
            sumsales = sum(Global_Sales, na.rm=TRUE))

glimpse(sales_agg)

#add a new calculated column
# To create a new element by dividing sumsales by product count
sales_agg = sales_agg %>%
  mutate(SalesPerProd=sumsales/count)

#view
glimpse(sales_agg)

#summary of ag data
summary(sales_agg)


# confirming that same product id appear multiple times - due to platform?
qplot(Product, data=sales3)

#Visualise data 
# total sales by region
sum(sales3$NA_Sales)
sum(sales3$EU_Sales)
sum(sales3$ROW_Sales)

# mosty polular platform
sales_plat <- sales3 %>%
  group_by(Platform) %>%
  summarise(sumsalesplat = sum(Global_Sales, na.rm=TRUE))

glimpse(sales_plat)
#wii has highest sales 

#view highest selling products
qplot(Product, SalesPerProd, data=sales_agg)

#zoom in on top 20 sellers
# Filter a column and specify criteria.
topsellers <- filter(sales_agg, SalesPerProd>20)

#Visualise
qplot(Product, SalesPerProd, data=topsellers, geom='col')
#107 id the top sellproduct by a long way

# Calculate IQR.
IQR(sales3$Global_Sales)  

# Return the standard deviation.
sd(sales3$Global_Sales) 


# get for normal distribution
# Specify the qqnorm function.
qqnorm(sales3$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Time')
#data as we are already aware is not normal

# Run a Shapiro-Wilk test:
shapiro.test(sales3$Global_Sales)

#W = 0.6818, p-value < 2.2e-16

#install Skew and kurtosis packages
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(sales3$Global_Sales) 
kurtosis(sales3$Global_Sales)

#skewness(sales3$Global_Sales) 
#[1] 4.045582
#> kurtosis(sales3$Global_Sales)
#[1] 32.63966

# Correlation between sales columns
cor(sales3$Global_Sales, sales3$NA_Sales)
#0.93 highly correlated as we would suspect as one is contributor to other
cor(sales3$EU_Sales, sales3$NA_Sales)
#0.71 not so correlated so worth exploring local market differences 

####################################
#Week 6
#Linear regression

#prepare aggregate data
#Determine the impact of sales per product 
#aggregate to produce total sales by product cat
sales_agg2 <- sales3 %>%
  group_by(Product) %>%
  summarise(count = n(),
            GS_sales_sum = sum(Global_Sales, na.rm=TRUE),
                           NA_sales_sum = sum(NA_Sales, na.re=TRUE),
                                              EU_sales_sum = sum(EU_Sales, na.re=TRUE))
#View data 
glimpse(sales_agg2)



# Create a model to test the relationship between NA sales adn global sales
model1 <- lm(GS_sales_sum~NA_sales_sum, data=sales_agg2)

# View the model.
model1

# View more outputs for the model - the full regression table.
summary(model1)

# Plot the residuals.
plot(model1$residuals)

# Specify the coefficients(model1).
abline(coefficients(model1))

# Calculate the sum of squares error (SSE) to determine strength.
SSE1 = sum(model1$residuals^2)

# View the result.
SSE1

# Very  high SSE = model1 is a poor fit.
# The closer the SSE is to 0, the better the fit.

##
# Create a model to test the relationship between EU sales and global sales
model2 <- lm(GS_sales_sum~EU_sales_sum, data=sales_agg2)

# View the model.
model2

# View more outputs for the model - the full regression table.
summary(model2)

# Plot the residuals.
plot(model2$residuals)

# add line of best fit
abline(coefficients(model2))

# Calculate the sum of squares error (SSE) to determine strength.
SSE2 = sum(model2$residuals^2)

# View the result.
SSE2

# Very  high SSE = model2 is a poor fit and worse that model1 so NA sales are better predictor of global sales
# The closer the SSE is to 0, the better the fit.

#Build a multi-var model

model3 = lm(GS_sales_sum~NA_sales_sum+EU_sales_sum, data=sales_agg2)
summary(model3)

# p-vlues are very small so highly significant variable
#adjusted R2 is 97% to 97% of variability in index variable is due to EU sales/ NA sales

#Make a forecast with this model
#define inputs
new <- data.frame(NA_sales_sum=34.02, EU_sales_sum=23.80)

#check new is a dataframe
is.data.frame(new)

#predict output 
predict(model3, newdata=new)
#Based on provided sales data regional sales as described above would product global sales of 65.7

