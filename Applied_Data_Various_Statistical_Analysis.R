---
title: "Applied Data Project"
author: "Jana Omaiche"
subtitle: 'Assignment 2'
output:
  html_document:
    df_print: paged
  pdf_document: default
  html_notebook: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```


### **Setup**

The following packages will be loaded to help produce the analysis:

```{r, echo = TRUE, warnings = FALSE}
library(dplyr) # Data manipulation
library(ggplot2) # Building data visualisations
library(knitr) # Generating reports in different formats
library(MASS) # Generating bivariate normal distribution
library(stats) # ANOVA
```


### **Task 1**

a) Generating the synthetic data.

```{r}
# Setting the seed
set.seed(4052139)

# Number of participants per group
n <- 100

# Probabilities for both groups (Group A = flu shot, Group B = placebo)
Prob_Group_A <- 0.10
Prob_Group_B <- 0.30

# Generating the synthetic data
Data_Group_A <- rbinom(n, size = 1, prob = Prob_Group_A)
Data_Group_B <- rbinom(n, size = 1, prob = Prob_Group_B)

# Synthetic data as a df (1 = had flu, 0 = no flu)
flu_df <- data.frame(Group_A = Data_Group_A, Group_B = Data_Group_B)

# Printing the synthetic data
print(head(flu_df))

# Calculating how many people got the flu/didn't get the flu
total_contracted_flu_A <- sum(flu_df$Group_A == 1)
total_contracted_flu_B <- sum(flu_df$Group_B == 1)
total_no_flu_A <- sum(flu_df$Group_A == 0)
total_no_flu_B <- sum(flu_df$Group_B == 0)

# Creating a df for the table
table_data <- data.frame(
  Condition = c("Contracted flu", "No flu", "TOTAL"),
  Group_A = c(total_contracted_flu_A, total_no_flu_A, n),
  Group_B = c(total_contracted_flu_B, total_no_flu_B, n))

# Printing the table with the results
print(table_data)
```

b) i. The estimated probability that a person receiving the placebo will not contract the flu during winter is 71%.

```{r}
# Total number of people in group B 
total_group_B <- length(flu_df$Group_B)

# Total number of people in group B who didn't contract the flu
no_flu_group_B <- sum(flu_df$Group_B == 0)

# Calculating probability
placebo_no_flu_group_B <- no_flu_group_B / total_group_B

print(placebo_no_flu_group_B)
```

b) ii. Deriving a 95% confidence interval for the proportion of people receiving the placebo who did not contract the flu in winter. The z-distribution will be used because the population mean and standard deviation is known, and also the population size is greater than 30 which confirms the Central Limit Theorem (CLT) to follow a normal distribution.


```{r}
# Confidence interval 95%
confidence_interval <- 0.95

alpha <- 1 - confidence_interval

# Calculating standard error
standard_error <- sqrt((placebo_no_flu_group_B * (1 - placebo_no_flu_group_B)) / total_group_B)

# Calculating z value
z_value <- qnorm(alpha / 2, mean = 0, sd = 1, lower.tail = FALSE)

# Calculating margin of error
margin_error <- z_value * standard_error

# Calculating lower and upper confidence intervals
lower_confidence <- placebo_no_flu_group_B - margin_error
upper_confidence <- placebo_no_flu_group_B + margin_error

# Printing rounded confidence intervals
print(round(lower_confidence, 3))
print(round(upper_confidence, 3))
```

There is a 95% confidence interval that between 62.1% and 79.9% of people in Group B who got the placebo vaccine, did not get the flu during winter.


b) iii. The percentage of people who would contract the flu that year considering 40% of the wider adult population receive the vaccine is 28.94%.

```{r}
# Percentage receiving vaccine
population_vaccinated <- 0.40

# Total percentage of people who contracted the flu in both groups
percentage_flu_A <- total_contracted_flu_A / length(flu_df$Group_A) * 100
percentage_flu_B <- total_contracted_flu_B / length(flu_df$Group_B) * 100

total_percentage_flu <- (percentage_flu_A * population_vaccinated / 100) + (percentage_flu_B * (1 - population_vaccinated / 100))

print(round(total_percentage_flu, 2))
```


<br>
<br>
<br>


b) iv. The estimate probability that a person who is vaccinated against the flu for 10 years will contract the flu on at least 3 of those years is 3.1%.

```{r}
years <- 10
mean <- mean(flu_df$Group_A)

# Calculating the probability
probability_three_years <- pbinom(3, years, mean, lower.tail = FALSE)

# Printing probability of 3 years 
print(round(probability_three_years, 3))
```


## **Task 2**

a) Generating 1000 observations of an exponential distribution with mean of 10.

```{r}
# Generating 1000 observations with mean 10 
set.seed(4052139)

n <- 1000
lambda <- 1/10
task_two_data <- rexp(n, lambda)

print(head(task_two_data, 10))
```

a) i. The sample mean of 10.05 is very close to the population mean of 10, which shows that the sample provides an accurate and reliable mean representation of the exponential distribution. The standard deviation also represents the same as the sample sd is 9.78 and population sd is 10.

```{r}
# Generating mean and sd of sample and population
sample_mean <- mean(task_two_data)
sample_sd <- sd(task_two_data)
  
population_mean <- 10
population_sd <- sqrt(1/lambda^2)

print(round(sample_mean, 2))
print(round(sample_sd, 2))
print(population_mean)
print(population_sd)
```

a) ii. The histogram of the sample data and the density plot of the exponential distribution data appear to have the same shape/curve pattern. This shows that the sample data follows the same right-skewed distribution in the statistical analysis. The sample means and medians are also shown on the graph, which confirms there is no normal distribution because of the wide variation between the two.

```{r}
# Generating histogram of sample data and density plot of exponential distribution data
sample_df <- data.frame(values = task_two_data)

mean_value <- mean(sample_df$values)
median_value <- median(sample_df$values)

histogram_sample_data <- ggplot(sample_df, aes(x = values)) + geom_histogram(binwidth = 1, fill = "lavender", colour = "black") +
  geom_vline(xintercept = mean_value, color = "indianred2", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_value, color = "gray37", linetype = "dashed", size = 1) + 
  annotate("text", x = mean_value, y = 10, label = "Mean", color = "indianred2", size = 3, hjust = 0) +
  annotate("text", x = median_value, y = 15, label = "Median", color = "gray37", size = 3, hjust = 0) +
  labs(title = "Histogram of sample data", x = "Values", y = "Frequency")

density_plot_exponential <- ggplot(data.frame(x = c(0, 100)), aes(x = x)) +
  stat_function(fun = dexp, args = list(rate = lambda), colour = "hotpink", linewidth = 1) +
  labs(title = "Density plot of exponential distribution", x = "Values", y = "Density")

print(histogram_sample_data)
print(density_plot_exponential)
```


b) i. The histogram of the sample means where the sample size is 2 displays right skewed data. This shows that there are some sample means which are larger than other ones. The mean of means and the median have a noticeable variation, therefore it is not a normal distribution.

```{r}
# Generating 1000 sample means of sample size 2
set.seed(4052139)

n <- 1000
lambda <- 1/10
sample_size <- 2

sample_data_b <- rexp(n * sample_size, rate = lambda)
sample_data_b <- matrix(sample_data_b, ncol = sample_size)

print(head(sample_data_b))
```

```{r}
# Generating histogram for part b
sample_means <- rowMeans(sample_data_b)

mean_of_means_value <- mean(sample_means)
median_of_means_value <- median(sample_means)

ggplot(data = data.frame(Sample_means = sample_means), aes(x = Sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = mean_of_means_value, color = "indianred2", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_of_means_value, color = "gray37", linetype = "dashed", size = 1) + 
  annotate("text", x = mean_of_means_value, y = 10, label = "Mean", color = "indianred2", size = 3, hjust = 0) +
  annotate("text", x = median_of_means_value, y = 15, label = "Median", color = "gray37", size = 3, hjust = 0) +
  labs(title = "Histogram of sample means of sample size 2",
       x = "Sample means",
       y = "Frequency")
```


c) i. When the sample size was increased to 30, a normal distribution is observed in the histogram as a bell-shaped curve is produced. This confirms the CLT as the sample size is now large enough (>= 30) for the sample means to follow a normal distribution. The mean of means (9.96) and the median (9.83) is also nearly exactly the same value (only differs by 0.13), therefore it confirms a normal distribution.

```{r}
# Generating 1000 sample means of sample size 30
set.seed(4052139)

n <- 1000
lambda <- 1/10
sample_size <- 30

sample_data_c <- rexp(n * sample_size, rate = lambda)
sample_data_c <- matrix(sample_data_c, ncol = sample_size)

print(head(sample_data_c, 2))
```

```{r}
# Generating histogram for part c
sample_means <- rowMeans(sample_data_c)

mean_of_means_c <- mean(sample_means)
median_of_means_c <- median(sample_means)

ggplot(data = data.frame(Sample_means = sample_means), aes(x = Sample_means)) +
  geom_histogram(binwidth = 0.5, fill = "deeppink", color = "black") +
  geom_vline(xintercept = mean_of_means_c, color = "white", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_of_means_c, color = "gray28", linetype = "dashed", size = 1) + 
  annotate("text", x = mean_of_means_c, y = 10, label = "Mean", color = "white", size = 3, hjust = 0) +
  annotate("text", x = median_of_means_c, y = 15, label = "Median", color = "gray29", size = 3, hjust = 0) +
  labs(title = "Histogram of sample means of sample size 30",
       x = "Sample means",
       y = "Frequency")

print(mean_of_means_c)
print(median_of_means_c)
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## **Task 3**

a) One-tailed test of the different means using a 5% significance level concludes that the null hypothesis is accepted. Therefore, the new design is equally or less attractive than the current design.

```{r}
set.seed(4052139)

n <- 20
a <- 0.05 
mean_current <- 7.5
mean_new <- 8.2
sd_difference <- 1.9

# t-stat
t_stat <- (mean_new - mean_current) / (sd_difference / sqrt(n))

# degrees of freedom (df)
df <- n - 1

# one-tailed test with 5% significance level
t_value <- qt(1 - alpha, df)

# p-value
p_value <- pt(t_stat, df, lower.tail = FALSE)

print(round(t_stat, 2))
print(df)
print(round(t_value, 2))
print(round(p_value, 3))
```

* Null hypothesis = new design is equally or less attractive than the current design.
* Alternative hypothesis = new design is more attractive than the current design.

As the p-value is greater than 0.05, the null hypothesis is accepted. Therefore, there is no statistical significance that the new design is more attractive than the current design.

<br>
<br>
<br>

b) After analysing the data of the research study for the new design, it is seen that the attractiveness score is higher in the new design compared to the current design. However, the statistical analysis revealed that the p-value (0.058) exceeds the significance level of 0.05, which suggests that the difference in attractiveness scores may not be statistically significant. In addition, the t-statistic value of 1.65 was less than the t-value of 1.73, which suggests that the attractiveness score differences are not statistically significant. Due to lack of strong evidence, an expanded sample size would be recommended to help produce more reliable results. 


## **Task 4**

Generating 10 pairs of observations from a bivariate normal distribution with set parameter values (𝜇1, 𝜇2, 𝜎1, 𝜎2, 𝜌) = (50, 55, 10, 10, 0.8):

```{r}
set.seed(4052139)

# Generating random observations with 10 pairs
n <-10
mu1 <- 50
mu2 <- 55
sigma1 <- 10
sigma2 <- 10
rho <- 0.8

data_task_four_a <- mvrnorm(n, c(mu1, mu2), matrix(c(sigma1^2, rho*sigma1*sigma2, rho*sigma1*sigma2, sigma2^2), nrow = 2))

print(data_task_four_a)
```

a) i. An appropriate hypothesis test would be a paired t-test. This is because the sample size is less than 30, and there are two groups to compare the means for. Another reason is because the standard deviation of the population is not stated.

<br>
a) ii. Paired t-test with confidence interval of 95% (5% significance) has a p-value of 0.002. Therefore, the null hypothesis is rejected and we confirm that the means of the paired observations are different. 

```{r}
# paired t-test
t_test <- t.test(data_task_four_a[,1], data_task_four_a[,2], paired = TRUE, alternative = "two.sided")

print(t_test)
```

<br>

b) i. Generating observations with 30 pairs:

```{r}
# Generating observations with 30 pairs
set.seed(4052139)
n <- 30
mu1 <- 50
mu2 <- 55
sigma1 <- 10
sigma2 <- 10
rho <- 0.8

data_task_four_b <- mvrnorm(n, c(mu1, mu2), matrix(c(sigma1^2, rho*sigma1*sigma2, rho*sigma1*sigma2, sigma2^2), nrow = 2))

print(head(data_task_four_b))
```
<br>

b) ii. Although the sample size is now 30, the standard deviation is still not specified, therefore the same paired t-test will be used from before. The p-value is very small (7.027e-05), therefore the null hypothesis is rejected.

```{r}
# paired t-test
t_test <- t.test(data_task_four_b[,1], data_task_four_b[,2], paired = TRUE, alternative = "two.sided")

print(t_test)
```


c) By comparing the answers to part a and b, the conclusions are consistent with one another. There is strong evidence to show significant difference between the means of the paired observations. When the sample size was enlarged in part b, there was a stronger evidence shown in the p-value which provides more accuracy and reliability of the mean difference.


## **Task 5**

a) Generating 6 monthly sales (recorded in thousands of units):

```{r}
set.seed(4052139)

# Generating the data
n_stores <- 6
n_months <- 6

scenario_1 <- rnorm(n_stores * n_months, mean = 50, sd = 30)
scenario_2 <- rnorm(n_stores * n_months, mean = 55, sd = 30)
scenario_3 <- rnorm(n_stores * n_months, mean = 60, sd = 30)

# Data frame
sales_data <- data.frame(
  Scenario = rep(1:3, each = n_stores * n_months),
  Monthly_sales = c(scenario_1, scenario_2, scenario_3))

print(head(sales_data))
```

a) i. A one-way ANOVA test is used due to compare the means of more than two groups that has one independent variable. In this task, there are three groups, one independent variable (promotional scenarion), and one dependent variable (monthly sales). 

<br>

a) ii. The p-value with F-statistic is 0.0692 which is greater than 0.05 and less than 1.0. The anova test is automatically set to use a 5% significance level.

```{r}
# One-way ANOVA
anova_result <- aov(Monthly_sales ~ Scenario, data = sales_data)

# Summarising ANOVA results
summary(anova_result)
```

<br>

b) Repeating with standard deviation of 25.

```{r}
# Repeating anova test with sd of 25
set.seed(4052139)

n_stores <- 6
n_months <- 6

scenario_1 <- rnorm(n_stores * n_months, mean = 50, sd = 25)
scenario_2 <- rnorm(n_stores * n_months, mean = 55, sd = 25)
scenario_3 <- rnorm(n_stores * n_months, mean = 60, sd = 25)

sales_data_b <- data.frame(
  Scenario = rep(1:3, each = n_stores * n_months),
  Monthly_sales = c(scenario_1, scenario_2, scenario_3))

anova_result_b <- aov(Monthly_sales ~ Scenario, data = sales_data_b)

print(anova_result_b)
```


c) The change in standard deviation in part b affected the variability within the groups, thus the anova output was different. The anova test in part a with a standard deviation of 30 had produced some evidence of differences between the groups. When the standard deviation is decreased, the variability is reduced within the groups which makes it difficult to detect statistical significance of the differences. Therefore, the conclusions need to be carefully made as the standard deviation can impact the statistical data analysis. 
