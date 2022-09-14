---
categories:
- ""
- ""
date: "`r Sys.Date()`"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: ipsum
title: Advanced Data Analysis
---


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```

```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
```

# Youth Risk Behavior Surveillance

## Load the data

```{r}
data(yrbss)
glimpse(yrbss)
```

Check missing values, summary statistics of numerical variables, and a very rough histogram.

```{r, feeling the data}
skimr::skim(yrbss)
```

## Exploratory Data Analysis

### Distribution


```{r, eda_on_weight}
weight <- skimr::skim(yrbss) %>%
  filter(skim_variable=="weight")

ggplot(yrbss, aes(x=weight, na.rm=TRUE)) +
   geom_density() 

```

> As we can see from the data, there are 1004 missing observations, the distribution is right skewed with its mean at 68kg and standard deviation at 17.

```{r, mutate_and_count}
yrbss_clean <- yrbss %>% 
  drop_na(weight) %>%
  drop_na(physically_active_7d) %>%
  mutate(physical_3plus = ifelse(physically_active_7d > 2, "yes", "no"))

#count method
yrbss_cm <- yrbss_clean %>% 
  count(physical_3plus)
##calculating percentage active and inactive with count method
percentage_active_cm <- yrbss_cm[2,2] / (yrbss_cm[1,2]+ yrbss_cm[2,2])
percentage_inactive_cm <- 1 - percentage_active_cm

#group by and summarize method (gbsm)
yrbss_gbsm <- yrbss_clean %>% 
  group_by(physical_3plus) %>%
  summarize(n = n())
##calculating percentage active and inactive with gbsm method
percentage_active_gbsm <- yrbss_gbsm[2,2] / (yrbss_gbsm[1,2]+ yrbss_gbsm[2,2])
percentage_inactive_gbsm <- 1 - percentage_active_gbsm
```

>Above, we calculated the number of those that are not active for more than three days - 4022 - which constitutes 32.5% of the observations. The number is the same when using both the count method as well as the group by and summarise way.

### CI: inactive high schoolers

the Confidence Interval for the population proportion of high schoolers that are NOT active 3 or more days per week

```{r, confidence_interval}

sum = yrbss_gbsm[1,2] + yrbss_gbsm[2,2]
sd = sqrt((sum*percentage_inactive_cm)*(percentage_active_gbsm))
z = 1.96

#calculating confidence intervals
CI_low = percentage_inactive_cm-z*sd/sqrt(sum)
CI_high = percentage_inactive_cm+z*sd/sqrt(sum)

print(CI_low)
print(CI_high)
```

>The 95% confidence interval is between -0.593 and 1.24. This confidence interval fully covers the range from 0 to 1, which are the only results that we could observe in real life. This shows us that the data does not give us a reliable method of estimation for the population proportion of high schoolers that are not active for 3 or more days a week.

### Boxplot: physical_3plus & weight

```{r, boxplot}
yrbss_clean %>%
  group_by(physical_3plus, weight) %>%
  ggplot(aes(x = physical_3plus, y = weight, fill=physical_3plus)) +
  geom_boxplot() +
    labs(
    title = "Boxplot of people active for more than 3 days vs weight",
    x = "Are they active more than 3 times a week?",
    y = "Weight")
```

> Based on this graph we can infer that there does not seem to be a relationship between weight and activity, which seems counterintuitive. This could point us to the fact that there is in fact another variable that influences your weight instead of physical activity, like for example consumption habits or genetics.

## Confidence Interval

```{r, ci_using_formulas}
#Confidence interval - highly active
CI_highly_active <- yrbss_clean %>%
  filter(physical_3plus == "yes") %>%
  summarize(mean_weight = mean(weight, na.rm=TRUE),
            std_dev_weight = sd(weight, na.rm=TRUE),
            sample_size = n(),
            z = 1.96,
            CI_high = mean_weight + z * std_dev_weight / sqrt(sample_size),
            CI_low = mean_weight - z * std_dev_weight / sqrt(sample_size)
            )

print(CI_highly_active)

#Confidence interval - not active
CI_not_active <- yrbss_clean %>%
  filter(physical_3plus == "no") %>%
  summarize(mean_weight = mean(weight, na.rm=TRUE),
            std_dev_weight = sd(weight, na.rm=TRUE),
            sample_size = n(),
            z = 1.96,
            CI_high = mean_weight + z * std_dev_weight / sqrt(sample_size),
            CI_low = mean_weight - z * std_dev_weight / sqrt(sample_size)
            )

print(CI_not_active)

```


## Hypothesis test with formula


```{r, t_test_using_R}

t.test(weight ~ physical_3plus, data = yrbss_clean)

```

> The null hypothesis is that the true difference in means between group yes and no equals to zero, the alternative hypothesis is that it does not equal zero. As the p value is smaller than 0.05, we can reject the null hypothesis.

## Hypothesis test with `infer`

Initialize the test: 

```{r, calc_obs_difference}
obs_diff <- yrbss_clean %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("yes", "no"))

```

Simulate the test on the null distribution: 

```{r, hypothesis_testing_using_infer_package}

null_dist <- yrbss_clean %>%
  # specify variables
  specify(weight ~ physical_3plus) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("yes", "no"))

```

Visualize the null distribution: 

```{r}
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

```

The p-value for the hypothesis test: 

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```

>As the p value is on the right side of the observations, we can reject the null hypothesis.


# IMDB ratings: Differences between directors

```{r directors, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "directors.png"), error = FALSE)
```


> Before anything, write down the null and alternative hypotheses, as well as the resulting test statistic and the associated t-stat or p-value. At the end of the day, what do you conclude?

Load the data and examine its structure: 

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)
```

> The null hypothesis is that the true difference in mean ratings between movies directed by Steven Spielberg and Tim Burton is 0. Conversely, the alternative hypothesis is that the true difference in means is not 0. 

```{r reproducing_imdb}
# Reproducing the plot
movies2<-movies %>% 
  filter(director %in% c("Steven Spielberg", "Tim Burton")) %>% 
  select(director, rating) %>% 
  group_by(director) %>% 
  summarise(mean = mean(rating, na.rm = TRUE), 
            sd = sd(rating, na.rm = TRUE), 
            cnt = n(), 
            t_critical = qt(p = 0.025, df = cnt - 1, lower.tail=FALSE),
            se = sd/sqrt(cnt), 
            margin_of_error = se * t_critical,
            lower_ci = mean - margin_of_error, 
            upper_ci = mean + margin_of_error)

ggplot(movies2, aes(x=mean,y=director)) + 
  annotate("rect",
           ymin = -Inf, ymax = Inf,
           xmin = max(movies2$lower_ci), xmax = min(movies2$upper_ci), fill = "lightgrey"
  ) + 
  geom_errorbar(aes(xmin=lower_ci, 
                    xmax=upper_ci,colour=director,y=fct_reorder(director,mean))) + 
  geom_text(aes(x = mean, y = director, label = round(mean, 2)), size = 4) + 
  geom_text(aes(x = upper_ci, y = director, label = round(upper_ci, 2)), size = 3) + 
  geom_text(aes(x = lower_ci, y = director, label = round(lower_ci, 2)), size = 3) + 
  labs(
    title = "Do Spielberg and Burton have the same mean IMDB ratings?",
    subtitle = "95%confidence intervals overlap", 
    x = "Mean IMBD Rating", 
    y = "Director") + 
  theme(legend.position = "none", 
        axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8), 
        plot.title = element_text(size = 10), 
        panel.background = element_rect(fill = "white", color = "black"), 
        panel.grid = element_line(colour = "light grey"))


# Testing the hypothesis
movies3<-movies%>%
filter(director %in% c("Steven Spielberg", "Tim Burton"))%>%
  select(director, rating)

t.test(rating ~director , data=movies3)

```

> Results obtained from the t-series test give a p-value of 1%. As it is well below the critical value of 5%, the null hypothesis can be rejected and the alternative hypothesis accepted.

# Omega Group plc- Pay Discrimination

## Loading the data

```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))
glimpse(omega) # examine the data frame
```

## Relationship Salary - Gender ?

```{r, confint_single_variables}
# Summary Statistics of salary by gender
mosaic::favstats (salary ~ gender, data=omega)

# Dataframe with two rows (male-female) and having as columns gender, mean, SD, sample size, 
# the t-critical value, the standard error, the margin of error, 
# and the low/high endpoints of a 95% condifence interval

```


> From this basic analysis we can see that Males have a higher salary on average, although further analysis must be done to determine whether this difference is significant. Females have more varied salaries, however they do also have a larger sample size.


```{r, hypothesis_testing_1}
# hypothesis testing using t.test() 
t.test(salary ~ gender, data=omega)

# hypothesis testing using infer package

library(infer)
set.seed(1234)
pub_private_null <- omega %>% 
     specify(salary ~ gender) %>% 
     hypothesize(null = "independence") %>% 
     generate(reps = 1000, type = "permute") %>% 
     calculate(stat = "diff in means", 
                              order = c("female", "male"))
pub_private_null %>% visualize()
observed_difference <- mean(omega$salary) - mean(omega$gender)

#p-value too small to obtain

```


> With a P value below 0.05, we can determine that the result is statistically significant. We can say at the confidence interval, there is enough evidence to reject null hypothesis and say that there is a difference between mean salaries of male and female employees.

## Relationship Experience - Gender?

At the board meeting, someone raised the issue that there was indeed a substantial difference between male and female salaries, but that this was attributable to other reasons such as differences in experience. A questionnaire send out to the 50 executives in the sample reveals that the average experience of the men is approximately 21 years, whereas the women only have about 7 years experience on average (see table below).

```{r, experience_stats_1}
# Summary Statistics of salary by gender
favstats (experience ~ gender, data=omega)

library(infer)
set.seed(1234)
pub_private_null <- omega %>% 
     specify(experience ~ gender) %>% 
     hypothesize(null = "independence") %>% 
     generate(reps = 1000, type = "permute") %>% 
     calculate(stat = "diff in means", 
                              order = c("female", "male"))

observed_difference <- mean(omega$salary) - mean(omega$gender)

pub_private_null %>% visualize()

#p-value too low to obtain


```


> We can see that the P value is extremely close to 0 so we can say that there is a significant difference between the experience of male and female executives. This conclusion endangers the premise that salaries differ solely on gender, through this analysis we can see that experience also has a significant part to play. 

## Relationship Salary - Experience ?


```{r, salary_exp_scatter_1}
library(ggplot2)
plot1 <- ggplot(data = omega, mapping = aes(x = experience, y = salary
))+
geom_point()+
geom_smooth(method = "lm")+
NULL 

```

## Check correlations between the data


```{r, ggpairs_1}
omega %>% 
  select(gender, experience, salary) %>% #order variables they will appear in ggpairs()
  ggpairs(aes(colour=gender, alpha = 0.3))+
  theme_bw()
```

> From the scatter plot and the correlation coefficient we can see that there is a significant positive correlation (0.803) between the 2 variables. As experience increases, salary increases.


# Challenge 1: Brexit plot

```{r brexit}

brexit_results <- read_csv("https://raw.githubusercontent.com/kostis-christodoulou/am01/master/data/brexit_results.csv")
brexit_results

brexit_results_long <- pivot_longer(brexit_results, 
                                    col = c("con_2015", "lab_2015","ld_2015", "ukip_2015"), 
                                    names_to = "party", 
                                    values_to = "party_percentage") %>%
  filter(party %in% c("con_2015", "lab_2015", "ld_2015", "ukip_2015", "leave_share"))
brexit_results_long

brexit_results_long <- pivot_longer(brexit_results, 
                                    col = c("con_2015", "lab_2015","ld_2015", "ukip_2015"), 
                                    names_to = "party", 
                                    values_to = "party_percentage") %>%
  summarize(Seat, party, party_percentage, leave_share)
brexit_results_long

party_colours = c("#0087DC", "#E4003B", "#FAA61A", "#FFD700")
parties = c("Conservaltive", "Labour", "Lib Dems", "UKIP")

g <- ggplot(brexit_results_long, aes(x = party_percentage, y = leave_share, colour = party)) +
  geom_point(size = 0.3) +
  geom_smooth(method = lm) +
  scale_colour_manual(labels = parties, values = party_colours) +
  labs(
    title = "How political affiliation translated to Brexit voting"
  ) + 
  ylab("Leave % in the 2016 Brexit referendum") +
  xlab("Party % in the UK 2015 general election") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())
g
```

> Looking at this graph, we can see that there is a moderate negative correlation between LibDem percentage and voting for Brexit.  On the other hand, what stands out, is a high correlation between the UKIP percentage and voting for Brexit. This is not very surprising given that UKIP (UK Independence Party) was at the forefront of leading the Brexit campaign. Moreover, we can also observe a slight positive correlation between conservative % and leave %.

# Challenge 2:GDP components over time and among countries

```{r read_GDP_data}

UN_GDP_data  <-  read_excel(here::here("data", "Download-GDPconstant-USD-countries.xls"), # Excel filename
                sheet="Download-GDPconstant-USD-countr", # Sheet name
                skip=2) # Number of rows to skip

```

## Data Cleaning

-   make the dataframe into the long format

-   express all figures in billions

-   rename the indicators into something shorter

```{r data_overview}
head(UN_GDP_data)
skimr::skim(UN_GDP_data)
```

```{r the_indicators}
indicator_long <- sort(distinct(UN_GDP_data, IndicatorName)$IndicatorName)
```

```{r get_abbreviation}
# create a dataframe with indicator~shorter name
indicator_match <- data.frame(IndicatorName = sort(distinct(UN_GDP_data, IndicatorName)$IndicatorName), 
                              abbrv = c(
  "category_AB", "Changes in inventories", "category_F", "Exports", 
  "Final expenditure", "Government expenditure", "Gross capital formation", "GDP", 
  "Gross fixed capital formation", "Household expenditure", "Imports", "category_D", 
  "category_CE", "category_JP", "TVA", "category_I", "category_GH")
  )
indicator_match
```

```{r cleaning_GDP_data}
tidy_GDP_data <- 
  pivot_longer(data = UN_GDP_data, col = 4:51, names_to = "year", values_to = "value") %>% # 
  mutate(value = value / 1e9) %>%   # to billion
  left_join(indicator_match, by = "IndicatorName")  # get the abbreviation of indicators

glimpse(tidy_GDP_data)

# the 3 countries for comparison
country_list <- c("United States","India", "Germany")
```

## GDP components growth overview

```{r gdp_comp_over_time}
target_ind <- c(
  "Gross fixed capital formation", 
  "Exports", "Government expenditure", 
  "Household expenditure", "Imports"
  )

tidy_GDP_data %>% 
  # filter by target countries and indicators
  filter((Country %in% country_list) & (abbrv %in% target_ind)) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(x = year, y = value, colour = abbrv, group = abbrv)) +
  geom_line() +
  scale_x_discrete(limits=c(1970, 1980, 1990, 2000, 2010)) + 
  facet_wrap(~Country) + 
  labs(title = "GDP components over time", 
       subtitle = "In constant 2010 USD", 
       y = "Billion US$", 
       x = "") + 
  scale_color_discrete(name="Components of GDP") + 
  theme_bw()
```

## Calculated & Recorded GDP

calculate GDP using the formula below:

    GDP = Government expenditure + Gross fixed capital formation + Household expenditure + Net exports

```{r get_components}
tidy_GDP_data_x <- tidy_GDP_data %>% 
  filter(abbrv %in% 
           c("Exports", "Imports", 
             "Government expenditure", 
             "Gross capital formation", 
             "Household expenditure", 
             "GDP")) %>% 
  # get GDP components, ratios / recorded_GDP, GDP_hat
  summarize(Country, year, abbrv, value) %>% 
  pivot_wider(names_from = abbrv, values_from = value) %>% 
  mutate(`Net exports` = Exports - Imports, 
         Gr = `Government expenditure` / GDP, 
         Cr = `Household expenditure` / GDP, 
         Ir = `Gross capital formation` / GDP, 
         Xr = (Exports - Imports) / GDP, 
         GDP_hat = 
           `Government expenditure` + `Household expenditure` + 
           `Gross capital formation` + (Exports - Imports)) %>% 
  summarize(Country, year, 
            `Government expenditure`, 
            `Gross capital formation`, 
            `Household expenditure`, `Net exports`, 
            Gr, Cr, Ir, Xr, 
            GDP, GDP_hat) %>% 
  # convert to longer
  pivot_longer(cols = 3:12, names_to = "components", values_to = "value")
```

> Differences between the calculated (or theoretical) GDP and the recorded one varies across countries.

```{r mean_diff}
tidy_GDP_data_x %>% 
  pivot_wider(names_from = components, values_from = value) %>% 
  mutate(diff_perc = (GDP_hat - GDP) / GDP) %>% 
  group_by(Country) %>% 
  summarize(mean_difference = mean(diff_perc)) %>% 
  slice_max(mean_difference, n = 10)

tidy_GDP_data_x %>% 
  pivot_wider(names_from = components, values_from = value) %>% 
  mutate(diff_perc = (GDP_hat - GDP) / GDP) %>% 
  group_by(Country) %>% 
  summarize(mean_difference = mean(diff_perc)) %>% 
  slice_min(mean_difference, n = 10)
```

```{r func_exp_true_gdp}
# function for visualizing gdp difference given country list
get_gaps <- function(data,  countries) { 
  data %>% 
    filter(Country %in% countries) %>% 
    pivot_wider(names_from = components, values_from = value) %>% 
    mutate(diff_perc = (GDP_hat - GDP) / GDP) %>% 
    mutate(year = as.integer(year)) %>% 
    ggplot(aes(x = year, y = diff_perc)) +
    geom_line() +
    scale_x_discrete(limits=c(1970, 1980, 1990, 2000, 2010)) + 
    scale_y_continuous(labels = scales::percent) + 
    facet_wrap(~Country) + 
    labs(title = "% difference between calculated & recorded GDP", 
       y = "% difference", 
       x = "") + 
  theme_bw()
}
```

> Taking three countries as an example, the difference also changes over time for individual country.

```{r gdp_gaps_eg}
get_gaps(tidy_GDP_data_x, c("United States","India", "Germany"))
```

## The GDP components trend

```{r get_comp_gdp_func}
# visualizing gdp components% changing with years, comparing countries
get_comp_plot <- function(df, countries){
  # filter by countries and indicators
  data <- df %>% 
    filter((Country %in% countries) & 
             (components %in% c("Gr", "Cr", "Ir", "Xr"))
           ) %>% 
    mutate(year = as.integer(year))
  data$components <- factor(data$components, 
                            c("Gr", "Ir", "Cr", "Xr"))
  # plotting
  ggplot(data = data, aes(x = year, y = value, colour = components, group = components)) +
    geom_line() +
    scale_x_discrete(limits=c(1970, 1980, 1990, 2000, 2010)) + 
    scale_y_continuous(labels = scales::percent) + 
    facet_wrap(~Country, nrow = 1) + 
    labs(title = "GDP and its breakdown at constant 2010 prices in US Dollar", 
         caption = "Source: United Nations, https://unstats.un.org/unsd/snaama/Downloads", 
         y = "proportion", 
         x = "") + 
    scale_color_discrete(
      labels = c("Government expenditure", "Gross capital formation", "Household expenditure", "Net exports")) +
    theme_bw() +
    theme(legend.title = element_blank())
}
```

### US, Indian, Germany

```{r gdp_components_eg}
get_comp_plot(tidy_GDP_data_x, c("United States","India", "Germany"))
```

> For all the three, household expenditure is a primary component of GDP.

> Government expenditure in India has been far less than the other two countries. More specifically, the gross capital formation and the household expenditure exhibit opposite trends, while the government expenditure and the net exports are more stable.

> As for the net exports, Germany shows a positive net export from the 21st century, while Indian and the US shows a trade deficit. Plus, the trade deficit of the US is expanding.

### US, Indian, China

```{r gdp_components_eg2}
get_comp_plot(tidy_GDP_data_x, c("United States","India", "China"))
```

> Since the beginning of the 21st century, the net export in China has been above zero, partly due to its cheap labour, which boosted the manufacturing industry.

# Deliverables

There is a lot of explanatory text, comments, etc. You do not need these, so delete them and produce a stand-alone document that you could share with someone. Knit the edited and completed R Markdown file as an HTML document (use the "Knit" button at the top of the script editor window) and upload it to Canvas.

# Details

-   Who did you collaborate with:

    -   Amelia Przybyl, Athos Gyalui, Drishti Hoskote, Mingyu Dai, San Kashyap

-   Approximately how much time did you spend on this problem set:

    -   about 8 hrs each person

-   What, if anything, gave you the most trouble:

    -   Preparing the data for visual analysis and reproducing certain aesthetic features of graphs.

**Please seek out help when you need it,** and remember the [15-minute rule](https://mam2022.netlify.app/syllabus/#the-15-minute-rule){target="_blank"}. You know enough R (and have enough examples of code from class and your readings) to be able to do this. If you get stuck, ask for help from others, post a question on Slack-- and remember that I am here to help too!

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else?

# Rubric

Check minus (1/5): Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed.

Check (3/5): Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output).

Check plus (5/5): Finished all components of the assignment correctly and addressed both challenges. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output.
