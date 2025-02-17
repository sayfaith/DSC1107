---
title: "FA2 DSC1107"
author: "Lindsay Faith Bazar"
date: "February 17, 2025"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

## 2. Import

```{r}
data("who")
data("population")
```

**2.1. Preview the contents of who.tsv and population.csv by inspecting the files.**

```{r}
head(who)
head(population)
```

**2.2. Import the data into tibbles named who and population.**

```{r}
who <- as_tibble(who)
population <- as_tibble(population)
who
population
```

**2.3. Determine the number of rows and columns in each tibble.**

```{r}
dim(who)
dim(population)
```

**2.4. Check the summary of variable types for population.csv. Fix any anomalies and store the corrected data in population2.**

```{r}
summary(population)
population2 <- population
population2

```

## 3. Tidy Data

**3.1.1 Identify the variables in the dataset.**

```{r}
colnames(who)
```

**3.1.2 Perform a pivot operation to make the data tidy, storing the result in who2.**

```{r}

who2 <- who %>%
  pivot_longer(
    cols = -c(country, iso2, iso3, year),
    names_to = c("new", "TB_Type", "gender_age"),
    names_pattern = "^(newrel|new)_(sn|sp|ep|rel)?_?(m\\d+|f\\d+)?$",
    values_to = "count"
  ) %>%
  mutate(TB_Type = ifelse(TB_Type == "" | is.na(TB_Type), "rel", TB_Type))

who2
```

**3.1.3 Separate values like new_ep_f014 into components (e.g., new, ep, f014). Remove the column containing new, and store the result in who3.**

```{r}
who3 <- who2 %>%
  select(-new)

who3
```

**3.1.4 Further separate values like f014 into f and 014, storing the result in who_tidy.**

```{r}
who_tidy <- who3 %>%
  separate(col = gender_age, into = c("gender", "age"), sep = "(?<=^[mf])")

who_tidy

```

**3.2.1 Identify the variables in this dataset.**

```{r}
colnames(population2)
```

**3.2.2 Perform a pivot operation to tidy the data, storing the result in population3.**

The dataset looks already in a tidy format, where each row represents a unique observation with country, year, and population so we need not do pivot operation.

```{r}
population2

population3 <- population2
```

**3.2.3 Cast the population variable to an appropriate data type, storing the result in population_tidy.**

```{r}
population_tidy<-population2 %>% 
  mutate(population= as.integer(population))

population_tidy
```

## 3.3. Join Datasets

```{r}
tuberculosis<-population_tidy %>% 
  left_join(who_tidy, by=c("country", "year"))
tuberculosis<-tuberculosis %>%
  mutate(count=as.integer(count))

tuberculosis
colnames(tuberculosis)
```

## 3.4. Clean up Data

**3.4.1 Remove unnecessary variables from tuberculosis.**

```{r}
tuberculosis2 <- tuberculosis %>% select(-iso2, -iso3)
tuberculosis2
```

**3.4.2 Filter out rows with NA values.**

```{r}
tuberculosis3 <- tuberculosis2 %>% drop_na()
tuberculosis3
```

**3.4.3 Save the cleaned data back into tuberculosis.**

```{r}
tuberculosis <- tuberculosis3
tuberculosis
```

## Data Manipulation

**4.1 Determine the total TB cases among men and women in the 21st century in the United States. Identify which sex had more cases.**

```{r}
USA <- tuberculosis %>%
  filter(str_detect(country, "United States of America"), year >= 2001) %>%
  group_by(gender) %>%
  summarise(total_cases = sum(count, na.rm = TRUE), .groups = "drop")


USA <- USA %>%
  bind_rows(tibble(gender = "Total", total_cases = sum(USA$total_cases, na.rm = TRUE)))

print(USA)

```

In the 21st century in the U.S.A., men had 73,769 TB cases, while women had 43,982. That’s 29,787 more cases in men, making them the most affected group. The total number of cases for both genders combined was 117,751.

**4.2 Create a new variable, cases_per_100k, representing TB cases per 100,000 people by year, sex, age group, and TB type.**
  
```{r}
tuberculosis <- tuberculosis %>%
  mutate(cases_per_100k = (count / population) * 100000)
tuberculosis
```

**4.3 Identify:**

**The country and year with the highest cases per 100k.**

```{r}
highest_cases <- tuberculosis %>%
  filter(!is.na(cases_per_100k)) %>%
  arrange(desc(cases_per_100k)) %>%
  slice(1) %>%
  select(country, year, cases_per_100k)

print(highest_cases)


```

The country that are recorded to have the highest tuberculosis case per 100,000 people (602. cases) is the country Samoa in the year 2009.

**The country and year with the lowest cases per 100k.**

```{r}
lowest_cases <- tuberculosis %>%
  filter(!is.na(cases_per_100k)) %>%
  arrange(cases_per_100k) %>%
  slice(1) %>%
  select(country, year, cases_per_100k)

print(lowest_cases)
```

While the country with lowest case of tuberculosis is Afghanistan (no case) in the year 1997.

## Data Visualization

**5.1 Plot the total cases per 100k as a function of year for China, India, and the United States: Use a log scale on the y-axis (scale_y_log10()).Describe emerging patterns**

 
```{r}
library(ggplot2)

tuberculosis %>%
  filter(country %in% c("China", "India", "United States of America")) %>%
  group_by(country, year) %>%
  summarise(total_per_100k = sum(cases_per_100k, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_per_100k, color = country)) +  
  geom_line() +
  scale_y_log10() +  
  labs(title = "Total TB Cases per 100k Over Time",
       x = "Year",
       y = "Cases per 100,000 (log scale)") +
  theme_minimal()
```

The TB trends look different for each country. In the U.S. (blue line), TB cases stayed low, with a small increase around 2005 but mostly declining over time. In China (red line), cases went up steadily until about 2008 that it leveled off. India (green line) saw a sharp rise in cases from 1995 to the early 2000s, peaking around 2007, then decreases and leveled off. This suggests that India and China had big TB outbreaks before things started to stabilize, while the U.S. had some few rises, but in general kept TB cases low.

**5.2 Compare distributions of total cases per 100k (summed over years, sexes, and TB types) across age groups: Use a log scale on the y-axis. Highlight observed patterns.**

 
```{r}

tuberculosis %>%
  group_by(age) %>%
  summarise(total_per_100k = sum(cases_per_100k, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = age, y = total_per_100k)) +  
  geom_bar(stat = "identity", fill = "palegreen4") +
  scale_y_log10() + 
  labs(title = "Distribution of TB Cases per 100k Across Age Groups",
       x = "Age Group",
       y = "Cases per 100,000 (log scale)") +
  theme_minimal()

```

TB cases are lowest among children (0-14 years old) and highest among adults aged 25-34. This pattern may be because this age are at their working years which may be more exposed to TB, possibly due to more social interactions and exposure. After that age group, the cases slowly decrease but still relatively high among seniors (65+). 

**5.3 Create a plot to evaluate whether the number of cases per 100k in 2000 was related to a country’s population: Conclude based on the visualization. **
 
```{r}
tuberculosis %>% 
  filter(year == 2000, cases_per_100k > 0, population > 0) %>%
  ggplot(aes(x = population, y = cases_per_100k)) +
  geom_point(alpha = 0.5, color = "hotpink") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Cases per 100k vs. Population in 2000",
    x = "Population (log scale)",
    y = "Cases per 100k (log scale)"
  ) +
  theme_minimal()
```

The plot suggests an inverse relationship between population size and TB cases per 100k. Countries with smaller populations tend to have higher and more varied TB rates per 100,000 people while countries with larger populations generally have lower TB rates.