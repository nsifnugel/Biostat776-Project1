install.packages("tidytuesdayR")
install.packages("gapminder")
install.packages("paletteer")

library(tidyverse)
library(tidytuesdayR)
library(here)
library(gapminder)
library("magrittr")
library("paletteer")
library(plotly)
library(ggthemes)
library(paletteer)

setwd("~/Desktop/Biostat776-Project1")

# tests if a directory named "data" exists locally
if (!dir.exists(here("~/Desktop/biostat776","data"))) { #playing around with here function
  dir.create(here("~/Desktop/biostat776","data"))
}

# saves data only once (not each time you knit a R Markdown)
if (!file.exists(here("~/Desktop/biostat776","data", "chocolate.RDS"))) {
  url_csv <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv"
  chocolate <- read_csv(url_csv)
  saveRDS(chocolate, file = here("~/Desktop/biostat776","data", "chocolate.RDS"))  # save the file to RDS objects
}

#shows that even though my working directory is project 1, i can still pull and save data into another folder using the here function - in this case it was the biostat776 folder , data folder nested inside that

chocolate <- readRDS(here("data", "chocolate.RDS"))
as_tibble(chocolate)

glimpse(chocolate)

# getting rid of qplot so need new function to generate the histograms, lecture said something about it being used for ggplot but not ggplot2
qplot(rating, data = chocolate)
qplot(rating, data = chocolate, bins = 10)
qplot(rating, data = chocolate, bins = 15)
qplot(rating, data = chocolate, bins = 20)
qplot(rating, data = chocolate, bins = 25) #this one looks the best - smooth

qplot(country_of_bean_origin, data = chocolate)

as_tibble(count(chocolate, country_of_bean_origin)) %>%
  print(n = Inf)

summary_ecuador <- chocolate %>%
  filter(country_of_bean_origin == "Ecuador") %>%
  summarize(tibble(
    `mean` = mean(rating),
    `sd` = sd(rating),
    `total` = length(rating)
  ))

chocolate %>%
  filter(country_of_bean_origin == "Ecuador") %>%
  group_by(company_manufacturer) %>%
  summarize(
    average_rating = mean(rating)
  ) %>%
  arrange(desc(average_rating))

chocolate_ecuador %>%
  group_by(company_manufacturer) %>%
  summarize(
    average_rating = mean(rating)
  ) %>%
  arrange(desc(average_rating))

chocolate %>%
  group_by(country_of_bean_origin) %>%
  summarize(
    average_rating = mean(rating)) %>%
  arrange(desc(average_rating))

count(chocolate,country_of_bean_origin)

x <- chocolate %>%
  group_by(country_of_bean_origin) %>%
  mutate(number_of_ratings = length(rating)) %>%
  filter(number_of_ratings >= 10) %>%
  summarize(
    average_rating = mean(rating)) %>%
  arrange(desc(average_rating))

count(x, country_of_bean_origin)

chocolate_reviews_50 <- chocolate %>%
  group_by(country_of_bean_origin) %>%
  mutate(number_of_ratings = length(rating)) %>%
  filter(number_of_ratings >= 50) %>%
  mutate(cocoa_percent_integer = as.integer(substr(cocoa_percent,1,1))) %>%
  mutate(cocoa_percent_group = case_when(
    cocoa_percent_integer > 2 & cocoa_percent_integer < 6 ~ "<60%",
    cocoa_percent_integer == 6 ~ ">=60% to <70%",
    cocoa_percent_integer == 7 | cocoa_percent_integer == 8 ~ ">=70% to <90%",
    cocoa_percent_integer == 1 | cocoa_percent_integer == 9 ~ ">=90%"
  )) %>%
  arrange(cocoa_percent_group)

boxplot <- ggplot(data = chocolate_reviews_50) +
  geom_boxplot(aes( x = cocoa_percent_group, y = rating)) +
  facet_grid(country_of_bean_origin ~ .) +
  labs(title = "Percent Cocoa vs. Rating by Country") +
  labs(
    x = "Percent Cocoa",
    y = "Rating")

print(boxplot)

# part 2

gapminder_country_continent <- gapminder %>%
    rename(country_of_bean_origin = country) %>%
    select(country_of_bean_origin,continent) %>%
    unique()

chocolate <- left_join(x = chocolate, y = gapminder_country_continent, by = "country_of_bean_origin")

chocolate_noblend_10reviews <- chocolate %>%
  group_by(country_of_bean_origin) %>%
  filter(country_of_bean_origin != "Blend") %>%
  mutate(number_of_ratings = length(rating)) %>%
  filter(number_of_ratings >= 10)

complete_continent_list <- chocolate_noblend_10reviews %>%
  select(country_of_bean_origin, continent) %>%
  filter(is.na(continent)) %>%
  unique() %>%
  select(country_of_bean_origin) %>%
  add_column(continent = c("Oceania", "Oceania", "Africa", "Oceania", "Americas", "Americas", "Americas","Africa", "Oceania","Americas","Americas" )) %>%
  rbind(gapminder_country_continent)

chocolate_noblend_10reviews <- chocolate_noblend_10reviews %>%
  select(-(continent))

chocolate_noblend_10reviews <- left_join(x = chocolate_noblend_10reviews, y = complete_continent_list, by = "country_of_bean_origin")

chocolate_noblend_10reviews %>%
  group_by(continent) %>%
  summarize(
    average_rating=mean(rating),
    sd = standa
  )%>% print()

ggplot(chocolate_noblend_10reviews, aes(x=continent, y=rating, fill = continent))+
  geom_violin() +
  labs(title = "Rating by Continent") +
  labs(
    x = "Continent",
    y = "Rating")

# part 3

chocolate_ingredients_flavors <- chocolate %>%
  select(ref,review_date,ingredients,most_memorable_characteristics)%>%
  mutate(beans = case_when(
    grepl("B",ingredients) == TRUE ~ 1,
    grepl("B",ingredients) == FALSE ~ 0
  )) %>%
  mutate(sugar = case_when(
    grepl("S",ingredients) == TRUE ~ 1,
    grepl("S",ingredients) == FALSE ~ 0
  )) %>%
  mutate(cocoa_butter = case_when(
    grepl("C",ingredients) == TRUE ~ 1,
    grepl("C",ingredients) == FALSE ~ 0
  ))%>%
  mutate(vanilla = case_when(
    grepl("V",ingredients) == TRUE ~ 1,
    grepl("V",ingredients) == FALSE ~ 0
  ))%>%
  mutate(lectin = case_when(
    grepl("L",ingredients) == TRUE ~ 1,
    grepl("L",ingredients) == FALSE ~ 0
  )) %>%
  mutate(salt = case_when(
    grepl("Sa",ingredients) == TRUE ~ 1,
    grepl("Sa",ingredients) == FALSE ~ 0
  )) %>%
  mutate(char_cocoa = case_when(
    grepl("cocoa",most_memorable_characteristics) == TRUE ~ 1,
    grepl("cocoa",most_memorable_characteristics) == FALSE ~ 0
  ))  %>%
  mutate(char_sweet = case_when(
    grepl("sweet",most_memorable_characteristics) == TRUE ~ 1,
    grepl("sweet",most_memorable_characteristics) == FALSE ~ 0
  ))  %>%
  mutate(char_nutty = case_when(
    grepl("nutty",most_memorable_characteristics) == TRUE ~ 1,
    grepl("nutty",most_memorable_characteristics) == FALSE ~ 0
  )) %>%
  mutate(char_creamy = case_when(
    grepl("creamy",most_memorable_characteristics) == TRUE ~ 1,
    grepl("creamy",most_memorable_characteristics) == FALSE ~ 0
  ))%>%
  mutate(char_roasty = case_when(
    grepl("roasty",most_memorable_characteristics) == TRUE ~ 1,
    grepl("roasty",most_memorable_characteristics) == FALSE ~ 0
  )) %>%
  mutate(char_earthy = case_when(
    grepl("earthy",most_memorable_characteristics) == TRUE ~ 1,
    grepl("earthy",most_memorable_characteristics) == FALSE ~ 0
  )) %>%
    group_by(review_date) %>%
    summarize(beans = mean(beans),
              sugar = mean(sugar),
              cocoa_butter = mean(cocoa_butter),
              vanilla = mean(vanilla),
              lectin = mean(lectin),
              salt = mean(salt),
              char_cocoa = mean(char_cocoa),
              char_sweet = mean(char_sweet),
              char_nutty = mean(char_nutty),
              char_creamy = mean(char_creamy),
              char_roasty = mean(char_roasty),
              char_earthy = mean(char_earthy)
              )

chocolate_ingredients_flavors_tidy <- chocolate_ingredients_flavors %>%
  pivot_longer(-review_date, names_to = "feature", values_to = "mean_score") %>%
  mutate(review_date = factor(review_date), feature = factor(feature))

# part 4
paletteer_d("ggprism::colors", 13)

chocolate_ingredients_flavors_tidy %>%
  ggplot(aes((as.numeric(review_date)+2005), mean_score,color = feature)) +
  geom_point(
    size = 1,
  ) +
  geom_smooth(
    se = FALSE,
    linewidth = 0.5,
    method = "lm"
  ) +
  labs(title = "Features of Chocolate by Year",
       subtitle = "Decreases in cocoa butter, lectin, and vanillia in chocolate in the last decade.",
    x = "Year",
    y = "Average Prevalence") +
  scale_x_continuous(n.breaks=16) +
  theme_minimal() +
  scale_color_manual(values = paletteer_d("ggprism::colors", 13))

# part 5

chocolate_ingredients_flavors_tidy %>%
  ggplot(aes((as.numeric(review_date)+2005), mean_score,color = feature)) +
  geom_smooth(
    linetype = 3,
    se = TRUE,
    linewidth = 0.5,
    method = "loess"
  ) +
  scale_x_continuous(n.breaks=4) +
  scale_y_continuous(n.breaks=4) +
  theme_dark() +
  theme(legend.position = c(0.85, 0.5)) +
  labs(caption = "Trends in chocolate ingredients and characteristics over time")

# part 6
paletteer::scale_fill_paletteer_c("ggthemes::Sunset-Sunrise Diverging")

Rating_per_Year <- chocolate %>%
  ggplot(aes(
    x = as.factor(review_date),
    y = rating,
    fill = factor(review_date))) +
  geom_violin() +
  labs(title = "Average Chocolate Rating vs. Year",
       subtitle = "Smaller distributions in chocolate rating data since 2006 suggest greater consensus over rating.") +
  labs(
       x = "Year",
       y = "Average Rating") +
  bbplot::bbc_style() +
  theme(legend.position = "none",
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
  scale_fill_manual(values = paletteer_d("unikn::pal_unikn_pair"))

plotly::ggplotly((Rating_per_Year))

#removed legend
#labeled axes
#created title and subtitle
# centered title and subtitle
# new color scheme
# new theme

chocolate %>%
  group_by(country_of_bean_origin) %>%
  summarize(tibble(
    `number_of_ratings` = length(rating))) %>%
    unique() %>%
    arrange(desc(number_of_ratings))

