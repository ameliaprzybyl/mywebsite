---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: pic09.jpg
keywords: ""
slug: magna
title: Introductory Data Analysis
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

```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
```

# Rents in San Francsisco 2000-2018

[Kate Pennington](https://www.katepennington.org/data) created a panel of historic Craigslist rents by scraping posts archived by the Wayback Machine. More about her work,

> [What impact does new housing have on rents, displacement, and gentrification in the surrounding neighborhood? Read our interview with economist Kate Pennington about her article, "Does Building New Housing Cause Displacement?:The Supply and Demand Effects of Construction in San Francisco."](https://matrix.berkeley.edu/research-article/kate-pennington-on-gentrification-and-displacement-in-san-francisco/)

In our case, we have a clean(ish) dataset with about 200K rows that corresponds to Craigslist listings for renting properties in the greater SF area. The data dictionary is as follows

| variable    | class     | description           |
|-------------|-----------|-----------------------|
| post_id     | character | Unique ID             |
| date        | double    | date                  |
| year        | double    | year                  |
| nhood       | character | neighborhood          |
| city        | character | city                  |
| county      | character | county                |
| price       | double    | price in USD          |
| beds        | double    | n of beds             |
| baths       | double    | n of baths            |
| sqft        | double    | square feet of rental |
| room_in_apt | double    | room in apartment     |
| address     | character | address               |
| lat         | double    | latitude              |
| lon         | double    | longitude             |
| title       | character | title of listing      |
| descr       | character | description           |
| details     | character | additional details    |

The dataset was used in a recent [tidyTuesday](https://github.com/rfordatascience/tidytuesday) project.

```{r}
# download directly off tidytuesdaygithub repo

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv', show_col_types = FALSE)

```

## A glimpse

8 variables are character, while 9 are numeric. `descr` has most missing values (1975).

`date`, `year`, `beds`, `baths`, `room_in_apt` should be int instead of double.

```{r skim_data}
# show top 10 rows
head(rent, n = 10)

# glimpse
glimpse(rent)
# skim data
skimr::skim(rent) %>% 
  arrange(desc(n_missing))
```

## Ranking of Classifieds

```{r top_cities}
df_top_twenty <-
  rent %>%
    count(city) %>% #to count number of classifieds in each city
    arrange(desc(n)) %>% #arrange by highest number 
    slice(1:20) %>%
    mutate(percentage = n/sum(n)) 
ggplot(df_top_twenty, aes(x = percentage, y = fct_reorder(city, percentage))) + 
  geom_col() +
  theme_bw() + 
  labs(
    title = "San Francisco accounts for more than a quarter of all rental classifieds",
    subtitle = "% of Craigslist listings, 2000-2018",
    x = "Rental Classifieds Percentage",
    y = "City",
    caption = "Source: Pennington. Kate (2018). Bav Area Craigslist Rental Housing Posts 2000-2018") +
  scale_x_continuous(labels = scales::percent)
```

## Rentals evolution in San Francisco

```{r sf_median_prices}
medianPrices <- rent %>%
    filter(city == "san francisco", beds <= 3) %>%
    group_by(beds, year) %>%
    summarize(medianYear = median(price))
    
ggplot(medianPrices, aes(year, medianYear, color = factor(beds))) +
  geom_line() + 
  facet_wrap(vars(beds), nrow = 1) +
  theme_bw() + 
  labs(
    title = "San Francisco rentals have been steadily increasing",
    subtitle = "0 to 3-bed listings, 2000-2018",
    x = "Year",
    y = "Rental Price",
    caption = "Source: Pennington. Kate (2018). Bay Area Craigslist Rental Housing Posts 2000-2018") +
  theme(legend.position = "None")
```

## Rentals Evolution in the top 12 cities

```{r rental_4}
top_twelve_data <- rent %>%
  count(city) %>% #to count number of classifieds in each city
  arrange(desc(n)) %>%
  slice(1:12) 
top_twelve_cities <- as.vector(top_twelve_data$city)
top_twelve_cities

rent %>%
  filter(city %in% top_twelve_cities) %>%
  group_by(year, city) %>%
  summarize(median_price = median(price)) %>%
ggplot(aes(year, median_price, color = factor(city))) +
  geom_line() +
  facet_wrap(vars(city)) +
  labs(
  title = "Rental prices for flats in the Bay Area",
  x = "Year",
  y = "Rental Price",
  caption = "Source: Pennington. Kate (2018). Bay Area Craigslist Rental Housing Posts 2000-2018") +
  theme(legend.position = "None")
```

## Conclusion

What can you infer from these plots? Don't just explain what's in the graph, but speculate or tell a short story (1-2 paragraphs max).

> There has been a overall increase in rental prices in the Bay Area between 2000 and 2018. This reflects the tech boom in the Bay Area. In particular, Palo Alto, the birthplace of Sillicon Valley, has experienced the highest rental increase. On the other hand, cities like Santa Rosa and San Jose, have experienced relatively lower rental increase. This increase can probably be accounted for by inflation adjustments. Most cities experienced a dip in prices around 2008-2010. We believe this reflects the housing bubble burst of 2008, which triggered a global financial crisis. 

# Analysis of movies- IMDB dataset

The data is taken from the [Kaggle IMDB 5000 movie dataset](https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset)

Besides the obvious variables of `title`, `genre`, `director`, `year`, and `duration`, the rest of the variables are as follows:

-   `gross` : The gross earnings in the US box office, not adjusted for inflation
-   `budget`: The movie's budget
-   `cast_facebook_likes`: the number of facebook likes cast members received
-   `votes`: the number of people who voted for (or rated) the movie in IMDB
-   `reviews`: the number of reviews for that movie
-   `rating`: IMDB average rating

## Glimpse

-   As shown below, there is no missing values or duplicate entries <!-- not sure -->

-   However, some movies are recorded for multiple times with some inconsistent feature values (same title).

    -   inconsistent features: `cast_facebook_likes`, `votes`, `reviews`

```{r overview_imdb}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)
# overview of the dataset
## the total observation, variables' types, 
skimr::skim(movies)

# check duplicates
distinct(movies) %>% 
  summarize(n_tot_obs = n())

# duplicated titles
movies %>% 
  summarize(n_unique_titles = n_distinct(title))

# which features are consistent? 
movies %>% 
  group_by(title) %>% 
  mutate(n_title = n()) %>% 
  filter(n_title > 1) %>% 
  summarize_all(n_distinct) %>% 
  summarize_all(sd)
```

Over 2k movies are Comedy, Action, or Drama.

```{r cnt_by_genre_desc}
# Produce a table with the count of movies by genre, ranked in descending order
movies %>% 
  group_by(genre) %>% 
  summarize(n_movies = n_distinct(title)) %>% 
  arrange(desc(n_movies))
```

The average gross, budget, and return on budget by genre are shown below.

```{r avg_gross_budget_by_genre}
# average gross and budget
movies %>% 
  distinct(title, .keep_all = TRUE) %>% 
  group_by(genre) %>% 
  summarize(avg_gross = mean(gross), avg_budget = mean(budget)) %>% 
  mutate(return_on_budget = avg_gross / avg_budget) %>% 
  arrange(desc(return_on_budget))
```

The top 15 directors who have created the highest gross revenue in the box office, including the total, mean, median, and standard deviation value of gross amount per director.

```{r top_15_directors}
# top 15 directors
movies %>% 
  distinct(title, .keep_all = TRUE) %>% 
  group_by(director) %>% 
  summarize(tot_gross = sum(gross), avg_gross = mean(gross), med_gross = median(gross), std_gross = sd(gross)) %>% 
  slice_max(order_by = tot_gross, n = 15)
```

Distribution of ratings by genre

```{r rating_distr_by_genre}
movies %>% 
  distinct(title, .keep_all = TRUE) %>% 
  group_by(genre) %>% 
  summarize(avg_r = mean(rating), min_r = min(rating), max_r = max(rating), med_r = median(rating), std_r = sd(rating), n_movies = n())

movies %>% 
  distinct(title, .keep_all = TRUE) %>% 
  ggplot(aes(x = rating)) + 
  geom_density(alpha = 0.2, fill = "grey") + 
  facet_wrap(~ genre) + 
  theme_bw() + 
  labs(
    title = "Movie ratings distribution by genre",
    x = "Rating", 
    y = "Density", 
    caption = "Source: https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset")
```

## Finding Predictors for Gross - Graphs

-   The number of facebook likes that the cast has received does not seem to be a suitable predictor of gross income, the gross value is widely spread for movies of the same likes amount.
-   The Y denotes the dependent variable (gross), the X denotes the independent variable (cast_facebook_likes).

```{r, gross_on_fblikes}
movies %>% 
  group_by(title) %>% 
  summarize(gross = mean(gross), cast_facebook_likes = mean(cast_facebook_likes)) %>% 
  ggplot(aes(x = cast_facebook_likes, y = gross)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
```

-   Budget is not likely to be a good predictor of gross.

```{r, gross_on_budget}
movies %>% 
  group_by(title) %>% 
  summarize(gross = mean(gross), budget = mean(budget)) %>% 
  ggplot(aes(x = budget, y = gross)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
```

-   There seems to be a positive correlation between gross and rating within some particular genres - **Action**, **Adventure**, **Biography**, **Comedy**, **Drama**.
-   But the rating still seems to be a bad predictor for gross among the whole dataset.

## Strange things in the dataset:

duplicate entries - several movies have duplicate records with inconsistent records. - When working on the above tasks, we took only one records into analysis and drop the others if the inspected feature was consistent (e.g., `gross`, `rating`, etc.). - If the inspected feature was inconsistent (`cast_facebook_likes`, `votes`, `reviews`), we used the average value for analysis.

```{r, gross_on_rating}
movies %>% 
  group_by(title, genre) %>% 
  summarize(gross = mean(gross), rating = mean(rating)) %>% 
  ggplot(aes(x = rating, y = gross)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~genre) + 
  geom_smooth(method = "lm", se = FALSE)

movies %>% 
  group_by(title, genre) %>% 
  summarize(gross = mean(gross), rating = mean(rating)) %>% 
  ggplot(aes(x = rating, y = gross)) + 
  geom_point(alpha = .2)
```

# Returns of financial stocks

> useful material [finance data sources](https://mam2023.netlify.app/reference/finance_data/).

```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- readr::read_csv(here::here("data","nyse.csv"))
```

## The number of companies per sector

```{r companies_per_sector}
#Understanding the data
head(nyse)

#Creating a table that shows the number of companies per sector, in descending order
descending_sector <- nyse %>%
  dplyr::group_by(sector) %>%
  count(sector) %>%
  arrange(desc(sector))
descending_sector

#Creating a bar plot with the number of companies per sector, in descending order

ggplot(descending_sector, aes(x=reorder(sector, -n), y = n)) + 
  geom_bar(stat = "identity") +
  labs(title = "Number of companies by sector",
    x = "Sector",
    y = "Number of companies")
```

Next, let's choose some stocks and their ticker symbols and download some data. You **MUST** choose 6 different stocks from the ones listed below; You should, however, add `SPY` which is the SP500 ETF (Exchange Traded Fund).

## Picking stocks

Stocks we chose: `AAPL`, `JPM`, `ANF`, `TSLA`, `XOM`, `SPY`

```{r get_price_data, message=FALSE, warning=FALSE, cache=FALSE}

myStocks <- c("AAPL","JPM","ANF","TSLA","XOM","SPY" ) %>%
  tq_get(get  = "stock.prices",
         from = "2011-01-01",
         to   = "2022-08-31") %>%
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame
```

## Daily and monthly returns.

```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```

Summarizing monthly returns for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}
monthly_returns_summary <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(mean_monthly_return = mean (monthly_returns, na.rm=TRUE),
      median_monthly_return = median (monthly_returns, na.rm=TRUE),
      sd_monthly_return = sd (monthly_returns, na.rm=TRUE),
      min_monthly_return = min (monthly_returns, na.rm=TRUE),
      max_monthly_return = max (monthly_returns, na.rm=TRUE))
monthly_returns_summary
```

Density plot for each of the stocks:

```{r density_monthly_returns}
plot1 <- myStocks_returns_monthly %>%
  group_by(symbol) %>% 
  ggplot(aes(x = monthly_returns)) +
   facet_wrap(~symbol, scales = "free")+
   geom_density()
plot1
```

> We can see that most plots, apart from Apple and Tesla follow a normal distribution. We can also determine that TSLA is the stock with the highest risk. Not only is the distribution heavily skewed right, but the monthly retuns on the most likely return are lower than zero. XOM on the other hand is the least risky stock, as it's density plot is heavily concentrated amongst one value, 0. Therefore we can infer that it's variability is fairly low.

## Risk & Return

```{r risk_return_plot}
plot2 <- ggplot(data = monthly_returns_summary, mapping = aes(x = sd_monthly_return, y = mean_monthly_return)) +
   geom_point() +
   ggrepel::geom_text_repel(aes(label=symbol))+
   labs(title = "Risk/return by stock",
        x = "Risk",
        y = "Return")
plot2
```

> As we can see based on this plot, higher return is typically correlated with higher risk. However, some stocks, like for example ANF, have higher risk without there being a significant increase in the returns. As an investor, you can use this graph to determine not to invest in a stock like ANF.

# On your own: Spotify

<!-- Spotify have an API, an Application Programming Interface. APIs are ways for computer programs to talk to each other. So while we use Spotify app to look up songs and artists, computers use the Spotify API to talk to the spotify server. There is an R package that allows R to talk to this API: [`spotifyr`](https://www.rcharlie.com/spotifyr/). One of your team members, need to sign up and get a [Spotify developer account](https://developer.spotify.com/dashboard/) and then you can download data about one's Spotify usage. A detailed article on how to go about it can be found here [Explore your activity on Spotify with R and *spotifyr*](https://towardsdatascience.com/explore-your-activity-on-spotify-with-r-and-spotifyr-how-to-analyze-and-visualize-your-stream-dee41cb63526) -->

<!-- If you do not want to use the API, you can download a sample of over 32K songs by having a look at <https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md> -->

```{r, download_spotify_data}

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')


```

The data dictionary can be found below

| **variable**             | **class** | **description**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
|-------------------|-------------------|----------------------------------|
| track_id                 | character | Song unique ID                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| track_name               | character | Song Name                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| track_artist             | character | Song Artist                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| track_popularity         | double    | Song Popularity (0-100) where higher is better                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| track_album_id           | character | Album unique ID                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| track_album_name         | character | Song album name                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| track_album_release_date | character | Date when album released                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| playlist_name            | character | Name of playlist                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| playlist_id              | character | Playlist ID                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| playlist_genre           | character | Playlist genre                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| playlist_subgenre        | character | Playlist subgenre                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| danceability             | double    | Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.                                                                                                                                                                                                                                                                       |
| energy                   | double    | Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.                                                                                                                          |
| key                      | double    | The estimated overall key of the track. Integers map to pitches using standard Pitch Class notation . E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on. If no key was detected, the value is -1.                                                                                                                                                                                                                                                                                                                            |
| loudness                 | double    | The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.                                                                                                                                                                                       |
| mode                     | double    | Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0.                                                                                                                                                                                                                                                                                                                                                    |
| speechiness              | double    | Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks. |
| acousticness             | double    | A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.                                                                                                                                                                                                                                                                                                                                                                                       |
| instrumentalness         | double    | Predicts whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.                                                                                                                 |
| liveness                 | double    | Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.                                                                                                                                                                                                                                                                                            |
| valence                  | double    | A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).                                                                                                                                                                                                                                                                  |
| tempo                    | double    | The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.                                                                                                                                                                                                                                                                                                                         |
| duration_ms              | double    | Duration of song in milliseconds                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |

<!-- In this dataset, there are only 6 types of `playlist_genre` , but we can still try to perform EDA on this dataset. -->

<!-- Produce a one-page summary describing this dataset. Here is a non-exhaustive list of questions: -->

<!-- 1.  What is the distribution of songs' popularity (`track_popularity`). Does it look like a Normal distribution? -->

<!-- 2.  There are 12 [audio features](https://developer.spotify.com/documentation/web-api/reference/object-model/#audio-features-object) for each track, including confidence measures like `acousticness`, `liveness`, `speechines`and `instrumentalness`, perceptual measures like `energy`, `loudness`, `danceability` and `valence` (positiveness), and descriptors like `duration`, `tempo`, `key`, and `mode`. How are they distributed? can you roughly guess which of these variables is closer to Normal just by looking at summary statistics? -->

<!-- 3.  How are `job_satisfaction` and `work_life_balance` distributed? Don't just report counts, but express categories as % of total -->

<!-- 4.  Is there any relationship between `valence` and `track_popularity`? `danceability` and `track_popularity` ? -->

<!-- 5.  `mode` indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0. Do songs written on a major scale have higher `danceability` compared to those in minor scale? What about `track_popularity`? -->

## Popularity normal distribution

```{r spotify_pop_distr}

ggplot(spotify_songs)+aes(x=track_popularity)+geom_histogram(binwidth=5)

spotify_songs_no0<-filter(spotify_songs,track_popularity>0)
ggplot(spotify_songs_no0)+aes(x=track_popularity)+geom_histogram(binwidth=5)
```

> The distribution appears close to normal for track_popularity between 20 to 100. However, a spike can be observed for songs with little to no popularity. From the sample data of over 32000 songs, X have 0 track_popularity. The second graph generated excludes those songs from the dataset. The normal distribution can be observed with more ease.

## Distribution of the 12 audio features

```{r audio_12}
ggplot(spotify_songs)+aes(x=acousticness)+geom_histogram(binwidth=0.05) # skewed right

summary(spotify_songs)

ggplot(spotify_songs)+aes(x=valence)+geom_histogram() #  closest to normal

ggplot(spotify_songs)+aes(x=tempo)+geom_histogram() # summary data suggests close to normal but fails to pick up the shape
```

> The audio features vary in terms of distributions. Most, such as acousticness below are skewed.

> From the summary, we can try determining which of the features is closer to normal. A 'perfect' normal distribution has equal mean and median, the lower and upper interquartile range should have equal distance to the median. Likewise, the distance from the min value --\> mean distance should equal the mean --\> max value distance. The feature closest to these properties is the valence. Illustrated below, we can see it follows a normal-type distriution. Conversely, the tempo also has features close to the theoretical one. However, it appears to have a primary and secondary peak which is not a characteristic of normal curves. The summary can therefore be used as an approximation of the distribution type but cannot pick up more complex characteristics.

## Track popularity relationship

> There does not appear to be a relationship between valence and track_popularity.

```{r valence_relationship}
ggplot(spotify_songs)+aes(x=valence, y=track_popularity)+geom_point()
```

> Very few songs have danceability under 0.25. There appears to be a very weak positive correlation between track_popularity and danceability. The most popular songs have danceability between 0.5 and 0.8. However, it is also the danceability level of most songs, many off which are not popular.

```{r danceability_relationship}
ggplot(spotify_songs)+aes(x=danceability, y=track_popularity)+geom_point()
```

## Major and minor modality

```{r modality_major_minor}
filter(spotify_songs,mode==1)%>%
summarise(mean(danceability),mean(track_popularity))

filter(spotify_songs,mode==0)%>%
summarise(mean(danceability),mean(track_popularity))

# distribution of danceability and popularity of each mode
library(patchwork)
p1 <- ggplot(data = spotify_songs) + 
  aes(x = danceability, color = factor(mode)) + 
  geom_density()
p2 <- ggplot(data = spotify_songs) + 
  aes(x = track_popularity, color = factor(mode)) + 
  geom_density()
p1 + p2
```

Songs in minor modality are slightly more danceable than major ones but also slightly less popular.

# Challenge 1: Replicating a chart

```{r read_data}
# Read Data ---------------------------------------------------------------
#
rent <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv'
)
```

```{r challenge_1, message=FALSE, warnings=FALSE}
library(DataCombine)

rent
top_twelve_data <- rent %>%
  count(city) %>% #to count number of classifieds in each city
  arrange(desc(n)) %>%
  slice(1:12) 
top_twelve_cities <- as.vector(top_twelve_data$city)
top_twelve_cities

mean_price_each_year <- rent %>%
    filter(beds <= 2, city %in% top_twelve_cities) %>%
    group_by(city, beds, year) %>%
    summarize(median_price = median(price))
mean_price_each_year  

cumulative_percentage_change <- mean_price_each_year %>%
  group_by(city, beds) %>%
  mutate(percent_change = (median_price/lag(median_price))) %>% 
  mutate(percent_change = ifelse(is.na(percent_change), 1, percent_change)) %>%
  mutate(cumulative_percent_change = cumprod(percent_change))
cumulative_percentage_change

ggplot(cumulative_percentage_change,aes(x = year, y = cumulative_percent_change,color= city)) + 
  geom_line() + 
  facet_grid(beds ~ city,scales= "free_y") + 
  theme_bw(base_size = 9)  +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 100))  + 
  scale_x_continuous(breaks = seq(2005, 2018, by = 5)) + 
  labs(title = "Cumulative % change in 0,1, and 2-bed rentals in Bay Area", 
       subtitle = "2000-2018")

```

# Challenge 2: 2016 California Contributors plots

## Top 10 cities for Hillary and Trump

> The top ten cities in highest amounts raised in political contributions in California during the 2016 US Presidential election.

```{r load_pkg}
library(patchwork)
library(tidytext)
```

```{r challenge_2, message=FALSE, warnings=FALSE}

CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))
glimpse(CA_contributors_2016)
zipcodes <- vroom::vroom(here::here("data","zip_code_database.csv"))
zipcodes <- mutate(zipcodes, zip = as.double(zip))
contributors <- left_join(CA_contributors_2016, zipcodes, by = "zip")
hillary <- contributors %>%
  filter(cand_nm == "Clinton, Hillary Rodham") %>%
  group_by(primary_city) %>%
  summarize(contb_receipt_amt = sum(contb_receipt_amt)) %>%
  slice_max(order_by = contb_receipt_amt, n = 10) %>%
  mutate(city = fct_reorder(primary_city, contb_receipt_amt, max))
#hillary
p1 <- ggplot(hillary, aes(contb_receipt_amt, city)) +
  geom_col(fill = "blue") + 
  labs(title = "Clinton, Hillary Rodham",
   x = NULL,
   y = NULL) + 
  theme_bw() + 
  scale_x_continuous(labels = scales::dollar_format()) +
  theme(text = element_text(size = 9),element_line(size =1))

#p1

trump <- contributors %>%
  filter(cand_nm == "Trump, Donald J.") %>%
  group_by(primary_city) %>%
  summarize(contb_receipt_amt = sum(contb_receipt_amt)) %>%
  slice_max(order_by = contb_receipt_amt, n = 10) %>%
  mutate(city = fct_reorder(primary_city, contb_receipt_amt, max))
#trump
p2 <- ggplot(trump, aes(contb_receipt_amt, city)) +
  geom_col(fill = "red") + 
  labs(title = "Trump, Donald J.",
   x = NULL,
   y = NULL) + 
  theme_bw() + 
  scale_x_continuous(labels = scales::dollar_format())  +
  theme(text = element_text(size = 9),element_line(size =1))
#p2

p1 + p2 + plot_annotation(
  title = "Where did candidates raise most money?"
)
```

## Top 10 cities for the top 10 candidates

```{r challenge_2_2}
#CA_contributors_2016
zipcodes <- vroom::vroom(here::here("data","zip_code_database.csv"))
zipcodes <- mutate(zipcodes, zip = as.double(zip))
contributors <- left_join(CA_contributors_2016, zipcodes, by = "zip")
#contributors

top_ten <- contributors %>%
  group_by(cand_nm) %>%
  summarise(total_price = sum(contb_receipt_amt)) %>%
  slice_max(order_by = total_price, n=10)
#top_ten

library(tidytext)

contributors %>%
  filter(cand_nm %in% top_ten$cand_nm) %>%
  group_by(cand_nm, primary_city) %>%
  summarize(contb_receipt_amt = sum(contb_receipt_amt)) %>%
  arrange(cand_nm, desc(contb_receipt_amt)) %>%
  top_n(10, contb_receipt_amt) %>%
  mutate(primary_city = reorder_within(primary_city, contb_receipt_amt, cand_nm)) %>%
  ggplot(aes(contb_receipt_amt, primary_city)) +
  geom_col() + 
  facet_wrap(vars(factor(cand_nm)), scales = "free") +
  labs(
    title = "Where did the top 10 candidates raise most money?",
    x = NULL,
    y = NULL
  ) + 
  scale_y_reordered() + 
  scale_x_continuous(labels = scales::dollar)
```

# Deliverables

There is a lot of explanatory text, comments, etc. You do not need these, so delete them and produce a stand-alone document that you could share with someone. Knit the edited and completed R Markdown file as an HTML document (use the "Knit" button at the top of the script editor window) and upload it to Canvas.

# Details

-   Who did you collaborate with: (alphabetical order)

    -   Amelia Przybyl, Athos Gyalui, Drishti Hoskote, Mingyu Dai, San Kashyap

-   Approximately how much time did you spend on this problem set: 5-6 hours each group member

-   What, if anything, gave you the most trouble:

    -   making the plots readable and *not* bad-looking
    -   dealing with missing or duplicated data
    -   understanding quesitons (part of them)

**Please seek out help when you need it,** and remember the [15-minute rule](https://mam2022.netlify.app/syllabus/#the-15-minute-rule){target="_blank"}. You know enough R (and have enough examples of code from class and your readings) to be able to do this. If you get stuck, ask for help from others, post a question on Slack-- and remember that I am here to help too!

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else?

> Yes.

# Rubric

Check minus (1/5): Displays minimal effort. Doesn't complete all components. Code is poorly written and not documented. Uses the same type of plot for each graph, or doesn't use plots appropriate for the variables being analyzed.

Check (3/5): Solid effort. Hits all the elements. No clear mistakes. Easy to follow (both the code and the output).

Check plus (5/5): Finished all components of the assignment correctly and addressed both challenges. Code is well-documented (both self-documented and with additional comments as necessary). Used tidyverse, instead of base R. Graphs and tables are properly labelled. Analysis is clear and easy to follow, either because graphs are labeled clearly or you've written additional text to describe how you interpret the output.

