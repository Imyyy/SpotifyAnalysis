#NEED TO WORK TO ITERATE ALL THE PLAYLISTS
## Load libraries, you might need to install them first (install.packages)
library(tidyverse)
library(lubridate)
library(spotifyr)
library(magick)
library(imager)
library(scales) 
library(showtext) #Imy adding this in to try and get a nice font on the graph

## Custom ggplot2 theme
theme_palette <- function () { 
  theme_bw(base_size=10, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "black"), 
      plot.margin = margin(5),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
}
## Set up credentials for working with the Spotify AP
Sys.setenv(SPOTIFY_CLIENT_ID = 'c61c9eacc62348afb0c7b5a45f323d64') # 'your-spotify-client-id'
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'baea283d1f174bf78809115f018ae993') # 'your-spotify-client-secret'
username <- "imy147" ## Spotify username of the owner of the playlist. If it's not you, the playlist must be public
#access_token <- get_spotify_authorization_code()

#Select which playlist you want 
playlist_id <- "0WoCSljHvzavVQJyAvCCYF" ## Spotify playlist ID of the playlist you want to generate the cover for

#Trying to make it go through the list automatically, going to need a for loop and a dataframe

#G D S = spotify:playlist:1fusY0JyJ1471UckBxsQsJ
#Canada 2019 =spotify:playlist:0xtwWYoMPvOfQB0mgkJTZQ
#DID = spotify:playlist:5Im4bcnijkzuNklI5pbnl3
#CC = spotify:playlist:0bf0dgzr3nCFYOPMmLIWIM
#FYP = spotify:playlist:0va8UKDUxo66aYrJmladkS
#Bio = spotify:playlist:2CDtAeZKqqHkpJS23wUyg7 spotify:playlist:2CDtAeZKqqHkpJS23wUyg7
#Homehome = spotify:playlist:2CDtAeZKqqHkpJS23wUyg7 spotify:playlist:6PeoxOqBsJH3VDUHZs13sx
#Frisbee womens indoor nats = spotify:playlist:7frpLkZcpJjECuPYo0Wsyg
p_name <- c("concentration" , " dD Women's Burgess Hill adventure", "D Y B", "Run165", "G D S", 
            "Canada 2019", "D I D", "Country concentration", "FYP Concentration", "Bio", 
            "HomeHome", "Frisbee Womens Indoor Nats Glasgow")
p_id <- c("3o2BfNZ6JFHKt8MjfxL616", "0WoCSljHvzavVQJyAvCCYF", "604AyaWTIM1WIF28fI2kGl", "7AbwoYgj7W2tGJt8DuDIbT", "1fusY0JyJ1471UckBxsQsJ",
          "0xtwWYoMPvOfQB0mgkJTZQ", "5Im4bcnijkzuNklI5pbnl3", "0bf0dgzr3nCFYOPMmLIWIM", 
          "0va8UKDUxo66aYrJmladkS", "2CDtAeZKqqHkpJS23wUyg7", "6PeoxOqBsJH3VDUHZs13sx", "7frpLkZcpJjECuPYo0Wsyg")
playlists <- data.frame(p_name, p_id)

red <- "energy" ## Feature which determines the red component of the color
green <- "valence" ## Feature which determines the green component of the color
blue <- "danceability" ## Feature which determines the blue component of the color
height_feature <- "tempo"
## Possible inputs
  #valence = positiveness
  #instrumentalness
  #speechiness
  #energy
  #danceability

## Make sure that dplyr can get the column names as they are stored in a character vector
red <- ensym(red)
green <- ensym(green)
blue <- ensym(blue)

## Get track features for the given playlist
#full_features <- get_playlist_audio_features(playlist_name, playlist_id)
features <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, 
         track_id = track.id, popularity = track.popularity,
         danceability, energy, speechiness, acousticness,
         instrumentalness, liveness, valence,
         key_name, mode_name, key_mode, tempo)

## Create color palette based on the chosen features
cover_palette <- features %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")

## Try and personalise the colour palette

## Generate plot
#Then want to try and add a legend and a title to the plot
p <- ggplot(cover_palette, aes(id, get(height_feature), fill = color)) +
  geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() +
  scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
#p + labs(title="dD Womens Burgess Hill adventure", caption = "Data source: ToothGrowth", family="Courier")

# Running it for each playlist

#Concentration
playlist_id <- "3o2BfNZ6JFHKt8MjfxL616" 
features1 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette1 <- features1 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p1 <- ggplot(cover_palette1, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p1
#dD women bh ad
playlist_id <- "0WoCSljHvzavVQJyAvCCYF" 
features2 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette2 <- features2 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p2 <- ggplot(cover_palette2, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p2
# D Y B
playlist_id <- "604AyaWTIM1WIF28fI2kGl" 
features3 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette3 <- features3 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p3 <- ggplot(cover_palette3, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p3
#Run165
playlist_id <- "7AbwoYgj7W2tGJt8DuDIbT" 
features4 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette4 <- features4 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p4 <- ggplot(cover_palette4, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p4
#G D S
playlist_id <- "1fusY0JyJ1471UckBxsQsJ" 
features5 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette5 <- features5 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p5 <- ggplot(cover_palette5, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p5
#Canada 2019
playlist_id <- "0xtwWYoMPvOfQB0mgkJTZQ" 
features6 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette6 <- features6 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p6 <- ggplot(cover_palette6, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p6
#D I D
playlist_id <- "5Im4bcnijkzuNklI5pbnl3" 
features7 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette7 <- features7 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p7 <- ggplot(cover_palette7, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p7
#Country concentration
playlist_id <- "0bf0dgzr3nCFYOPMmLIWIM" 
features8 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette8 <- features8 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p8 <- ggplot(cover_palette8, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p8
#FYP concentration
playlist_id <- "0va8UKDUxo66aYrJmladkS" 
features9 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette9 <- features9 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p9 <- ggplot(cover_palette9, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p9
#Bio
playlist_id <- "2CDtAeZKqqHkpJS23wUyg7" 
features10 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette10 <- features10 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p10 <- ggplot(cover_palette10, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p10
#HomeHome
playlist_id <- "6PeoxOqBsJH3VDUHZs13sx" 
features11 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette11 <- features11 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p11 <- ggplot(cover_palette11, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p11
#Frisbee womens indoor nats glsgow
playlist_id <- "7frpLkZcpJjECuPYo0Wsyg" 
features12 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette12 <- features12 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p12 <- ggplot(cover_palette12, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p12
#IML
playlist_id <- "1S65WOlnMfB0MALsquSJrg"
features13 <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id, track_name = track.name, track_id = track.id, popularity = track.popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence, key_name, mode_name, key_mode, tempo)
cover_palette13 <- features13 %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue, maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, g = !!green, b = !!blue, maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")
p13 <- ggplot(cover_palette13, aes(id, get(height_feature), fill = color)) + geom_bar(stat = "identity") +theme_palette() + scale_fill_identity() + scale_x_discrete() + scale_y_continuous(breaks = NULL) + coord_polar() + expand_limits(y = -1) 
p13

# THE for loop attempt
for (i in playlists) {
  playlist_id = as.character(playlists$p_id)
  features <- get_playlist_audio_features(username = username, playlist_uris = playlist_id) %>% 
    select(playlist_name, playlist_id,
           track_name = track.name, 
           track_id = track.id,
           popularity = track.popularity,
           danceability, energy, speechiness, acousticness,
           instrumentalness, liveness, valence,
           key_name, mode_name, key_mode, tempo)
  cover_palette <- features %>% 
    rowwise() %>% 
    mutate(color_hsv = rgb2hsv(r = !!red, g = !!green, b = !!blue,  maxColorValue = 1) %>% list(),
           color = rgb(r = !!red, g = !!green, b = !!blue,maxColorValue = 1)) %>% 
    unnest_wider(color_hsv) %>% 
    arrange(...1, ...2) %>% 
    rowid_to_column(var = "id")
  print(ggplot(cover_palette, aes(id, get(height_feature), fill = color)) +
          geom_bar(stat = "identity") +
          theme_palette() +
          scale_fill_identity() +
          scale_x_discrete() +
          scale_y_continuous(breaks = NULL) +
          coord_polar())
}