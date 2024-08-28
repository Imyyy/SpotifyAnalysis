#NEED TO WORK TO ITERATE ALL THE PLAYLISTS

## Load libraries, you might need to install them first (install.packages)
library(tidyverse)
library(lubridate)
library(spotifyr)
library(magick)
library(imager)
library(scales)

## Custom ggplot2 theme
theme_palette <- function () { 
  theme_bw(base_size=10, base_family="Avenir") %+replace% 
    theme(
      panel.background  = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "black"), 
      plot.title = element_blank(),
      plot.margin = margin(10),
      legend.position="none",
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
playlist_id <- "3o2BfNZ6JFHKt8MjfxL616" ## Spotify playlist ID of the playlist you want to generate the cover for

#Trying to make it go through the list automatically, going to need a for loop and a dataframe

#concentration = "3o2BfNZ6JFHKt8MjfxL616"
#dD Women Burgess Hill = 0WoCSljHvzavVQJyAvCCYF
#D Y B = spotify:playlist:604AyaWTIM1WIF28fI2kGl
#Run165 = spotify:playlist:7AbwoYgj7W2tGJt8DuDIbT
#G D S = spotify:playlist:1fusY0JyJ1471UckBxsQsJ
#Canada 2019 =spotify:playlist:0xtwWYoMPvOfQB0mgkJTZQ
#DID = spotify:playlist:5Im4bcnijkzuNklI5pbnl3
#CC = spotify:playlist:0bf0dgzr3nCFYOPMmLIWIM
#FYP = spotify:playlist:0va8UKDUxo66aYrJmladkS
#Bio = spotify:playlist:2CDtAeZKqqHkpJS23wUyg7
#Homehome = spotify:playlist:2CDtAeZKqqHkpJS23wUyg7
#Frisbee womens indoor nats = spotify:playlist:7frpLkZcpJjECuPYo0Wsyg

red <- "danceability" ## Feature which determines the red component of the color
green <- "energy" ## Feature which determines the green component of the color
blue <- "valence" ## Feature which determines the blue component of the color
#height_feature <- "popularity" ## Feature which determines the height of the individual bars
height_feature <- "tempo"
## Make sure that dplyr can get the column names as they are stored in a character vector

red <- ensym(red)
green <- ensym(green)
blue <- ensym(blue)

## Get track features for the given playlist
features <- get_playlist_audio_features(username = username,
                                        playlist_uris = playlist_id) %>% 
  select(playlist_name, playlist_id,
         track_name = track.name, track_id = track.id,
         popularity = track.popularity,
         danceability, energy, speechiness, acousticness,
         instrumentalness, liveness, valence,
         key_name, mode_name, key_mode,
         tempo)

## Create color palette based on the chosen features

cover_palette <- features %>% 
  rowwise() %>% 
  mutate(color_hsv = rgb2hsv(r = !!red, 
                             g = !!green, 
                             b = !!blue, 
                             maxColorValue = 1) %>% list(),
         color = rgb(r = !!red, 
                     g = !!green, 
                     b = !!blue,
                     maxColorValue = 1)) %>% 
  unnest_wider(color_hsv) %>% 
  arrange(...1, ...2) %>% 
  rowid_to_column(var = "id")

## Generate plot

p <- ggplot(cover_palette, aes(id, get(height_feature), fill = color)) +
  geom_bar(stat = "identity") +
  theme_palette() +
  scale_fill_identity() +
  scale_x_discrete() +
  scale_y_continuous(breaks = NULL) +
  coord_polar() +
  expand_limits(y = -2) 

p
