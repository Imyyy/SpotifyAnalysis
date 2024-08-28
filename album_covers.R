## Set working directory

setwd("your-working-directory")

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

## Set up credentials for working with the Spotify API

Sys.setenv(SPOTIFY_CLIENT_ID = 'your-spotify-client-id')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your-spotify-client-secret')

access_token <- get_spotify_authorization_code()

## Let's go!

playlist_id <- "your-playlist-id" ## Spotify playlist ID of the playlist you want to generate the cover for
n_colors <- 20 ## Maximum number of colors extracted from the individual album covers

## Create tibble of class 'magick-image' with the corresponding values for each album cover

images <- get_playlist_tracks(playlist_id, fields = c("track.album.name","track.album.images")) %>% 
  as_tibble() %>%
  arrange(track.album.name) %>% 
  hoist(track.album.images, url = "url") %>% 
  unnest_wider(url) %>% 
  pull(1) %>% 
  image_read() %>% 
  image_resize("100")

## Extract the color palette from all the covers

cover_palette <- lapply(1:length(images), function(x)
  images[x] %>% 
    image_quantize(max = n_colors, colorspace = "RGB") %>%
    magick2cimg() %>%  
    as.data.frame(wide = "c") %>% 
    mutate(hex = rgb(red = rescale(c.1), green = rescale(c.2), blue = rescale(c.3),
                     maxColorValue = 1),
           red = c.1, green = c.2, blue = c.3) %>%
    count(hex, red, green, blue, name = "count") %>%
    add_column(id = x) 
) %>% bind_rows() %>% 
  bind_cols(., rgb2hsv(r = .$red, g = .$green, b = .$blue, maxColorValue = 1) %>% 
              as.data.frame() %>% t() %>% as_tibble()) %>% 
  select(id, everything()) %>% 
  mutate(lum = sqrt(0.299 * red + 0.587 * green + 0.114 * blue)) %>% 
  arrange(h, lum, v, red, green, blue)

## Generate plot

p <- ggplot(cover_palette, aes(id, fill = hex)) +
  geom_bar() +
  theme_palette() +
  scale_fill_identity() +
  scale_x_discrete() +
  scale_y_continuous(breaks = NULL) +
  coord_polar() +
  expand_limits(y = -2) 

p
