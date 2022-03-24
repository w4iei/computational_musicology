# Week 12 Lab
library(ggplot2)
library(tidyverse)
library(dendextend)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)


bach_prelude_playlist_features = get_playlist_audio_features("", "4yNYY3xmNhPTDrfFc0qG9b") %>%
  mutate(track.duration_min = track.duration_ms/1000/60)
# Unwrap Tempo
tempo_threshold = 95;
time_threshold = 1.5;
bach_prelude_playlist_features = bach_prelude_playlist_features %>% 
  mutate(corrected_tempo = case_when(tempo>tempo_threshold & track.duration_min > time_threshold ~ tempo * 0.5, TRUE ~ tempo))

get_best_artist = function(artists){
  if (length(artists[['name']]) > 1) {
      artists[['name']][2] 
  }
    else {
     artists[['name']][1]
    }
}

get_yr = function(release_string){
  str_split(release_string, '-')[[1]][1]
}

meta_df = transmute(bach_prelude_playlist_features,
                    artist = map(track.artists, get_best_artist),
                    release_year = as.numeric(map(track.album.release_date, get_yr)),
                    corrected_tempo = corrected_tempo,
                    track.track_number = track.track_number, 
                    track.album.album_type = track.album.album_type)

extracted_features <- bach_prelude_playlist_features %>% 
  transmute(
    track.popularity = as.numeric(track.popularity, stringsAsFactors = FALSE)
         )
scaled_features = scale(extracted_features)
distances = dist(scaled_features)
hc <- hclust(distances, method = 'complete')
dend = as.dendrogram(hc)
labels(dend) = meta_df[['artist']]
cols <- c("#1b5e20", "#c8e6c9", "#81c784",)
dend <- color_branches(dend, k = 3, col = cols)
plot(rev(dend), main="Popularity Distance") 


final_df = extracted_features %>% mutate(cluster = factor(cutree(hc, k=3), labels=c('Somewhat Popular','Unpopular', 'More Popular')), 
                                         artist = c(meta_df[['artist']]),
                                         album_year = c(meta_df[['release_year']]),
                                         tempo = c(meta_df[['corrected_tempo']]),
                                         track.track_number = c(meta_df[['track.track_number']]),
                                          album_type = c(meta_df[['track.album.album_type']]))
                                         
green_popularity_palette = c("Unpopular" = "#c8e6c9",
                             "Somewhat Popular" = "#81c784",
                             "More Popular" = "#1b5e20")

ggplot(data=final_df, aes(x=album_year, y=track.popularity, color=cluster)) + 
  geom_point() + geom_text(label=final_df[['artist']], nudge_y=1) + 
  scale_x_continuous(breaks=c(1960, 1970,1980,1990, 2000, 2010, 2020))+ 
  scale_color_manual(values = green_popularity_palette) 

ggplot(data=final_df, aes(x=album_year, y=tempo, color=cluster)) + 
  geom_point() + geom_text(label=final_df[['artist']], nudge_y=1) + 
  scale_color_manual(values = green_popularity_palette) 

ggplot(data=final_df, aes(x=factor(album_type), y=track.popularity, color=cluster)) + 
  geom_point() + geom_text(label=final_df[['artist']], nudge_y=1) + 
  scale_color_manual(values = green_popularity_palette) 

library(plotly)
fig <- final_df %>%
  plot_ly(
    x = ~album_type,
    y = ~track.popularity,
    split = ~album_type,
    type = 'violin',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) 
fig <- fig %>%
  layout(
    xaxis = list(
      title = "Album Type"
    ),
    yaxis = list(
      title = "Popularity (higher is better)"
    )
  )

fig
