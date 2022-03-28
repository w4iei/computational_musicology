# Album Analysis

# Week 12 Lab
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyverse)
library(spotifyr)
library(compmus)


bach_prelude_playlist_features = get_playlist_audio_features("", "4yNYY3xmNhPTDrfFc0qG9b") %>%
  mutate(track.duration_min = track.duration_ms/1000/60)
# Unwrap Tempo
tempo_threshold = 95;
time_threshold = 1.5;
bach_prelude_playlist_features = bach_prelude_playlist_features %>% 
  mutate(corrected_tempo = case_when(tempo>tempo_threshold & track.duration_min > time_threshold ~ tempo * 0.5, TRUE ~ tempo))

album_tracks = bach_prelude_playlist_features %>% filter(track.album.album_type == "album")

track.album.id
album_tracks[["track.album.id"]][[1]]

get_best_artist = function(artists){
  
  if (length(artists[['name']]) > 1) {
    artists[['name']][[2]]
  }
  else {
    artists[['name']][[1]]
  }
}

# Adds all tracks from the associated album to the dataframe
add_nested_tracks_from_album = function(playlist_df) {
  get_adjacent_tracks = function(album_id){
    track_query_brief = get_album_tracks(album_id, 48)  
    # ^^ This doesn't return all the info we need, so use the IDs to query again
    # Requery, using IDs:
    album_df = paste(track_query_brief[['id']], collapse=",") %>% 
      get_tracks() %>% select(id, disc_number, track_number, popularity)
    album_df = album_df %>% mutate(track_audio_features = map(id, get_track_audio_features))
    # Almost every album is 2 CDs with 24 tracks each, so create an index
    # This doesn't work with Schiff.
    album_df %>% mutate(track_index = (disc_number-1)*24 + track_number) 
  }
  
  # This will take a while. Does an audio feature query per track. 
  album_track_info = playlist_df %>% 
    mutate(
      album_tracks = map(track.album.id, get_adjacent_tracks),
           album_artist_label = map(track.artists, get_best_artist))
}



bach_prelude_playlist_features = add_nested_tracks_from_album(bach_prelude_playlist_features) 
big_df_simple = bach_prelude_playlist_features %>% select(album_tracks, album_artist_label)

unnested = unnest(big_df_simple, album_tracks) %>% 
  mutate(popularity = as.numeric(popularity),
         album_artist_label = as.character(album_artist_label)
         )

artists_to_compare = c( 'Glenn Gould', 'Lang Lang', 
                        'András Schiff', 'Luc Beauséjour', 'Trevor Pinnock', 
                       'Marisa Robles', 'Wilhelm Kempff', 'Julia Cload')
unnested_f = unnested %>% filter(album_artist_label %in% artists_to_compare ) %>%
  mutate(artist_factor = as.factor(album_artist_label))

print(unique(unnested_f$album_artist_label))

stacked_pop = ggplot(unnested_f, aes(x=track_index, y=popularity, color=album_artist_label)) + geom_line()

#(unnested, aes(x=track_index, y=popularity)) + geom_line() + facet_wrap(~album_artist_label)

popularity_trace = ggplot(unnested_f, aes(x=track_index, y=popularity)) + geom_line() + facet_wrap(~artist_factor) 
ggplotly(popularity_trace) 
ggplotly(stacked_pop)
       