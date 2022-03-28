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
    
    afs = get_track_audio_features(paste(track_query_brief[['id']], collapse=','))
    #album_df = album_df %>% mutate(track_audio_features = map(id, get_track_audio_features))
    album_df = merge(album_df, afs, x.on="id", y.on="id")
    # Almost every album is 2 CDs with 24 tracks each, so create an index
    # This doesn't work with Schiff.
    album_df %>% mutate(track_index = (disc_number-1)*24 + track_number) 
  }
  playlist_df %>% mutate(album_tracks = map(track.album.id, get_adjacent_tracks))
}

# Down select artists
artists_to_compare = c( 'Glenn Gould', 'Keith Jarrett', 'Daniel Barenboim',
                        'Luc Beauséjour', 'Trevor Pinnock', 
                        'Wilhelm Kempff', 'Julia Cload')

playlist_with_artists = bach_prelude_playlist_features %>% 
  mutate(album_artist_label = as.character(map(track.artists, get_best_artist)))

filtered_playlist = playlist_with_artists %>% filter(album_artist_label %in% artists_to_compare )

filtered_nested = add_nested_tracks_from_album(filtered_playlist) 
big_df_simple = filtered_nested %>% select(album_tracks, album_artist_label)

unnested_f = unnest(big_df_simple, album_tracks) %>% 
  mutate(popularity = as.numeric(popularity), 
         artist_factor = as.factor(album_artist_label))

stacked_pop = ggplot(unnested_f, aes(x=track_index, y=popularity, color=album_artist_label)) +
  geom_line() + xlab('Track Index') + ylab('Popularity (0-100)') + labs(color="Artist")
ggplotly(stacked_pop)

popularity_trace = ggplot(unnested_f, aes(x=track_index, y=popularity)) + geom_line() + facet_wrap(~artist_factor) 
ggplotly(popularity_trace) 

loudness_trace = ggplot(unnested_f, aes(x=track_index, y=loudness)) + geom_line() + facet_wrap(~album_artist_label) 
ggplotly(loudness_trace)


stacked_loudness = ggplot(unnested_f, aes(x=track_index, y=loudness, color=album_artist_label)) +
  geom_line() + xlab('Track Index') + ylab('Loudness (dB)') + labs(color="Artist")
ggplotly(stacked_loudness)

stacked_energy = ggplot(unnested_f, aes(x=track_index, y=energy, color=album_artist_label)) +
  geom_line() + xlab('Track Index') + ylab('Energy') + labs(color="Artist")
ggplotly(stacked_energy)

stacked_key = ggplot(unnested_f, aes(x=track_index, y=key, color=album_artist_label)) +
  geom_line() + xlab('Track Index') + ylab('Key') + labs(color="Artist")
ggplotly(stacked_key)
