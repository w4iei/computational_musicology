library(tidyverse)
library(remotes)
library(spotifyr)
library(ggplot2)
library(grid)
library(gridExtra)
spotifyr::get_spotify_access_token()


bach_prelude_playlist = "4yNYY3xmNhPTDrfFc0qG9b"
bach_goldberg_aria_playlist = "0wwqrAl2LgAKGw0zuJRuwe"
music_for_frogs_playlist = "6G6qnBVUORSeTf4kA7vC22"
music_for_cats_playlist = "7gfsHECGdEKU7dFQEQCh1p"
music_for_rabbits_playlist = "2JKtxntKKWXm5K9F1jEfC4"
music_for_dogs_playlist = "48DvsLs8VirGB4BnCYWl2x"
reggae_for_dogs_playlist = "4CHnPKSVHnze8p1FaVOZeB"

bach_prelude_playlist_features = get_playlist_audio_features("", bach_prelude_playlist)
bach_prelude_playlist_features = bach_prelude_playlist_features %>% mutate(track.duration_min = track.duration_ms/1000/60)
wrapped_tempo_label = "Unmodified"

bach_prelude_playlist_features = fix_tempo_wrap(bach_prelude_playlist_features)
wrapped_tempo_label = "Wrapped"

summarize_bach = bach_prelude_playlist_features %>% summarize(Median_Tempo_bpm=median(tempo), Median_Duration_min=median(track.duration_min))
# Inject additional features:
# playlist_features = playlist_features %>% mutate("Instrument"="Piano")
special_bach_track_notes = c('', 'Gould', '', '', '', 'Harp', '', '',
                             'Axel Gillison', 'Harpsichord', '', '', '', '', '',
                             'Lang Lang', 'Schiff w/ Fugue', 'Jazz Arrangement',
                             '', '', '', '', '', '', '', '', '', '', '',
                             '', '')

special_bach_track_notes_key = c('', '', '', '', '', 
                                 'Harp', '', '', 'Axel Gillison', 'Harpsichord',
                                 '', '', '', '', '',
                                 '', '', '', 'Schiff w/ Fugue', 'Jazz Arrangement',
                              '', '', '', '', '', '', '', '', '',
                             '', '')

# Best first plot, as a sanity check
# Duration Histogram
hist1 = ggplot(bach_prelude_playlist_features, aes(
  x=track.duration_min, 
)) + geom_histogram(bins=12) + xlab("Track Duration (min)") + scale_y_continuous(breaks=c(2,4,6,8))

# Best first plot, as a sanity check
# Tempo Histogram
hist2 = ggplot(bach_prelude_playlist_features, aes(
  x=tempo, 
)) + geom_histogram(bins=12) + xlab("Tempo (bpm)") + scale_y_continuous(breaks=c(2,4,6,8))

grid_plots = grid.arrange(hist1, hist2, ncol = 2, top=textGrob(paste(wrapped_tempo_label, " Histograms", sep=''),))

ggsave(
  paste(wrapped_tempo_label, " Histograms.png", sep=''),
  plot = grid_plots,
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


# Tempo vs Duration
tempo_v_duration = ggplot(bach_prelude_playlist_features, aes(
  x=track.duration_min, y=tempo,
)) + geom_point() + xlab("Track Duration (min)") + ylab("Tempo (bpm)") +
  geom_text(
    label=special_bach_track_notes_key,
    nudge_x=.15, nudge_y=1.7
  ) + ggtitle(paste(wrapped_tempo_label, " Tempo vs Duration", sep=''))

ggsave(
  paste(wrapped_tempo_label, " Tempo vs Duration.png", sep=''),
  plot = tempo_v_duration,
  dpi = 300,
  limitsize = TRUE,
  bg = NULL,
)


# Axel Gillison corrected... doesn't work. I don't get r
fix_axel = function(bach_playlist_features) {
  minor_third_ratio = 19/16
  minor_third_ratio_invert = 16/19
  bach_prelude_playlist_features = bach_prelude_playlist_features %>% mutate(track.duration_ms = case_when(key_mode=="A major" ~ track.duration_ms*minor_third_ratio_invert, TRUE ~ track.duration_ms))
  bach_prelude_playlist_features = bach_prelude_playlist_features %>% mutate(tempo = case_when(key_mode=="A major" ~ tempo.*minor_third_ratio, TRUE ~ tempo))
  bach_prelude_playlist_features = bach_prelude_playlist_features %>% mutate(key_mode = case_when(key_mode=="A major" ~ "C Major Corrected", TRUE ~ tempo))
}

fix_tempo_wrap = function(bach_playlist_features) {
  tempo_threshold = 95;
  time_threshold = 1.5;
  bach_prelude_playlist_features = bach_playlist_features %>% 
    mutate(tempo = case_when(tempo>tempo_threshold & track.duration_min > time_threshold ~ tempo * 0.5, TRUE ~ tempo))
}


bach_prelude_playlist_features = fix_axel(bach_prelude_playlist_features)

# Tempo vs Key
key_plot = ggplot(bach_prelude_playlist_features, aes(
  x=tempo, y=key_mode
)) + geom_point() +
  geom_text(
    label=special_bach_track_notes_key,
    nudge_y=0.25,
  ) + xlab("Tempo (bpm)") + ylab("Key")
ggsave("Key vs Tempo.png",
  plot = key_plot, dpi = 250, limitsize = TRUE, width=10, height=3)

# Tempo Histogram
ggplot(bach_prelude_playlist_features, aes(
  x=tempo,
)) + geom_histogram()

# Debug for Tempo:
ggplot(bach_prelude_playlist_features, aes(
  x=tempo, y=key_mode
)) + geom_point()

