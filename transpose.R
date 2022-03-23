library(tidyverse)
library(spotifyr)
library(ggplot2)
library(compmus)
library(grid)
library(gridExtra)
spotifyr::get_spotify_access_token()

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

luc = 
  get_tidy_audio_analysis("13zyfbzt2PiYo2RjjJMCsb") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

transpose_pitch = function(pitch_list, number_of_semitones) { # n = 1: C -> C#
  names = names(pitch_list)
  new_list = setNames(circshift(unname(pitch_list), number_of_semitones), names)
  new_list
}

transpose_pitches = function(df, n) {  # n = 1 is C -> C#
  df %>% dplyr::mutate(pitches = purrr::map2(pitches, n, transpose_pitch))  # doesn't work correctly
}

luc_t = transpose_pitches(luc, 1)

luc_chroma = luc %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c() + ggtitle("Luc Beausejour") + theme(legend.position = "none")

luc_t_chroma = luc_t %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c() + ggtitle("Luc Beausejour Transposed 1 Semitone") + theme(legend.position = "none")

grid.arrange(luc_chroma, luc_t_chroma, ncol = 2,
             nrow = 1, top=textGrob('Chromagrams'))
