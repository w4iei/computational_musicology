library(tidyverse)

library(remotes)
library(usethis)
# install.packages("package_name")

remotes::install_github('charlie86/spotifyr')

usethis::edit_r_environ()

library(spotifyr)

spotifyr::get_spotify_access_token()

playlist_features = get_playlist_audio_features("", "4CHnPKSVHnze8p1FaVOZeB")


juditha <- get_track_audio_features(c("2M5b9YLAgFroqWzeaZf86e", "3DBKc4ioGnMQLlbGQcFDIO"));
alla <- get_album_tracks("7oI0E3DdTbD85rhMg19GSU");
gilberto <- get_artist_audio_features("gilberto gil");
ecm <- get_playlist_audio_features("", "1zN4nK6oHMo2dei2WWPtSL");

disney_playlist = get_playlist_audio_features("", "37i9dQZF1DX8C9xQcOrE6T");
let_it_go <- get_track_audio_features(c("0qcr5FMsEO85NAQjrlDRKo"));

summary(playlist_features$tempo)

playlist_features %>%
  summarise(
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )
