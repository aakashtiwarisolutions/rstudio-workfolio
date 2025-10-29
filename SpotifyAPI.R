library(httr)
library(jsonlite)
library(ggplot2)

client_id <- "4d98eefd8b1e4eada454fdf239bfa725"
client_secret <- "0c4758749a404aef934868e7ca4d0396"

auth_response <- POST("https://accounts.spotify.com/api/token",
                      accept_json(),
                      authenticate(client_id, client_secret),
                      body = list(grant_type = "client_credentials"),
                      encode = "form")

auth_token <- content(auth_response)$access_token

get_artist_id <- function(artist_name) {
  url <- "https://api.spotify.com/v1/search"
  res <- GET(url, query = list(q = artist_name, type = "artist"),
             add_headers(Authorization = paste("Bearer", auth_token)))
  artist_data <- content(res, as = "parsed", type = "application/json")
  
  if (length(artist_data$artists$items) == 0) {
    stop("Artist not found. Please check the artist name.")
  }
  
  return(artist_data$artists$items[[1]]$id)
}
get_top_tracks <- function(artist_id) {
  url <- paste0("https://api.spotify.com/v1/artists/", artist_id, "/top-tracks")
  res <- GET(url, query = list(country = "US"),
             add_headers(Authorization = paste("Bearer", auth_token)))
  tracks_data <- content(res, as = "parsed", type = "application/json")
  
  if (length(tracks_data$tracks) == 0) {
    stop("No top tracks found for this artist.")
  }
  
  return(tracks_data$tracks)
}

get_popularity_data <- function(tracks) {
  if (length(tracks) == 0) {
    stop("No tracks found. API might not have returned tracks.")
  }
  
  popularity_data <- data.frame(
    track_name = sapply(tracks, function(track) track$name),
    popularity = sapply(tracks, function(track) track$popularity)
  )
  
  return(popularity_data)
}

artist_name <- "Karan Aujla"
artist_id <- get_artist_id(artist_name)
top_tracks <- get_top_tracks(artist_id)


popularity_data <- get_popularity_data(top_tracks)


print(popularity_data)

if (nrow(popularity_data) > 0) {
  ggplot(popularity_data, aes(x = popularity)) +
    geom_density(fill = "blue", alpha = 0.5) +
    ggtitle(paste("Popularity Distribution of", artist_name, "Songs")) +
    xlab("Popularity Score") + ylab("Density")
} else {
  print("No popularity data available for plotting.")
}
# Aakash Tiwari
#10/27/2025