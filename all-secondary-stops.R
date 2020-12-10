library(dplyr)
library(valleybikeData)

groups <- trips %>%
  group_by(user_id) %>%
  arrange(start_time) %>%
  mutate(trip_number = row_number()) %>%
  filter(is.na(start_station) | is.na(end_station)) %>%
  group_by(user_id, bike) %>%
  group_split() %>%
  as.list()

groups <- groups[sapply(groups, function(x) {
  multiple_trips <- nrow(x) > 1
  consecutive_trips <- any(diff(x$trip_number) == 1)
  return(multiple_trips & consecutive_trips)
})]

groups <- lapply(groups, function(x) {
  start_station_row <- !is.na(x$start_station)
  split(x, cumsum(start_station_row))
}) %>%
  unlist(recursive = FALSE)

# keep only consecutive trips
groups <- lapply(groups, function(x) {
  sequences <- cumsum(c(1, diff(x$trip_number) != 1))
  keep <- sequences %in% sequences[duplicated(sequences)]
  return(x[keep, ])
})

groups <- groups[sapply(groups, nrow) != 0]

groups <- lapply(groups, function(x) {
  keep <- logical(length = nrow(x))
  for (i in 1:nrow(x)) {
    keep[i] <- (i != nrow(x) && is.na(x$end_station[i]) && is.na(x$start_station[i + 1]) &&
                  geosphere::distVincentyEllipsoid(c(x$end_longitude[i], x$end_latitude[i]),
                                                   c(x$start_longitude[i + 1], x$start_latitude[i + 1])) <= 100 &&
                  between(difftime(x$start_time[i + 1], x$end_time[i], units = "hours"), 0, 5)) |
      (i != 1 && is.na(x$start_station[i]) && is.na(x$end_station[i - 1]) &&
         geosphere::distVincentyEllipsoid(c(x$end_longitude[i - 1], x$end_latitude[i - 1]),
                                          c(x$start_longitude[i], x$start_latitude[i])) <= 100 &&
         between(difftime(x$start_time[i], x$end_time[i - 1], units = "hours"), 0, 5))
  }
  return(x[keep, ])
})

groups <- groups[sapply(groups, nrow) != 0]

groups <- groups[sapply(groups, function(x) {
  !(all(is.na(x$start_station)) | all(is.na(x$end_station)))
})]

groups <- lapply(groups, function(x) {
  start_station_index <- which(!is.na(x$start_station))
  end_station_index <- which(!is.na(x$end_station))[1]
  return(x[start_station_index:end_station_index, ])
})

stops <- lapply(groups, function(x) {
  coords <- x[2:nrow(x), c("start_latitude", "start_longitude")]
  duration <- difftime(x$start_time[2:nrow(x)], x$end_time[1:(nrow(x) - 1)], units = "mins") %>%
    round(2)
  stops <- cbind(
    coords,
    duration
  ) %>%
    mutate(user_id = x$user_id[1], bike = x$bike[1]) %>%
    rename(latitude = start_latitude, longitude = start_longitude)
  return(stops)
}) %>%
  data.table::rbindlist() %>%
  filter(duration >= 1)

rm("groups")
