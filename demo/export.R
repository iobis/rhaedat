library(rhaedat)

df <- events()
write.csv(df, file = "output/events.csv", row.names = FALSE, na = "")

nogrid <- df %>% filter(is.na(gridCode)) %>% select(eventName, eventYear) %>% distinct()
write.csv(nogrid, file = "output/nogrid.csv", row.names = FALSE, na = "")
