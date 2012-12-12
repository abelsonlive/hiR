# download every imdb id
rm(list=ls())
library("hiR")
x <- data(imdb_genres, hiR)
ldply(x, get_all_imdb_ids)
# good luck!
