library(readr)
library(dplyr)
library(data.table)

##########################################################################################################################################################

data <- read_tsv("https://datasets.imdbws.com/title.basics.tsv.gz")
data_episodes <- read_tsv("https://datasets.imdbws.com/title.episode.tsv.gz")
data_ratings <- read_tsv("https://datasets.imdbws.com/title.ratings.tsv.gz")

##########################################################################################################################################################

df_series <- data_ratings %>% 
  left_join(data, by = "tconst") %>% 
  filter(titleType == "tvEpisode") %>% 
  left_join(data_episodes, by = "tconst") %>% 
  select(tconst, parentTconst, seasonNumber, episodeNumber, averageRating, numVotes, primaryTitle, runtimeMinutes, startYear)

colnames(df_series) <- c("tconst","parentTconst", "seasonNumber", "episodeNumber",
                         "averageRating", "numVotes", "episodeName", "runtimeMinutes", "releaseYear")

data_subset <- data %>% 
  filter(titleType == "tvSeries") %>% 
  select(tconst, primaryTitle, startYear, endYear, genres)

df_series <- df_series %>% 
  left_join(data_subset, by = c("parentTconst" = "tconst")) %>% 
  filter(seasonNumber != "\\N", episodeNumber != "\\N") %>% 
  select(tconst, parentTconst, primaryTitle, episodeName, seasonNumber, episodeNumber,
         averageRating, numVotes, runtimeMinutes, releaseYear, startYear, endYear, genres) 

df_series[df_series == "\\N"] <- NA

df_series$seasonNumber <- as.numeric(df_series$seasonNumber)
df_series$episodeNumber <- as.numeric(df_series$episodeNumber)
df_series$runtimeMinutes <- as.numeric(df_series$runtimeMinutes)

df_series <- df_series %>% 
  group_by(parentTconst) %>% 
  mutate(seriesAvgRating = round(mean(averageRating), 2),
         seriesNumVotes = sum(numVotes), .after = primaryTitle) %>% 
  mutate(lengthYears = ifelse(is.na(startYear) | is.na(endYear), NA, as.numeric(endYear) - as.numeric(startYear)),
         .after = endYear) %>% 
  filter(!genres %in% c("Game-Show,Music", "Comedy,News,Talk-Show", "Adventure,Game-Show,Reality-TV",
                        "Comedy,Reality-TV", "Game-Show,Reality-TV", "Comedy,Sport,Talk-Show", NA))
  
setDT(df_series)

df_series <- df_series[, episodeNumberTotal := sapply(1:.N, \(x) sum(seasonNumber < seasonNumber[x]) + as.numeric(episodeNumber[x])), by = parentTconst]

df_series <- as.data.frame(df_series)

df_series <- df_series %>% 
  group_by(parentTconst) %>% 
  mutate(avgVotes = round(mean(numVotes), 0),
         numSeasons = max(seasonNumber),
         numEpisodes = max(episodeNumberTotal),
         avgEpPerSeason = round(numEpisodes / numSeasons, 1),
         avgRuntime = round(mean(runtimeMinutes), 1),
         .after = seriesNumVotes) %>% 
  mutate(totalEpisodeNumber = episodeNumberTotal, .keep = "unused", .after = episodeNumber) %>% 
  mutate(seriesStartDecade = case_when(startYear %in% 1950:1959 ~ "1950s",
                                       startYear %in% 1960:1969 ~ "1960s",
                                       startYear %in% 1970:1979 ~ "1970s",
                                       startYear %in% 1980:1989 ~ "1980s",
                                       startYear %in% 1990:1999 ~ "1990s",
                                       startYear %in% 2000:2009 ~ "2000s",
                                       startYear %in% 2010:2019 ~ "2010s",
                                       startYear %in% 2020:2023 ~ "2020s"),
        .after = endYear) %>% 
  group_by(parentTconst, seasonNumber, episodeNumber) %>% 
  arrange(episodeNumber, .by_group = T)

df_series_top500 <- df_series %>% 
  ungroup %>% 
  arrange(desc(avgVotes)) %>% 
  filter(seriesNumVotes >= 10000 & !(primaryTitle %in% c("Dragon Ball Z", "Dragon Ball Z Kai")) & !is.na(genres) &
         avgEpPerSeason <= 50 & seriesAvgRating >= 3) %>% 
  select(parentTconst, primaryTitle, seriesAvgRating, seriesNumVotes, avgVotes, avgEpPerSeason, genres) %>% 
  distinct(parentTconst, .keep_all = T) %>% 
  head(600)

df_series_top1000 <- df_series %>% 
  ungroup %>% 
  arrange(desc(avgVotes)) %>% 
  select(parentTconst, primaryTitle, seriesAvgRating, seriesNumVotes, avgVotes, numSeasons, avgRuntime, startYear, seriesStartDecade,
         lengthYears, genres) %>% 
  distinct(parentTconst, .keep_all = T) %>% 
  filter(!is.na(avgRuntime) & !is.na(lengthYears) & numSeasons >= 2 &
           lengthYears >= 1 & primaryTitle != "Caesar's Hour" & seriesAvgRating >= 5) %>% 
  head(1200)

df_series <- df_series %>% 
  ungroup %>% 
  arrange(desc(avgVotes)) %>% 
  filter(parentTconst %in% (df_series_top500 %>% 
                              select(parentTconst) %>% 
                              pull)) %>% 
  arrange(primaryTitle)

df_series_top500 <- df_series_top500 %>% 
  mutate(baseGenre = case_when(genres %in% c("Comedy,Family", "Comedy,Family,Fantasy", "Comedy", "Comedy,Drama", "Comedy,Drama,War",
                                             "Comedy,Music", "Comedy,Drama,Family", "Comedy,Sci-Fi", "Comedy,Crime,Fantasy", 
                                             "Comedy,Family,Romance", "Comedy,Romance", "Comedy,Family,Sci-Fi", "Comedy,Drama,Romance",
                                             "Comedy,Crime", "Comedy,Drama,Mystery", "Comedy,Crime,Mystery", "Comedy,Crime,Drama",
                                             "Comedy,Drama,Sport", "Comedy,Drama,Music", "Comedy,Fantasy", "Comedy,Drama,Fantasy",
                                             "Comedy,Fantasy,Horror") ~ "Comedy",
                               genres %in% c("Crime,Drama,Mystery", "Crime,Drama", "Crime,Drama,Thriller", "Crime,Drama,History",
                                             "Crime,Drama,Fantasy", "Crime,Drama,Horror", "Crime,Mystery,Thriller",
                                             "Crime,Drama,Romance") ~ "Crime",
                               genres %in% c("Drama,Fantasy,Horror", "Drama,Family,Romance", "Drama,Romance", "Drama,Fantasy,Mystery",
                                             "Drama", "Drama,Romance,Sport", "Drama,Mystery", "Drama,Mystery,Sci-Fi", "Drama,Sport",
                                             "Drama,Fantasy,Sci-Fi", "Drama,Family", "Drama,Mystery,Romance",
                                             "Drama,Fantasy,Romance", "Drama,Horror,Romance", "Drama,History,Romance", 
                                             "Drama,Sci-Fi") ~ "Drama",
                               genres %in% c("Action,Adventure,Sci-Fi", "Action,Adventure,Crime", "Action,Crime,Drama", 
                                             "Action,Adventure,Animation", "Action,Adventure,Drama", "Action,Adventure,Family",
                                             "Action,Animation,Drama", "Action,Drama,Fantasy", "Action,Animation,Comedy", 
                                             "Action,Sport", "Action,Drama,Mystery", "Action,Drama,Romance", "Action,Comedy,Drama",
                                             "Action,Comedy,Crime", "Action,Adventure,Biography", "Action,Drama,Sci-Fi",
                                             "Action,Drama", "Action,Drama,History", "Action,Comedy,Horror", "Action,Drama,War", 
                                             "Action,Adventure,Fantasy", "Action,Adventure,Comedy") ~ "Action",
                               genres %in% c("Adventure,Drama,Family", "Adventure,Comedy,Drama", "Adventure,Animation,Comedy",
                                             "Adventure,Drama,Sci-Fi", "Adventure,Drama,Fantasy", "Adventure,Comedy,Sport",
                                             "Adventure,Fantasy,Romance", "Adventure,Drama,Mystery", "Adventure,Animation,Drama",
                                             "Adventure,Comedy,Crime") ~ "Adventure",
                               genres %in% c("Biography,Documentary,History", "Biography,Documentary,Music", "Biography,Crime,Drama",
                                             "Biography,Drama,History", "Documentary,Sport") ~ "Biography",
                               genres %in% c("Animation,Comedy", "Animation,Comedy,Drama", "Animation,Comedy,Family",
                                             "Animation,Crime,Drama", "Animation,Comedy,Romance", "Animation,Drama,History") ~ "Animation",
                               genres %in% c("Drama,Sci-Fi,Thriller", "Drama,Mystery,Thriller", "Drama,Thriller") ~ "Thriller",
                               genres %in% c("Fantasy,Horror,Mystery", "Horror", "Drama,Horror,Mystery", "Drama,Horror,Thriller",
                                             "Drama,Horror,Sci-Fi") ~ "Horror",
                               genres %in% c("Western", "Drama,Western") ~ "Western",
                               T ~ NA)) %>% 
  filter(!is.na(baseGenre)) %>% 
  arrange(desc(avgVotes)) %>% 
  head(500)

df_series_top1000 <- df_series_top1000 %>% 
  mutate(baseGenre = case_when(genres %in% c("Comedy,Family", "Comedy,Family,Fantasy", "Comedy", "Comedy,Drama", "Comedy,Drama,War",
                                             "Comedy,Music", "Comedy,Drama,Family", "Comedy,Sci-Fi", "Comedy,Crime,Fantasy", 
                                             "Comedy,Family,Romance", "Comedy,Romance", "Comedy,Family,Sci-Fi", "Comedy,Drama,Romance",
                                             "Comedy,Crime", "Comedy,Drama,Mystery", "Comedy,Crime,Mystery", "Comedy,Crime,Drama",
                                             "Comedy,Drama,Sport", "Comedy,Drama,Music", "Comedy,Fantasy", "Comedy,Drama,Fantasy",
                                             "Comedy,Fantasy,Horror") ~ "Comedy",
                               genres %in% c("Crime,Drama,Mystery", "Crime,Drama", "Crime,Drama,Thriller", "Crime,Drama,History",
                                             "Crime,Drama,Fantasy", "Crime,Drama,Horror", "Crime,Mystery,Thriller",
                                             "Crime,Drama,Romance") ~ "Crime",
                               genres %in% c("Drama,Fantasy,Horror", "Drama,Family,Romance", "Drama,Romance", "Drama,Fantasy,Mystery",
                                             "Drama", "Drama,Romance,Sport", "Drama,Mystery", "Drama,Mystery,Sci-Fi", "Drama,Sport",
                                             "Drama,Fantasy,Sci-Fi", "Drama,Family", "Drama,Mystery,Romance",
                                             "Drama,Fantasy,Romance", "Drama,Horror,Romance", "Drama,History,Romance", 
                                             "Drama,Sci-Fi") ~ "Drama",
                               genres %in% c("Action,Adventure,Sci-Fi", "Action,Adventure,Crime", "Action,Crime,Drama", 
                                             "Action,Adventure,Animation", "Action,Adventure,Drama", "Action,Adventure,Family",
                                             "Action,Animation,Drama", "Action,Drama,Fantasy", "Action,Animation,Comedy", 
                                             "Action,Sport", "Action,Drama,Mystery", "Action,Drama,Romance", "Action,Comedy,Drama",
                                             "Action,Comedy,Crime", "Action,Adventure,Biography", "Action,Drama,Sci-Fi",
                                             "Action,Drama", "Action,Drama,History", "Action,Comedy,Horror", "Action,Drama,War", 
                                             "Action,Adventure,Fantasy", "Action,Adventure,Comedy") ~ "Action",
                               genres %in% c("Adventure,Drama,Family", "Adventure,Comedy,Drama", "Adventure,Animation,Comedy",
                                             "Adventure,Drama,Sci-Fi", "Adventure,Drama,Fantasy", "Adventure,Comedy,Sport",
                                             "Adventure,Fantasy,Romance", "Adventure,Drama,Mystery", "Adventure,Animation,Drama",
                                             "Adventure,Comedy,Crime") ~ "Adventure",
                               genres %in% c("Biography,Documentary,History", "Biography,Documentary,Music", "Biography,Crime,Drama",
                                             "Biography,Drama,History", "Documentary,Sport") ~ "Biography",
                               genres %in% c("Animation,Comedy", "Animation,Comedy,Drama", "Animation,Comedy,Family",
                                             "Animation,Crime,Drama", "Animation,Comedy,Romance", "Animation,Drama,History") ~ "Animation",
                               genres %in% c("Drama,Sci-Fi,Thriller", "Drama,Mystery,Thriller", "Drama,Thriller") ~ "Thriller",
                               genres %in% c("Fantasy,Horror,Mystery", "Horror", "Drama,Horror,Mystery", "Drama,Horror,Thriller",
                                             "Drama,Horror,Sci-Fi") ~ "Horror",
                               genres %in% c("Western", "Drama,Western") ~ "Western",
                               T ~ NA)) %>% 
  filter(!is.na(baseGenre)) %>% 
  arrange(desc(avgVotes)) %>% 
  head(1000)


colnames(df_series_top500) <- c("seriesId", "seriesTitle", "seriesAvgRating", "totalVotes", "avgVotes", "avgEpPerSeason", "genres", 
                                "baseGenre")

colnames(df_series_top1000) <- c("seriesId", "seriesTitle", "seriesAvgRating", "totalVotes", "avgVotes", "numSeasons", "avgRuntime", 
                                 "seriesStartYear", "seriesStartDecade", "lengthYears", "genres", "baseGenre")

colnames(df_series) <- c("episodeId", "seriesId", "seriesTitle", "seriesAvgRating", "totalVotes", "avgVotes", "numSeasons", "numEpisodes", "avgEpPerSeason",
                         "avgRuntime","episodeName", "seasonNumber", "episodeNumber","totalEpisodeNumber", "avgRating", "votes", "runtime", 
                         "releaseYear", "seriesStartYear", "seriesEndYear", "seriesStartDecade", "lengthYears", "genres")

top100 <- df_series %>% 
  filter(!is.na(numSeasons) &  !is.na(numEpisodes) &
          !is.na(seasonNumber) & !is.na(episodeNumber) &
          !is.na(avgRating) & !is.na(votes) & !is.na(genres) &
          numSeasons %in% 5:20 & numEpisodes >= 50) %>% 
  arrange(desc(avgVotes)) %>% 
  distinct(seriesId) %>% 
  head(100) %>% 
  pull

df_series_top100 <- df_series %>% 
  select(seriesId, seriesTitle, episodeName, seasonNumber, episodeNumber, totalEpisodeNumber, avgRating, votes, releaseYear, 
         seriesAvgRating, totalVotes, avgVotes, numSeasons, numEpisodes, seriesStartYear, seriesEndYear, genres) %>% 
  filter(seriesId %in% top100) %>% 
  arrange(seriesTitle)

write.csv(df_series_top100, "C:/Users/48784/Documents/Studia/Repo/projects/tv-series-popularity/series_dataframe_top100.csv")

write.csv(df_series_top500, "C:/Users/48784/Documents/Studia/Repo/projects/tv-series-popularity/series_dataframe_top500.csv")

write.csv(df_series_top1000, "C:/Users/48784/Documents/Studia/Repo/projects/tv-series-popularity/series_dataframe_top1000.csv")
  
###########################################################################################################################################################
