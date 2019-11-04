library(stringr)
#install.packages(c("readxl","writexl")) 
library(readxl)
library(writexl)
#install.packages("readr")
library(readr)

movieData <- readxl::read_xlsx("CornellMovieCorpus/Cornell All Movies.xlsx",col_names = FALSE)[,2:5]
titleData = readxl::read_xlsx("CornellMovieCorpus/movie_titles_metadata.xlsx",col_names = FALSE)[,1:3]
colnames(movieData) = c('CharacterID','Movie','Character','Dialogue')
colnames(titleData)=c('Movie','Title','Year')
movieData$Character = str_to_title(movieData$Character)
titleData$Title = paste(titleData$Title,titleData$Year)
titleData = titleData[,1:2]
fullDataset = merge(x=movieData,y=titleData,by="Movie",all.x=TRUE)

GenerateWordCounts = function(dataframe_in){
  length_of_df = nrow(dataframe_in)
  word_counts = rep(NA,length_of_df)
  for(i in 1:length_of_df){
    numWords = length(unlist(strsplit(dataframe_in$Dialogue[i]," ")))
    word_counts[i] = numWords
  }
  return(word_counts)
}

fullDataset$'WordCount' = GenerateWordCounts(fullDataset)

dataset_aggregated = fullDataset[,c('Character','Title')]
dataset_aggregated$Title = str_to_title(dataset_aggregated$Title)

dataset_aggregated$WordCount = ave(fullDataset$WordCount,dataset_aggregated,FUN=sum)


dataset_aggregated = na.omit(dataset_aggregated[!duplicated(dataset_aggregated),])
write_xlsx(dataset_aggregated, "CornellMovieCorpus/CornellCorpusMovieData.xlsx")
rm(fullDataset,movieData,titleData)

title.basics = read_tsv(file = 'IMDB/title.basics.tsv',na = "\\N")
title.basics = title.basics[,c("tconst","primaryTitle","startYear","endYear")]
title.basics$endYear = ifelse(is.na(title.basics$endYear),title.basics$startYear,title.basics$endYear)
title.basics = title.basics[,c("tconst","primaryTitle","endYear")]
name.basics = read_tsv(file = 'IMDB/name.basics.tsv',na = "\\N")
name.basics = name.basics[,c("nconst","primaryName")]
title.principals = read_tsv(file = 'IMDB/title.principals.tsv',na = "\\N")
title.principals = title.principals[,c("tconst","nconst","characters")]
IMDB_FULL = merge(x=title.principals, y=name.basics, by = "nconst",all.x=TRUE)
rm(title.principals,name.basics)
IMDB_FULL = merge(x = IMDB_FULL, y = title.basics, by = "tconst", all.x = TRUE)
rm(title.basics)

strip = function(string){
  return(ifelse(is.na(string)),NA,substr(string,3,(nchar(string)-2)))
}

IMDB_FULL$characters = ifelse(is.na(IMDB_FULL$characters),NA,substr(IMDB_FULL$characters,3,(nchar(IMDB_FULL$characters)-2)))

CornellMovieData = read_excel("CornellMovieCorpus/CornellCorpusMovieData.xlsx")
IMDB_FULL = read_csv("IMDB/IMDB_FULL.csv")
colnames(IMDB_FULL)=c("index","tconst","nconst","Character","Actor","Title","Year")
IMDB_FULL$Title = paste(IMDB_FULL$Title,as.character(IMDB_FULL$Year))
IMDB_FULL = IMDB_FULL[,c("Character","Actor","Title")]
CornellMovieMerged = merge(x=CornellMovieData, y=IMDB_FULL, by = c("Character","Title"),all.x=TRUE)
#test = na.omit(CornellMovieMerged)
#write.csv(IMDB_FULL, file = "IMDB_FULL.csv")
write_xlsx(CornellMovieMerged,"CornellMovieCorpus/CornellCorpusMovieData.xlsx")
