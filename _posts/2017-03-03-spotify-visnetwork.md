<br> <br>

Introduction
------------

Last week I saw a post on [r-bloggers](https://www.r-bloggers.com/), where someone [used Spotify's API to find out which of Radiohead's songs is the saddest](http://blog.revolutionanalytics.com/2017/02/finding-radioheads-most-depressing-song-with-r.html). This really made me want to try it out for myself, but I wanted to do something more personal, so I decided to use the songs in my library instead. Recently I had to deal with some graph traversing problems at work, which gave me the idea to plot a network graph of my collection of songs on Spotify to see how and how much my artists are related to each other.

<!--more-->

<br>



#### Click [here](https://m-ferencz.github.io/spotify-visnetwork/) for the interactive plot!

<br>

Data prep
---------

The first obstacle I encountered is that there seems to be no way to access a user's library, i.e. the songs they saved to their device, through the API (even if you are logged in as said user). However, I found out that you can copy and paste your list of saved songs directly from the application, and copy it into an Excel spreadsheet. This resulted in 3221 Spotify href's, which was actually quite fortunate, as this bypassed having to search for them using the song/artist strings.

The initial format of the href is to the effect of <spotify:track:1z2fWjiH2CFnPU83I33QH2>, of which I only needed the 22-digit code at the end, so I removed the first 14 characters from each string.

``` r
# importing data

mysongs <- read.csv("mysongs.csv", header=F)
mysongs <- apply(mysongs, 1, substr, start=15, stop=36)
songs.df <- data.frame(ID=mysongs)
```

<br> <br>

Getting the artists for each song
---------------------------------

<br>

Now onto finding out the artist of each song. I went and set up a [Spotify developer app](https://developer.spotify.com/), and authorised it. The issue I ran into instantly was Spotify's [rate limiting](https://developer.spotify.com/web-api/user-guide/#rate-limiting), when it comes to requests. To get around this, I used a quick & dirty *for loop*, with which I was able to cycle through all rows with ease - I just re-declared the integer at which the loop started at the end of each iteration, so I just had to run the same loop over and over (I hit the limit after every ~100 **GET**).

``` r
library(tidyverse)
library(stringr)
library(httr)

# getting my access token

access_token <- POST('https://accounts.spotify.com/api/token',
                     accept_json(), authenticate(client_id, client_secret),
                     body = list(grant_type='client_credentials'),
                     encode = 'form', config(http_version=2)) %>% content %>% .$access_token


# function to pull back the song's artist

getSongArtist <- function(x){
 GET(paste('https://api.spotify.com/v1/tracks/', x, "/", sep="")) %>% 
    content %>%
    .$artists %>%
    .[[1]]
}


# quick and dirty loop to assign artist and artist ID to each song

j <- 1
for(i in j:nrow(songs.df)){
  temp.info <- getSongArtist(songs.df$ID[i])
  songs.df$Artist[i] <- temp.info$name
  songs.df$Artist.ID[i] <- temp.info$id
  
  j <- i
}
```

<br>

I created a data frame for my artists, and included the number of times they appeared in my song list. This gave me 340 rows, which I thought was quite large, and it might clutter the plot in the end. I decided not to care about artists by whom I only downloaded one or two songs, and set a lower limit of 10 songs per artist - this ensured that I have downloaded at least an album's worth of songs, and left me with a much more digestable 79 artists.

``` r
# setting up data frame for artists and their frequencies

artists.df <- songs.df[,2:3]
artists.df <- table(artists.df$Artist, artists.df$Artist.ID) %>%
  as.data.frame() %>%
  subset(Freq>10)

names(artists.df) <- c("Artist", "ID", "Count")
```

<br> Using the same quick & dirty loop, I retrieved each artist's subgenre (the first one listed), and the URL to their picture on Spotify.

``` r
# function to pull back artist info

getArtistInfo <- function(x){
  GET(paste('https://api.spotify.com/v1/artists/', x, "/", sep="")) %>% 
    content
}


# quick and dirty loop to retrieve subgenre and artist image URL

j <- 1
for(i in j:nrow(artists.df)){
  temp.info <- getArtistInfo(artists.df$ID[i])
  if(length(temp.info$genres)>0){
    artists.df$subgenre[i] <- temp.info$genres[[1]]
  }
  if(length(temp.info$images)>0){
  artists.df$image[i] <- temp.info$images[[1]]$url
  }
  
  j <- i
}
```

<br> Spotify's genre categorisation is pretty specific (*e.g. "australian alternative rock"*), and because I was planning to do some grouping based on genre, I decided to put them in some more generic groups. I picked five genres, rock, pop, hip hop, electronic and jazz/blues/funk, and assigned a genre to each subgenre.

``` r
# setting up lookup table for genres and subgenres

unique.subgenres <- data.frame(subgenre = unique(artists.df$subgenre), genre = c("electronic",
                                                                                 rep("hip hop", 12),
                                                                                 rep("jazz/blues/funk", 3),
                                                                                 rep("pop", 6),
                                                                                 rep("rock", 7)))

# merging genres to artist data frame

artists.df <- merge(artists.df, unique.subgenres, by = "subgenre", all.x = T)
```

<br> <br>

Getting related artists
-----------------------

<br> Next, I pulled back all available related artists for my list of artists, to serve as the edges to my network plot. I initialised all 20 columns for each related artist (maximum is 20), because I found that naming them this way was easier. I then used my trusty old loop to pull back the info. I used another loop inside, in case I have artists which have fewer than 20 related artists available.

``` r
# initialising related artists columns

artists.df$Related1 <- ""
artists.df$Related2 <- ""
artists.df$Related3 <- ""
artists.df$Related4 <- ""
artists.df$Related5 <- ""
artists.df$Related6 <- ""
artists.df$Related7 <- ""
artists.df$Related8 <- ""
artists.df$Related9 <- ""
artists.df$Related10 <- ""
artists.df$Related11 <- ""
artists.df$Related12 <- ""
artists.df$Related13 <- ""
artists.df$Related14 <- ""
artists.df$Related15 <- ""
artists.df$Related16 <- ""
artists.df$Related17 <- ""
artists.df$Related18 <- ""
artists.df$Related19 <- ""
artists.df$Related20 <- ""


# quick and dirty loop to pull back all related artists

j <- 1
for(i in j:nrow(artists.df)){
  temp.info <- getRelatedArtists(artists.df$ID[i])
  
  for(k in 1:20){
    if(length(temp.info$artists)>=k){
      artists.df[i, 5+k] <- temp.info$artists[[k]]$name
    }
  }
  j <- i
}
```

<br> <br>

Setting up graph edges
----------------------

<br>

For my edge-list, I just needed two columns; a *from* and a *to* column (even though this is not techically applicable here, as this graph will not be directed). I rearranged my 21 columns into these two, and removed all rows where either of the artists were not in my list of 79. Finally I removed reverse duplicates, i.e. connections between the same artists in either direction.

``` r
# combine all relations to two columns

edges.df <- data.frame(artist1=character(), artist2=character())
for(i in 1:20){
  edges.df <- rbind(edges.df, data.frame(artist1=artists.df$Artist, artist2=artists.df[,5+i]))
}

# keep only the edges where both artists apear in my list

edges.df <- edges.df[(edges.df$artist2 %in% artists.df$Artist & edges.df$artist1 %in% artists.df$Artist),]


# remove reverse duplicates

edges.df <- edges.df[!duplicated(apply(edges.df, 1, function(x) paste(sort(x), collapse=''))),]
```

<br> <br>

Prepping the plot
-----------------

<br>

I mentioned before that I wanted to do some sort of grouping based on genre - I decided to colour the nodes based on it, so I used the same method I used on the subgenres to assign a different colour to each genre, then merged it back into the artist data frame.

``` r
# assign a colour to each genre

unique.genres2 <- data.frame(genre = unique(artists.df$genre), color = c("green",
                                                                         "blue",
                                                                         "brown",
                                                                         "orange",
                                                                         "red"))

# merge back to artists.df

artists.df<- merge(artists.df, unique.genres, by = "genre", all.x = T)
```

<br> [visNetwork](http://datastorm-open.github.io/visNetwork/)'s calibration is pretty straight-forward; I just had to rename some columns in my data frames and I was pretty much ready to go. I set the shape of the nodes to *circularImage*, which enables the use of images on nodes, set the size of the nodes to be derived from the count of the artist and set some titles and labels.

``` r
# renaming columns in edges data frame to comply with visNetwork

edges.df$from <- edges.df$artist1
edges.df$to <- edges.df$artist2


# renaming columns in artists.df

artists.df$id <- artists.df$Artist
artists.df$shape <- "circularImage"
artists.df$value <- artists.df$size <- 10*artists.df$Count
artists.df$label <- artists.df$Artist
artists.df$title <- paste(artists.df$Artist, "-", artists.df$subgenre, "-", artists.df$Count, "songs")
artists.df$physics <- TRUE
```

<br> I added a drop-down menu to filter by genre, and disabled node dragging - that's it!

``` r
library(visNetwork)

# building plot

visgraph <- visNetwork(artists.df, edges.df, height = 600, width = 800, main = "Marcell's Spotify Library") %>%
  visInteraction(dragNodes = FALSE) %>%
  visOptions(selectedBy = "genre")

visgraph
```

<br> <br> <br>

Thank you
---------

Thank you for reading this far! Feedback and tips to improve are always welcome.
