Introduction
------------

This week I decided to play with everyone's favourite probability phenomenon, the Birthday Problem. But, instead of sampling random numbers 1-365 to demonstrate the 'problem', I thought it would be more engaging (for me) to query some real world data, so I ended up using the birthdays of football players registered for the past 5 FIFA World Cups. This way I managed to tie in some web scraping into this mini-project, which is what I really wanted to practise.

<!--more-->
<br>

Footnote: Turns out the BBC has already [done this](http://www.bbc.co.uk/news/magazine-27835311), which I hadn't realised until I was halfway through writing this, but I finished it regardless.

<br> <br>

Setting Up
----------

For those not in the know, the [birthday problem](https://en.wikipedia.org/wiki/Birthday_problem) is the phenomenon that to be pretty certain that two people in a group share the same birthday (just the month and day, not the year), you need a surprisingly low number of people. To be 100% sure, of course, you would need 366; but having 70 people in your set will give you a 99.9% probability that two people will celebrate on the same day. This may seem counter-intuitive, but once you calculate the probability of *no* two people sharing a birthday, the numbers start making more sense (I recommend reading through the Wikipedia explanation if you're unfamiliar with the birthday problem).

<br>

To get odds better than half-and-half (50.73%), you only need to have 23 people in your group; coincidentally, as of 2002, this is the maximum squad size allowed at the [FIFA World Cup Tournament](https://en.wikipedia.org/wiki/FIFA_World_Cup). At the [Women's World Cup](https://en.wikipedia.org/wiki/FIFA_Women%27s_World_Cup), this number was established at the 2015 event. I therefore had 5 years' worth of teams who made it into the group stages to collect data from.

<br> <br>

Web Scraping
------------

Luckily for me, Wikipedia has [archived](https://en.wikipedia.org/wiki/Category:FIFA_World_Cup_squads) these teams (along with dates of birth) since the 1930 WC. All I had to do was collect the information.

I created a quick function which would allow me to scrape all html tables from a webpage, format each to fit my needs - which is to only see the names, the dates of birth and the team number - and concatenate all the tables together.



``` r
library(rvest)
library(dplyr)
```

``` r
# declare function to scrape wiki
getAllSquads <- function(url, n){
  temp.tables <- read_html(url) %>%
    html_nodes("table") %>%
    html_table(fill=T)
  
  output <- data.frame(Name=character(), DOB=character(), Team=numeric())
  
  for(i in 1:n){
    temp.df <- temp.tables[i] %>% as.data.frame()
    temp.df <- data.frame(Name=temp.df$Player,
                          DOB=substr(temp.df$Date.of.birth..age., 2, 11),
                          Team=i)
    output <- rbind(output, temp.df)
  }
  return(output)
}
```

<br>

I then initialised an empty data frame and filled it up by looping through each year 2002-2014, concatenating each year to the last.

``` r
# initialise df
all.squads <- data.frame(Name=character(), DOB=character(), Team=numeric(), Year=numeric())

# loop through 2002-2014 to fetch all WC squads
for(i in seq(from=2002, to=2014, by=4)){
  temp.df <- getAllSquads(paste("https://en.wikipedia.org/wiki/", i, "_FIFA_World_Cup_squads", sep=""), n=32)
  temp.df$Year <- i
  all.squads <- rbind(all.squads, temp.df)
}
```

<br>

I had to add the women's squads manually, as the URL takes a different form to the men's, and the group stages only host 24 teams as opposed to 32.

``` r
# add women's 2015
temp.df <- getAllSquads("https://en.wikipedia.org/wiki/2015_FIFA_Women's_World_Cup_squads", n=24)
temp.df$Year <- 2015
all.squads <- rbind(all.squads, temp.df)
```

<br> <br>

Analysis
--------

Armed with (probably) enough data, I went on to find out which teams had players sharing birthdays. I set up a data frame for all unique teams, and wrote a function which would determine whether a team had identical birthdays by looking at the count of each birthday within the subsets.

``` r
# get unique teams from year and team
teams <- unique(all.squads[c("Year", "Team")])

# initialise function for finding matching birthdays
findMatchingBday <- function(year.team){
  temp.df <- all.squads %>%
    subset(Year==as.numeric(substr(year.team, 1, 4)) & 
             Team==as.numeric(substr(year.team, 5, nchar(year.team))))
  
  bdays.table <- table(substr(temp.df$DOB, 6, 11)) %>% as.data.frame()
  
  if(max(bdays.table$Freq)>1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

teams$Has.Matching.Birthday <- sapply(paste(teams$Year, teams$Team, sep=""), findMatchingBday)
```

<br>

Result
------

All I have to do now is count up how many teams had people sharing birthdays:

``` r
table(teams$Has.Matching.Birthday)
```

    ## 
    ## FALSE  TRUE 
    ##    67    85

<br>

Nearly 60% of the teams had matching birthday players! (In fact, [Algeria in 2010](https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_squads#Algeria) had 3 people born on December the 5th.)

<br> <br>

### (Hypothesis Testing...)

*(...because numbers by themselves are useless).*

Getting nearly 60% on a 50% thing is a little bit suspect, so I decided to check the probability.

Plugging my numbers *k = 85* and *n = 152* into a cumulative binomial distribution gives me a probability of:

``` r
#hypothesis testing
probability <- 0
for(i in 85:152){
   probability <- probability + ((choose(152, i) * (0.5073^i) * (1-0.5073)^(152-i)))
}

probability
```

    ## [1] 0.1151734

*(I'm sure there is a much more elegant way of doing this, but this will do for now.)*

<br>

So the probability of getting 85 *or more* teams out of 152 given a probability of 50.73% is ~11.5%. Not the highest, but considering it's higher than 5%, we'll take it. I also wouldn't argue against the mathematics of the Birthday Paradox.
