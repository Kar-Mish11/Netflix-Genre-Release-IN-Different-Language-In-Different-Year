install.packages("tidyverse")
library(tidyverse) # metapackage of all tidyverse packages
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)
install.packages("magrittr")
library(dplyr)
ntf=read.csv("C:/Users/kartik/Desktop/Data Analyst working/Netflix/NetflixOriginals.csv")
#First thing to do: get years from "Premiere" and separate Genres and Languages.
ntf <- ntf %>% mutate(Year = as.numeric(str_sub(Premiere, start = -4)))
ntf <- unnest_tokens(ntf, OutGenre, Genre, token = stringr::str_split, pattern = "/")
ntf <- unnest_tokens(ntf, OutLanguage, Language, token = stringr::str_split, pattern = "/")
#Now, I wish to use violin plots to see scores by language. I will just consider the five more common languages to turn visualization easier.
top10 <- ntf %>% count(OutLanguage) %>% arrange(desc(n))
top10 <- (head(top10,5))

ntf %>% filter(OutLanguage %in% top10$OutLanguage) %>% ggplot(aes(x = IMDB.Score, fill = OutLanguage)) + geom_density(alpha = 0.5)
#Looks like a balanced dispute here, although French and Italian originals are somewhat behind.

#Now, the same thing with genres:
ntf %>% filter(OutGenre %in% c("drama", "comedy", "thriller", "documentary", "horror thriller")) %>% ggplot(aes(x = IMDB.Score, fill = OutGenre)) + geom_density(alpha = 0.5)
#Not much surprised, really... are you? I could also do this plot with violin plots.
ntf %>% filter(OutGenre %in% c("drama", "comedy", "thriller", "documentary", "horror thriller")) %>% ggplot(aes(x = IMDB.Score, y = OutGenre, color = OutGenre)) + geom_violin() + geom_jitter()
#Looks like the "horror thriller" genre was poorly represented here, both in score AND in numbers, a detail a single density plot cannot compute. Gotta love those violin+jitter plots.

#Now, I want to put time in this plot. Let's use those previously gotten Years to do something.
ntf %>% filter(OutGenre == "documentary") %>% ggplot(aes(x = IMDB.Score, y = Year, color =
                                                           as.factor(Year))) + geom_violin(draw_quantiles = 0.5) + geom_jitter()
#It gives the perception documentaries' scores are falling a little year by year. Let's confirm this.
ntf %>% filter(OutGenre == "documentary") %>% group_by(Year) %>% summarise(mean = mean(IMDB.Score), n = n()) %>% ggplot(aes(x = Year, y = mean, label = mean)) + geom_line() + geom_label()
#Remember from the violin plot that 2014 had a single documentary, so it does not really add to the analysis. Except for 2020, it can be seen lower average scores, indeed.

#Now an alluvial plot comparing sources of genre, language and year.
install.packages("alluvial")
install.packages("network")
library(alluvial)
library(network)

ntff <- ntf %>% group_by(OutGenre, OutLanguage, Year) %>% summarise(freq = n()) %>% filter(freq>4)
pal <- as.color(ntff$OutGenre)
alluvial(ntff[1:3], freq=ntff$freq,
         cex = 0.7, col = pal
)
#Quite a large, predictable English domination. Let's try without English.
ntff <- ntf %>% group_by(OutGenre, OutLanguage, Year) %>% summarise(freq = n()) %>% filter(OutLanguage != "english") %>% filter(freq > 2)
pal <- as.color(ntff$OutGenre)
alluvial(ntff[1:3], freq=ntff$freq,
         cex = 0.7, col = pal
)
