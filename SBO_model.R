library(sbo)
library(dplyr)
library(stringi)
library(tidytext)

#blog data
con <- file("final/en_US/en_US.blogs.txt", open = "r")
blogs <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

#news data
con <- file("final/en_US/en_US.news.txt", open = "r")
news <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

#twitter data
con <- file("final/en_US/en_US.twitter.txt", open = "r")
twit <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)

#Turn into tidy data
blogs_df <- tibble(text = blogs)
blogs_df$source <- rep("blog", length.out = length(blogs_df))
blogs_df$source = as.factor(blogs_df$source)
news_df <- tibble(text = news)
news_df$source <- rep("news", length.out = length(news_df))
news_df$source <- as.factor(news_df$source)
twit_df <- tibble(text = twit)
twit_df$source <- rep("twitter", length.out = length(twit_df))
twit_df$source <- as.factor(twit_df$source)

corpus <- rbind(blogs_df, news_df, twit_df)

rm(list = c("blogs_df", "news_df", "twit_df", "blogs", "con", "news", "twit"))
invisible(gc(reset = TRUE))

#Add books to the corpus
library(janeaustenr)
library(gutenbergr)

austen <- austen_books() %>%
        rename(source = book)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767)) %>%
        rename(source = gutenberg_id)
bronte$source = as.factor(bronte$source)

hgwells <- gutenberg_download(c(35, 36, 5230, 159)) %>%
        rename(source = gutenberg_id)
hgwells$source = as.factor(hgwells$source)

dickens <- gutenberg_download(c(46, 1400, 766, 730, 580)) %>%
        rename(source = gutenberg_id)
dickens$source = as.factor(dickens$source)

darwin <- gutenberg_download(c(2009, 944, 2300)) %>%
        rename(source = gutenberg_id) 
darwin$source = as.factor(darwin$source)

doyle <- gutenberg_download(c(139, 1661, 2852, 3289, 834, 108, 2350)) %>%
        rename(source = gutenberg_id) 
doyle$source = as.factor(doyle$source)

lit <- gutenberg_download(c(10609)) %>%
        rename(source = gutenberg_id)
lit$source = as.factor(lit$source)

corpus <- rbind(corpus, austen, bronte, hgwells, dickens, darwin, doyle, lit)
corpus <- corpus[!(corpus$text == "" | corpus$text == "  "), ]
rm(list = c("austen", "bronte", "hgwells", "dickens", "darwin", "doyle", "lit"))

invisible(gc(reset = TRUE))

model <- sbo_predtable(object = corpus$text,
                       N = 4,
                       dict = target ~0.75,
                       .preprocess = sbo::preprocess,
                       EOS = ".?!:;",
                       lambda = 0.4,
                       L = 3L,
                       filtered = c("<UNK>", "<EOS>")
                       )

save(model, file = "model.rda")