library(tidytext)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(markovchain)
library(lexicon)
library(sbo)
library(stringi)
library(dplyr)
library(ggplot2)
library(igraph)
library(forcats)
library(caret)
library(corpus)

data("profanity_alvarez")
data("profanity_racist")
profanity <- data.frame(word = profanity_alvarez)
racist <- data.frame(word = profanity_racist)
remove_words <- rbind(profanity, racist)

setwd("~/Desktop/Data_science/predictive_text/")
options(scipen = 1)

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
gc(reset = TRUE)

#Exploratory analysis
#Remove numbers, profanity, and stop words.Count and display most common words
corpus_words <- corpus %>%
        unnest_tokens(word, text) %>%
        filter(!grepl('[0-9]', word)) %>%
        anti_join(stop_words) %>%
        anti_join(remove_words) %>%
        count(source, word, sort = TRUE)

corpus_words %>%
        group_by(source) %>%
        filter(n > 27500) %>%
        arrange(desc(n)) %>%
        ggplot(aes(n, word, fill = source)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~source, ncol = 2, scales = "free_y")

corpus_total <- corpus_words %>%
        group_by(source) %>%
        summarize(total = sum(n))

corpus_words <- left_join(corpus_words, corpus_total)

source_tf_idf <- corpus_words %>%
        bind_tf_idf(word, source, n) %>%
        select(-total) %>%
        arrange(desc(tf_idf))

source_tf_idf %>%
        group_by(source) %>%
        slice_max(tf_idf, n = 15) %>%
        ungroup() %>%
        ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = source)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~source, ncol = 2, scales = "free") +
        labs(x = "tf-idf", y = NULL)

gc(reset = TRUE)

# Add sources to the corpus

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

full_corpus <- rbind(corpus, austen, bronte, hgwells, dickens, darwin, doyle, lit)
full_corpus <- full_corpus[!(full_corpus$text == "" | full_corpus$text == "  "), ]
rm(list = c("austen", "bronte", "hgwells", "dickens", "darwin", "doyle", "lit"))
gc(reset = TRUE)

# prediction model
set.seed = 5360
attach(full_corpus)
trainIndex <- createDataPartition(y = source, p = 0.999, list = FALSE, times = 1)
train <- full_corpus[trainIndex, ]
test <- full_corpus[-trainIndex, ]
detach(full_corpus)

set.seed = 5361
textmodel <- sbo_predictor(object = train$text,
                             N = 4,
                             dict = target ~0.75,
                             .preprocess = sbo::preprocess,
                             EOS = ".?!:;",
                             lambda = 0.4,
                             L = 3L,
                             filtered = c("<UNK>", "<EOS>")
                                     )

evaluation <- eval_sbo_predictor(textmodel, test = test$text)
evaluation %>%
        summarise(accuracy = sum(correct)/n(),
                  uncertainty = sqrt(accuracy*(1 - accuracy)/n())
                  )

gc(reset = TRUE)