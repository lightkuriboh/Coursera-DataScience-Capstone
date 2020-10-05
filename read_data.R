
twitter_conn <- file('data/en_US/en_US.twitter.txt')
news_conn <- file('data/en_US/en_US.news.txt')
blogs_conn <- file('data/en_US/en_US.blogs.txt')

open(twitter_conn)
open(news_conn)
open(blogs_conn)

twitter_data <- readLines(twitter_conn, skipNul=TRUE)
length(twitter_data)
max(sapply(twitter_data, nchar))

blogs_data <- readLines(blogs_conn, skipNul=TRUE)
length(blogs_data)
max(sapply(blogs_data, nchar))

news_data <- readLines(news_conn, skipNul=TRUE)
length(news_data)
max(sapply(news_data, nchar))

close(twitter_conn)
close(blogs_conn)
close(news_conn)

sum(grepl('love', twitter_data)) / sum(grepl('hate', twitter_data))
head(twitter_data[grepl('biostats', twitter_data)])
sum(grepl('A computer once beat me at chess, but it was no match for me at kickboxing', twitter_data))
