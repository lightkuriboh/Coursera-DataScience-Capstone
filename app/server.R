
library(shiny)
library(tm)
library(dplyr)

shinyServer(function(input, output, session) {
    default_suggestion <- 'It'
    load('distribution_matrix.RData')
    words_in_history <- unique(
        unname(
            unlist(
                sapply(
                    rownames(distribution_matrix),
                    strsplit, ' '
                )
            )
        )
    )
    previous_words_count <- 0

    build_transition_matrix <- function (distribution_matrix) {
        t(apply(distribution_matrix, 1, function(current_row) {
            row_sum <- sum(current_row)
            current_row / row_sum
        }))
    }
    transition_matrix <- build_transition_matrix(distribution_matrix=distribution_matrix)

    get_top_suggestions <- function (history, model, top=10) {
        clean_sentence <- function(sentence) {
            replacePunctuation <- tm::content_transformer(
                function(x) gsub("[^[:alnum:][:space:]'`]", " ", x)
            )
            clean_corpus <- function(corpus) {
                corpus %>%
                    tm::tm_map(tm::stripWhitespace) %>%
                    tm::tm_map(replacePunctuation) %>%
                    tm::tm_map(tm::removeNumbers) %>%
                    tm::tm_map(content_transformer(tolower))
            }
            clean_corpus(tm::VCorpus(tm::VectorSource(sentence)))[[1]]$content
        }
        remove_unseen_words <- function (sentence) {
            words <- strsplit(sentence, ' ')[[1]]
            paste(words[words %in% words_in_history], collapse=' ')
        }

        get_data_from_matrix <- function (history, model, top) {
            top_suggestions_indices <- head(
                order(model[history, ], decreasing = TRUE),
                n=top
            )
            top_suggestions <- model[history, top_suggestions_indices]
            names(top_suggestions) <- colnames(model)[top_suggestions_indices]
            names(top_suggestions)
        }

        history <- clean_sentence(history)
        history <- remove_unseen_words(history)
        if (history == '') {
            return(c(default_suggestion))
        }
        if (history %in% rownames(model)) {
            return(get_data_from_matrix(history, model, top))
        }
        get_top_suggestions(
            paste(
                strsplit(history, ' ')[[1]][-1],
                collapse=' '
            ),
            model,
            top
        )
    }
    get_last_character <- function (sentence, skip_spaces=TRUE) {
        iterator <- nchar(sentence)
        while (iterator > 0) {
            ch <- substr(sentence, iterator, iterator)
            if (!skip_spaces || ch != ' ') {
                return(ch)
            }
            iterator <- iterator - 1
        }
        ''
    }

    generate_prediction <- reactive({
        words_history <- input[['text_history']]

        if (get_last_character(words_history) == '.') {
            return(c(default_suggestion))
        }
        # predicted_words <- get_top_suggestions(history=words_history,
        #                                        model=build_transition_matrix(
        #                                            distribution_matrix
        #                                            )
        #                                        )
        predicted_words <- get_top_suggestions(history=words_history,
                                               model=transition_matrix
                                               )
        predicted_words
    })

    observe({
        for (i in min(1, previous_words_count):previous_words_count) {
            removeUI(
                selector=paste('#option', i, sep='_')
            )
        }
        words <- generate_prediction()
        for (i in 1:length(words)) {
            button_label <- paste('option', i, sep='_')

            insertUI(
                selector='#words',
                where='afterEnd',
                ui=actionButton(
                    button_label,
                    words[i],
                    onclick="
                            Shiny.setInputValue('btnLabel', this.innerText);
                            "
                )
            )
        }
        previous_words_count <<- length(words)
    })

    observe({
        word_clicked <- input$btnLabel
        if (!is.null(word_clicked)) {
            session$sendCustomMessage('reset-input', '')
            current_text <- input[['text_history']]
            separator <- ' '
            if (get_last_character(current_text, FALSE) == separator) {
                separator <- ''
            }
            updateTextAreaInput(session, 'text_history',
                            value=paste(current_text,
                                        word_clicked,
                                        sep=separator)
                            )
            session$sendCustomMessage('scroll-text-area', '')
        }
    })
})
