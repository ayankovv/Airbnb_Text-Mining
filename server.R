
library(shiny)
library(wordcloud)
library(dplyr)
library(tidytext)
library(topicmodels)
library(shinydashboard)
library(leaflet)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(mongolite)
library(igraph)
library(ggraph)
library(purrr)
library(tm)
library(SnowballC)
library(broom)
library(shinyjs)
library(reshape2)
library(textdata)


load("Description.RData")
load("airbnb_alldf.RData")
load("sentiment_us.RData")
load("bing_us.RData")
load("nrc_us.RData")
load("bing_and_nrc_us.RData")
load("combined_sentiments_us.RData")
load("bigrams_US.RData")
load("bigram_tf_idf_us.RData")
load("processed_words.RData")
load("word_counts.RData")
load("processed_descriptions.RData")
load("top_terms.RData")
load("total_documents.RData")
load("afinn_lexicon.RData")
load("afinn_data.RData")
load("bing_data.RData")
load("nrc_data.RData")

function(input, output, session) {
    useShinyjs()  # Initialize shinyjs functionality
    
    # Handle theme switching
    observeEvent(input$themeSelector, {
      jsCode <- if (input$themeSelector == "dark") {
        "document.body.style.backgroundColor = '#333'; document.body.style.color = '#EEE';"
      } else {
        "document.body.style.backgroundColor = 'white'; document.body.style.color = 'black';"
      }
      shinyjs::runjs(jsCode)
    })
    
    # Reset button logic
    observeEvent(input$resetButton, {
      updateSliderInput(session, "freqSlider", value = 20)
      updateSelectInput(session, "countrySelect", selected = "United States")
      updateSelectizeInput(session, "themeSelector", selected = "light")
    })
    
    
  output$bigramGraph <- renderPlot({
    
    bigrams_high_rated_us <- Description %>%
      filter(review_scores_rating >= input$freqSlider, country %in% input$countrySelect) %>%  # Use the selected country(ies)
      unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
      count(bigram, sort=TRUE) %>%
      separate(bigram, c("word1", "word2"), sep=" ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      top_n(15)
    
    
    # Now plot using ggraph
    ggraph(bigrams_high_rated_us, layout = "fr") +  
      geom_edge_link(aes(width = sqrt(n)), edge_alpha = 0.8, color = "gray") +  
      geom_node_point(size = 5, color = "steelblue") +  
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  })
  
  
  output$positiveWordCloud <- renderPlot({
    
    mydfdes <- airbnb_alldf %>%
      mutate(
        country = address$country,
        success = review_scores$review_scores_rating,  # assuming 'review_scores_rating' is in 'review_scores'
        text = summary
      ) %>%
      select(country, success, text, number_of_reviews)  # Make sure 'number_of_reviews' is included
    
    # Filtering data based on conditions
    filtered_df <- mydfdes %>%
      filter(success > input$freqSlider, number_of_reviews > 5, country %in% input$countrySelect)
    
    sentiment_us <- Description %>%
      filter(review_scores_rating >= input$freqSlider, country %in% input$countrySelect) %>%
      unnest_tokens(word, text)
    afinn_us <- sentiment_us %>%
      inner_join(afinn_data, by = "word") %>%
      summarise(sentiment = sum(value), .groups = 'drop') %>%
      mutate(method = "AFINN")
    
    bing_us <- sentiment_us %>%
      inner_join(bing_data, by = "word") %>%
      count(sentiment) %>%
      mutate(method = "Bing et al.")
    
    nrc_us <- sentiment_us %>%
      inner_join(nrc_data, by = "word") %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      count(sentiment) %>%
      mutate(method = "NRC")
    
    bing_and_nrc_us <- bind_rows(bing_us, nrc_us) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment_score = positive - negative)
    
    combined_sentiments_us <- bind_rows(
      summarise(afinn_us, sentiment = sentiment, method = method),
      summarise(bing_and_nrc_us, sentiment = sentiment_score, method = method)
    )
  
    blue_palette <- c("lightblue", "steelblue", "dodgerblue", "darkblue", "midnightblue")
    ggplot(combined_sentiments_us, aes(x = method, y = sentiment, fill = method)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = blue_palette) +  # Assign the custom blue colors
      labs(title = "Sentiment Analysis Comparison",
           x = "Method",
           y = "Sentiment Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$negativeWordCloud <- renderPlot({
    
    bigrams_US <- Description %>%
      filter(review_scores_rating >= input$freqSlider, country %in% input$countrySelect) %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      separate(bigram, into = c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>%
      unite(bigram, word1, word2, sep = " ") %>%
      select(document_id, bigram)
    
    bigram_tf_idf_us <- bigrams_US %>%
      group_by(bigram) %>%
      summarise(n = n(),  # Document frequency
                tf = sum(n) / total_documents) %>%  # Term frequency
      mutate(idf = log(total_documents / n),  # Inverse document frequency
             tf_idf = tf * idf) %>%
      arrange(desc(tf_idf)) %>%
      slice_max(order_by = tf_idf, n = 10)
    
    ggplot(bigram_tf_idf_us, aes(x = reorder(bigram, tf_idf), y = tf_idf, fill = tf_idf)) +
      geom_col(show.legend = FALSE) +  # Using 'fill' based on the 'tf_idf' score
      scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Setting gradient from light to dark blue
      coord_flip() +  # Flip coordinates for a horizontal bar chart
      labs(x = "Bigrams", y = "TF-IDF Score", title = "Top Bigrams by TF-IDF Score") +
      theme_minimal() 
  })
  output$topTermsTopics <- renderPlot({
    
    processed_words <- Description %>%
      filter(review_scores_rating >= input$freqSlider, country %in% input$countrySelect, !is.na(text)) %>%
      unnest_tokens(word, text)
    
    # Remove stopwords and apply stemming
    processed_words <- processed_words %>%
      anti_join(stop_words, by = "word") %>%
      mutate(word = wordStem(word))
    
    # For DTM via word counts
    word_counts <- processed_words %>%
      group_by(document_id, word) %>%
      summarise(n = n(), .groups = 'drop')
    
    # For creating corpus (text aggregation for each document)
    processed_descriptions <- processed_words %>%
      group_by(document_id) %>%
      summarise(text = paste(word, collapse=" "))
    
    # Creating DTM from corpus
    corpus <- Corpus(VectorSource(processed_descriptions$text))
    dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf, stopwords = FALSE))  # Note: stopwords already removed
    
    # Running LDA
    lda_model <- LDA(dtm, k = 2)
    topics <- tidy(lda_model, matrix = "beta")
    
    # Analyze top terms for each topic
    if(exists("topics") && nrow(top_terms) > 0) {
      top_terms <- topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
      
      # Plot
      ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = TRUE) +  # If needed, re-enable the legend
        facet_wrap(~ topic, scales = "free", ncol = 1) +  # Organize topics vertically
        coord_flip() +
        labs(x = "Terms", 
             y = "Importance", 
             fill = "Topic") +  # If the legend is enabled, provide a label for it
        theme_minimal() +  # Start with a minimal theme
        theme(
          axis.title = element_text(size = 12),  # Adjust title sizes
          axis.text = element_text(size = 10),  # Adjust text sizes
          plot.title = element_text(size = 14, hjust = 0.5),  # Center the plot title
          strip.text = element_text(size = 12),  # Adjust facet label sizes
          legend.position = "bottom"  # Position the legend at the bottom
        ) +
        scale_fill_brewer(palette = "Set1")
    } else {
      print("No topics or terms available for plotting.")
    }
  })
  observeEvent(input$aboutBtn, {
    showModal(modalDialog(
      title = "About This Analysis",  
      

"Welcome to our analytical dashboard, where we delve into Airbnb's extensive listing data to distill actionable business insights. Airbnb, a prominent player in the online accommodation and experience market, endeavors to perfect its platform for hosts and guests alike. This project scrutinizes the text and numerical data of Airbnb listings to identify characteristics that enhance property appeal and discover patterns that influence guest interest.", 


"Objective: Our mission is to conduct a thorough analysis of the content in Airbnb listings. We focus on extracting patterns, sentiments, and term significance to provide a nuanced understanding of what makes listings successful and engaging.",

"Methodology: Employing advanced data visualization tools and analytical techniques, we break down the complexity of data into insightful visual representations. Our approach encompasses:

Sentiment Analysis: Evaluating the emotional tone behind the words in listing descriptions to gauge the positive or negative sentiment they convey.

Bigrams Analysis: Identifying commonly paired words to understand context and highlight prominent features or themes in listings.

TF-IDF (Term Frequency-Inverse Document Frequency): Calculating the importance of bigrams within the listings corpus to pinpoint the most distinguishing terms across documents.

LDA (Latent Dirichlet Allocation) Modeling: Uncovering latent topics within the text data to reveal hidden patterns and preferences.",

"Each of these methodologies is critical in dissecting the wealth of textual data, transforming it into a strategic knowledge base. We blend sentiment analysis to decode the emotive undercurrents of property descriptions, employ bigrams analysis to capture nuanced language patterns, and utilize TF-IDF scoring on bigrams to single out terms that uniquely characterize listings. Through LDA modeling, we distill broad themes, facilitating a macro-level understanding of content strategies that resonate with potential guests.
Outcome: The insights garnered from this analysis aim to elevate Airbnb's platform user experience, guiding hosts in crafting compelling property narratives and aiding the platform in tailoring its offerings to meet customer preferences. The analysis also informs Airbnb's response to regulatory challenges, ensuring a harmonious balance between business expansion and community well-being.
With these insights, Airbnb fortifies its dedication to a hospitable, community-centric marketplace that honors the needs of its diverse user base while nurturing its competitive stance in the travel and hospitality sector.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  output$map <- renderLeaflet({
    filtered_data <- airbnb_alldf %>%
      filter(review_scores$review_scores_rating >= input$freqSlider, number_of_reviews > 2, country %in% input$countrySelect, !is.na(text))
    
    # You will need to create a dataframe with longitude and latitude columns
    locations <- data.frame(
      longitude = sapply(filtered_data$address$location$coordinates, function(coords) coords[1]),
      latitude = sapply(filtered_data$address$location$coordinates, function(coords) coords[2])
    )
    
    # Filter out any NA coordinates
    locations <- na.omit(locations)
    
    # Create a leaflet map
    leaflet(locations) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude)
  })
  # Handle theme switching
  observeEvent(input$themeSelector, {
    jsCode <- if (input$themeSelector == "dark") {
      "document.body.style.backgroundColor = '#333'; document.body.style.color = '#EEE';"
    } else {
      "document.body.style.backgroundColor = 'white'; document.body.style.color = 'black';"
    }
    shinyjs::runjs(jsCode)
  })
  
  # Reset button logic
  observeEvent(input$resetButton, {
    updateSliderInput(session, "freqSlider", value = 20)
    updateSelectInput(session, "countrySelect", selected = "United States")
    updateSelectizeInput(session, "themeSelector", selected = "light")
  })
}