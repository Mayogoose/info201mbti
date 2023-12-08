---
output: html_document
runtime: shiny
---
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#------------------------------------------------------

library(shiny)
library(fmsb)
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)


mbtis <- read_delim("combined_mbti_df.csv")
spo <- read_csv("spotify-2023.csv")

mbti_name <- c(
  "INTJ" = "Architect",
  "INTP" = "Logician",
  "ENTJ" = "Commander",
  "ENTP" = "Debater",
  "INFJ" = "Advocate",
  "INFP" = "Mediator",
  "ENFJ" = "Protagonist",
  "ENFP" = "Campaigner",
  "ISTJ" = "Logistician",
  "ISFJ" = "Defender",
  "ESTJ" = "Executive",
  "ESFJ" = "Consul",
  "ISTP" = "Virtuoso",
  "ISFP" = "Adventurer",
  "ESTP" = "Entrepreneur",
  "ESFP" = "Entertainer"
)


mbtis <- mbtis %>% 
  mutate(mbti_name = mbti_name[mbti]) %>% 
  select(mbti, function_pair, mbti_name,
         danceability = danceability_mean, 
         valence = valence_mean, 
         energy = energy_mean, 
         acousticness = acousticness_mean, 
         instrumentalness = instrumentalness_mean, 
         liveness = liveness_mean, 
         speechiness = speechiness_mean)


mbtis_means <- mbtis %>%
  group_by(mbti) %>%
  summarise(Danceability = mean(danceability, na.rm = TRUE),
            Energy = mean(energy, na.rm = TRUE),
            Valence = mean(valence, na.rm = TRUE),
            Instrumentalness = mean(instrumentalness, na.rm = TRUE),
            Acousticness = mean(acousticness, na.rm = TRUE),
            Speechiness = mean(speechiness, na.rm = TRUE))

chr_col <- sapply(mbtis[, 1:3], function(x) any(sapply(x, is.numeric)))

contains_characters <- sapply(mbtis[, -c(1:3)], function(x) any(sapply(x, is.character)))

min_values <- sapply(mbtis[, -c(1:3)], min, na.rm = TRUE)
max_values <- sapply(mbtis[, -c(1:3)], max, na.rm = TRUE)

list(
  Check_for_numeric = chr_col,
  Check_for_character = contains_characters,
  MinValues = min_values,
  MaxValues = max_values
)

spo <- spo %>% 
  select(
    track_name, 
    artist = `artist(s)_name`,
    streams,
    danceability, 
    valence, 
    energy, 
    acousticness = `acousticness_%`, 
    instrumentalness, 
    liveness, 
    speechiness
  )


write_csv(mbtis, "mbtis_cleaned.csv")
write_csv(spo, "spotify_cleaned.csv")

mbti_colors <- c(ENFP="#9ac262", ENFJ="#9ac262", ENTJ="#95637e", ENTP="#95637e", 
                 ESFP="#e5c727", ESFJ="#72cccd", ESTP="#e5c727", ESTJ="#72cccd", 
                 INFP="#9ac262", INFJ="#9ac262", INTJ="#95637e", INTP="#95637e", 
                 ISFP="#e5c727", ISFJ="#72cccd", ISTP="#e5c727", ISTJ="#72cccd")

mbti_name <- c(
  "INTJ" = "Architect",
  "INTP" = "Logician",
  "ENTJ" = "Commander",
  "ENTP" = "Debater",
  "INFJ" = "Advocate",
  "INFP" = "Mediator",
  "ENFJ" = "Protagonist",
  "ENFP" = "Campaigner",
  "ISTJ" = "Logistician",
  "ISFJ" = "Defender",
  "ESTJ" = "Executive",
  "ESFJ" = "Consul",
  "ISTP" = "Virtuoso",
  "ISFP" = "Adventurer",
  "ESTP" = "Entrepreneur",
  "ESFP" = "Entertainer"
)

mbti_images <- c(
  "INTJ" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQi2VRFrPrpaSQSu8P9KpMiuZUbEjAMYzpyNdgZLsng2Q&s",
  
  "INTP" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSlOUv5i5v8mqz19m7xV7ZD5WPwT3wm3SgV-lmatt4Y-w&s",
  
  "ENTJ" = "https://www.16personalities.com/static/images/personality-types/avatars/entj-commander.png",
  
  "ENTP" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQyWr3zzuitEGEJa1vq0PK_Rsdwuc_z-3IW5rJsT74NKg&s",
  
  "INFJ" = "https://www.16personalities.com/static/images/personality-types/avatars/infj-advocate.svg?v=3",
  
  "INFP" = "https://www.16personalities.com/static/images/personality-types/avatars/infp-mediator.png",
  
  "ENFJ" = "https://www.16personalities.com/static/images/personality-types/avatars/enfj-protagonist.png",
  
  "ENFP" = "https://www.16personalities.com/static/images/personality-types/avatars/enfp-campaigner.svg?v=3",
  
  "ISTJ" = "https://www.16personalities.com/static/images/personality-types/avatars/istj-logistician.png",
  
  "ISFJ" = "https://www.16personalities.com/static/images/personality-types/avatars/isfj-defender.png",
  
  "ESTJ" = "https://www.16personalities.com/static/images/personality-types/avatars/estj-executive.png",
  
  "ESFJ" = "https://www.16personalities.com/static/images/personality-types/avatars/esfj-consul.png", 
  
  "ISTP" = "https://www.16personalities.com/static/images/personality-types/avatars/istp-virtuoso.png",
  
  "ISFP" = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT-_cAGZD35zF-jIvqaP_ZROUgP8PyYINJlJdEqAIVWvA&s",
  
  "ESTP" = "https://www.16personalities.com/static/images/personality-types/avatars/estp-entrepreneur.png",
  
  "ESFP" = "https://static.neris-assets.com/images/personality-types/avatars/esfp-entertainer.png"
)




#q4-----------------------------------------------------------------------------------------

scale_to_percentage <- function(x) {
  (x - min(x)) / (max(x) - min(x)) * 100
}
mbtis_3 <- mbtis %>% 
  select(-function_pair, -mbti_name) %>% 
  group_by(mbti) %>% 
  summarise_all(mean) %>% 
  mutate(across(where(is.numeric), scale_to_percentage))


# Prepare Spotify data
spo_3 <- spo %>% 
  mutate(streams = as.numeric(streams)) %>%
  arrange(desc(streams)) %>%
  head(300) %>% 
  select(-streams, -artist) 

calculate_best_fit <- function(mbti_row) {
  min_score <- Inf
  best_song <- NA
  for (i in 1:nrow(spo_3)) {
    total_diff <- 0
    for (j in 2:8) {
      total_diff <- total_diff + abs(mbti_row[[j]] - spo_3[i, j])
    }
    score <- total_diff / 7
    if (score < min_score) {
      min_score <- score
      best_song <- spo_3[i, 'track_name']
    }
  }
  return(data.frame(mbti = mbti_row['mbti'], best_fit_song = best_song))
}

# Initialize an empty data frame for the results
best_fit_songs <- data.frame(mbti = character(), 
                             best_fit_song = character())

# Iterate over each MBTI type and calculate the best fit song
for (i in 1:nrow(mbtis_3)) {
  mbti_row <- mbtis_3[i, ]
  best_fit_result <- calculate_best_fit(mbti_row)
  best_fit_songs <- rbind(best_fit_songs, best_fit_result)
}

# Merge the results with the original mbtis_3 data
merged_data <- mbtis_3 %>% 
  left_join(best_fit_songs, by = "mbti") %>% 
  select(mbti, "song_name" = track_name)

#---

merged_data_copy <- merged_data

mbtis_4 <- mbtis_3

top_artists <- spo %>%
  filter(!grepl(",", artist)) %>%
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist) %>%
  summarise(total_streams = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(total_streams)) %>% 
  head(100)

spo_4 <- spo %>%
  filter(artist %in% top_artists$artist) %>%
  select(-track_name, -streams) %>%
  group_by(artist) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))


calculate_best_fit <- function(mbti_row) {
  min_score <- Inf
  best_song <- NA
  for (i in 1:nrow(spo_4)) {
    total_diff <- 0
    for (j in 2:8) {
      total_diff <- total_diff + abs(mbti_row[[j]] - spo_4[i, j])
    }
    score <- total_diff / 7 
    if (score < min_score) {
      min_score <- score
      best_song <- spo_4[i, 'artist']
    }
  }
  return(data.frame(mbti = mbti_row['mbti'], best_fit_song = best_song))
}

# Initialize an empty data frame for the results
best_fit_songs <- data.frame(mbti = character(), best_fit_song = character())

# Iterate over each MBTI type and calculate the best fit song
for (i in 1:nrow(mbtis_4)) {
  mbti_row <- mbtis_4[i, ]
  best_fit_result <- calculate_best_fit(mbti_row)
  best_fit_songs <- rbind(best_fit_songs, best_fit_result)
}

# Merge the results with the original mbtis_4 data
merged_data_copy <- mbtis_4 %>% 
  left_join(best_fit_songs, by = "mbti") %>% 
  select(mbti, artist)

#-------------------------------------------------------------------------------------------


sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}

ui <- fluidPage(
  
  # Create tabs with the navbarPage function
  navbarPage(
    "MBTI Recommender",
    
    tabPanel("Project Overview",
             uiOutput("overviewoutput")
    ),
    
    tabPanel("Background Info",
             uiOutput("backgroundoutput")
    ),
    
    tabPanel("Valence Graph",
             uiOutput("valence_output")
    ),
    
    tabPanel("Functioning Pairs",
             uiOutput("functioningpairs_output")
    ),
    
    tabPanel("16 Types",
             sidebarLayout(
               sidebarPanel2(
                 selectInput("selectedMBTI", "Choose MBTI Type:", 
                             choices = unique(mbtis$mbti)),
                 out = HTML("<p>We use radar charts of variables for each of the 16 MBTI types to infer individual preferences of the six music characteristics we picked.</p>
                            <p>Here there is a distinct pattern of introverted (I) types being characterized by a large amount of instrumentalness, the only slight exceptions being introverted SP function pairs (ISxP) do not lean towards instrumentalness so much. But, in all of the personalities, the extroverted (E) types very clearly lean away from instrumentalness and lean more towards every other attribute; there is no extroverted (E) type that has another attribute less than instrumentalness. It is also clear that there are distinct differences in each of the sixteen personality types, with some similarities between function pairs.</p>
                            <p></p>")
               ),
               mainPanel(
                 fluidRow(
                   column(width = 6, plotOutput("radarChart")),
                   column(width = 6, 
                          fluidRow(
                            column(width = 12,
                                   uiOutput("mbtiImage"),
                                   style = "margin-top: 60px; margin-right: 10px;"  # Adjust margin values as needed
                            ),
                            column(width = 12, 
                                   textOutput("mbtiName")
                            )
                          )
                   )
                 ),
               )
             )
    ),
    
    
    tabPanel("Recommender",
             uiOutput("recommenderoutput")
    ),
    
    tabPanel("Conclusion",
             uiOutput("conclusionoutput")
    )
  )
)




server <- function(input, output) {
  
  output$overviewoutput <- renderUI({
    fluidRow(
      uiOutput("overviewtext")
    )
  })
  
  output$backgroundoutput <- renderUI({
    fluidRow(
      uiOutput("backgroundtext")
    )
  })
  
  output$valence_output <- renderUI({
    fluidRow(
      plotOutput("valence_plot", width = 600),
      uiOutput("valence_textoutput")
    )
  })
  
  output$functioningpairs_output <- renderUI({
    fluidRow(
      plotOutput("functioningpairplot1", width = 600),
      uiOutput("functioningpair_text"),
      plotOutput("functioningpairplot2", width = 600)
    )
  })
  
  output$recommenderoutput <- renderUI({
    fluidRow(
      dataTableOutput("recommendertable1"),
      dataTableOutput("recommendertable2"),
      uiOutput("recommendertext")
    )
    
  })
  
  output$conclusionoutput <- renderUI({
    fluidRow(
      uiOutput("conclusiontext")
    )
  })
  
  #overview--------------------------------------------------------------------------------
  

  output$overviewtext <- renderUI({
    HTML("<h3>Overview</h3>
         <p>If you are a Millennial or a Gen Z on social media during the first week of December, you might see your friends reposting their Spotify Wrapped all over social media. Spotify Wrapped lets users share the 5 artists/songs a user has listened to the most and branded its so-called “listening personality”. Similarly, mbti has become popular among young adults. We start to wonder if people with a certain mbti personality type would have a certain kind of Spotify Wrapped.</p>
         <h3>Motivation</h3>
         <p>Teenagers and young adults who want to know the music tastes of their friends based on their mbti or knowledge about their personality, so that they can play songs that they might like. (This might demand a more predictive model using machine learning)</p>
         <p>A noticeable number of teens and young adults are familiar with the MBTI personality classification. Many will need more detailed explanations of each of the functions in the MBTI personality type classifications to be able to fully comprehend our data. Furthermore, though a large portion of the target audience population have used Spotify or other music players, few will have had the chance to explore what the variables in our music metadata mean. </p>
         <h3>Data Sets</h3>
         <p>The first dataset we are using is [mbti-spotify-dataset](https://www.kaggle.com/datasets/xtrnglc/spotify-mbti-playlists) by Trung Le. This combined dataset contains 4081 rows and 49 columns of data. Each row is an observation of aggregated mean music metadata for a Spotify playlist. The data is crowd sourced by Spotify users, manifested as playlists names that include self-identified mbti types. While the data set is reasonable and integral, it is susceptible to biases rooted in the expressiveness of each personality type. The data set also has errors such as marking a playlist named “esty, entj, intj, istj, istp” as ISTJ, which can possibly undermine or deviate the pattern we find.</p>
         <p>The second dataset we are using is [Most Streamed Songs on Spotify in 2023](https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023) collected by Nidula Elgiriyewithana on Kaggle. This dataset contains 943 rows and 24 columns of data. Each row is a song with spotify metadata, such as artist name, released date, and audio quality measurements. The dataset has some encoding errors, as manifested by ï¿½. We need to manually filter out songs that include garbled code to prevent its presence in our recommended songs for part 4.
</p>
         <p>Both datasets are from Kaggle, an online repository that allows users to share models and datasets for the community to non-commercial use. The first dataset was updated a year ago, and the second updated two months ago. These datasets were retrieved using reliable techniques such as scraping the Spotify API and computational methods that will minimize human input error, giving it good integrity for our purposes. 
</p>
         <p>The mbtis dataset looks good, with no null or unreasonable data. All codes in the first three columns should consist of characters without any numeric values, so any data containing numeric values will be considered invalid. According to the code, there is no invalid data in the first three columns. Musical feature data should not contain anything other than numerical values, so any data that is not entirely numerical will be considered worthless. Judging from the code, there is no invalid data in the remaining columns. According to the calculation standard of the data set, the music features should all be in the range of 0 to 1. By finding the maximum and minimum values, all values are within that range.
</p>
         <p>The spotify data doesn’t look good. The first two columns are song names and authors that should be in character form. However, as we loaded the data, some of them were unable to translate to a wide string. We found out that some characters in Spanish were not able to be shown in the dataset. These data was replaced by replacement character such as �, ï,¿,½.  The song will be displaying like 'Frï¿½ï¿½gil', which is unable to be comprehend by audience. It's probably due to how the dataset was encoded and we couldn't find the right way to encode it. Therefore, we decided to filter all rows with unreadable song name or author name out. There are 70 song names with replacement character and 48 song authors with replacement character. For streams, there is one song with a stream starting with “BPM110” which shouldn't be like that. So we decided to filter this out also. The value of music traits looks good without character in it. The range was from 0-100 which is reasonable.
</p>
         <h3></h3>
         <p></p>
         ")
  })
  
  
  #background--------------------------------------------------------------------------------
  
  output$backgroundtext <- renderUI({
    HTML("<h3>Context: What is MBTI?</h3>
         <p>MBTI stands for Myers-Briggs Type Indicator, developed by Katharine Cook Briggs and Isabel Briggs Myers in the 1940s based on the psychological theories of Carl Jung. The idea behind MBTI is that individuals can be categorized into 16 distinct personality types based on four basic preferences: extraversion (E) vs. introversion (I), sensing (S) vs. intuition (N), thinking (T) vs. feeling (F), and judging (J) vs. perceiving (P). </p>
        <ul>
         <li>Extraversion (E) vs. introversion (I): Measures whether a person gains more energy by being in the company of other people or alone.</li>
         <li>Sensing (S) vs. intuition (I): Measures whether a person relies on their past experiences to interpret their world now, or if they rely on gut feeling to interpret the world.</li>
         <li>Thinking (T) vs. feeling (F) measures whether a person values logic or emotions when they are in the need of making any decisions or in any other context.</li>
         <li>Judging (J) vs. perceiving (P) is the extent to which an individual plans ahead or needs a structured plan; judging types tend to need a more formal and solidified expectation to their tasks ahead, while perceiving types are more likely to be comfortable with a rough outline and approach things as they come. Not only are these tendencies and values expressed in personal decision-making processes, it can be seen in an individual’s worldview and their mindset, affecting how they perceive the world and how they approach other people’s situations too. </li>
        </ul>
         <p><All combinations of these four facets come out to a total of 16 personalities in the form of four letters, such as ENFP. /p>
         <p>There are also broader categories, namely, the four function pairs: NF, NT, SJ, SP. These are paired to look at personalities from a broader sense due to their tendencies to be more similar to each other. It is notable that they do not pair the same two facets; intuition (N) pairs are paired with whether they lean towards thinking (T) or feeling (F), while sensing (S) pairs are paired with whether they lean towards judging (J) or perceiving (P). This is due to the nature of N versus S and how the combinations work; for example, it is broadly assumed that an INFP individual and an INFJ individual can be very hard to tell apart due to the nature of the combination of N and F or N and T forming a more distinctly different personality type than say an N and J combination. For sensing (S) types, this is not the case, and instead an ESFJ and ESFP will be more different than an ESFJ and ESTJ. </p>
         <h3>Why do we do this?</h3>
         <p>This is a fascinating topic because the human mind is a very complex system that makes fully understanding ourselves and others something both seemingly reachable yet unthinkable. From an outsider’s perspective, it feels incredible that philosophers such as Carl Jung were able to pull out several distinct essences and devise explanations for our complicated human beings. It is a means for the general public to be able to somewhat catch a glimpse into a potential deeper grasp of who they are, something that many people seek in the modern world where self-discovery has slowly become an important desire in life. Though many people have now been exposed to the idea of the 16 personalities in MBTI, the original philosophy is actually a very convoluted model that captures a lot more of the nuance to Jung’s theory, ranging from developmental changes and predictions in facets of the personality morphing throughout life to shadow personalities and hidden masks and personas. The MBTI is a lot more simplistic in the way that it is able to provide non-fanatics some of the rudimentary ideas of Jung’s philosophy to suit their own needs, carrying a lingerance of accuracy and reliability in giving people a glimpse of their inner personality and mindset. Through the MBTI test, people can read up on who they are more likely to be compatible with and create deeper bonds with others by examining what the websites propose their personalities are like.</p>
         <p>Much like the MBTI, music taste is a very nuanced interest for people. Using metadata about songs on Spotify, the metadata on aspects of music such as valence can be a way to describe the happiness expression extent of the music. It is a multidimensional approach to classifying music and therefore in a way the people who enjoy listening to that music. Understanding music taste is understanding yet another crucial dimension to a person. The role that music plays for an individual and what the story of the music they listen to is inherently shaped by who they are, their own stories, their preferences, and what they feel connected to.</p>
         <p>Thus, we planned to draw correlations and find patterns between people’s mbti personality type and their music taste, measured by the mean values of all music metadata of their playlists. Then, using music metadata (danceability, energy, loudness, speechiness, acousticness, liveliness, valence, tempo, instrumentalness), we can find specific songs from “Spotify’s trending music 2023” music library to recommend to our audience some songs based on their inputting their MBTI.</p>
         ")
  })
  
  
  #valence output--------------------------------------------------------------------------------

  output$valence_plot <- renderPlot({
    mbtis %>%
      group_by(mbti, group = ceiling(row_number() / 30)) %>%
      summarize(n = mean(valence)) %>%
      mutate(four_cats = ifelse(str_detect(mbti, "NF"), "NF",
                                ifelse(str_detect(mbti, "NT"), "NT",
                                       ifelse(str_detect(mbti, "SF"), "SF",
                                              ifelse(str_detect(mbti, "ST"), "ST", ""))))) %>%
      arrange(desc(four_cats)) %>%
      ggplot(aes(mbti, n, color=mbti)) +
      geom_point() +
      labs(x = "mbti", y="mean valence") +
      scale_color_manual(
        values = c( ENTJ="#95637e",
                    ENTP="#95637e",
                    INTJ="#95637e",
                    INTP="#95637e",
                    ENFP="#9ac262",
                    ENFJ="#9ac262",
                    INFP="#9ac262",
                    INFJ="#9ac262",
                    ESFJ="#72cccd",
                    ESTJ="#72cccd",
                    ISFJ="#72cccd",
                    ISTJ="#72cccd",
                    ESFP="#e5c727",
                    ESTP="#e5c727",
                    ISFP="#e5c727",
                    ISTP="#e5c727"
        )) +
      theme(axis.text.x = element_text(angle = 45))
  })

  output$valence_textoutput <- renderUI({
    HTML("<p>We first grouped the mbti-spotify data based on 16 MBTI types and generated a summary variable for their primary musical valence. By assigning different colors to each MBTI function pair, we positioned extraverts to the left and introverts to the right in the graph. The arrangement makes it easy to see that extraverted individuals tend to gravitate toward music with higher valence.</p>
  <p>Valence is a measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry). On the x-axis of the plot each MBTI type is listed, and on the y-axis of the plot is the mean valence of the music playlists. The extroverted (E) MBTI types are on the left and introverted types are on the right. There is a clear visible trend of extroverted types having a higher mean valence of songs than introverted types. In general, perceiving (P) types and sensing (S) types correlate to a higher mean valence, whereas judging (J) types and intuition (N) correlate to a lower mean valence. We are curious if different axis combination has different effects on people’s music taste, so we generated our second plot based on 4 functioning pair groups.</p>
    ")
  })
  
  #functioning pair output--------------------------------------------------------------------------------
  
  output$functioningpairplot1 <- renderPlot({
    subjective <- mbtis %>%
      group_by(function_pair) %>%
      summarize(
        mean_danceability = mean(danceability, na.rm = TRUE),
        mean_energy = mean(energy, na.rm = TRUE),
        mean_valence = mean(valence, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = starts_with("mean"), 
                   names_to = "attribute", 
                   values_to = "mean_value")
    
    ggplot(subjective, aes(x = function_pair, y = mean_value, fill = function_pair)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ attribute, scales = "free_y") +
      labs(title = "Mean Subjective Music Attributes by MBTI Functioning Pair", 
           x = "subjective measurement", 
           y = "Mean Attribute Value") +
      theme(axis.text.x=element_blank())  +
      scale_fill_manual(
        values = c(NF="#9ac262",
                   NT="#95637e",
                   SP="#e5c727",
                   SJ="#72cccd"))
  
  })
  
  
  output$functioningpairplot2 <- renderPlot({
    objective <- mbtis %>%
      group_by(function_pair) %>%
      summarize(
        mean_speechiness = mean(speechiness, na.rm = TRUE),
        mean_acousticness = mean(acousticness, na.rm = TRUE),
        mean_instrumentalness = mean(instrumentalness, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = starts_with("mean"), 
                   names_to = "attribute", 
                   values_to = "mean_value")
    
    ggplot(objective, aes(x = function_pair, y = mean_value, fill = function_pair)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ attribute, scales = "free_y") +
      labs(title = "Mean Objective Music Attributes by MBTI Functioning Pair", 
           x = "Functioning Pair", 
           y = "Mean Attribute Value") + 
      theme(axis.text.x=element_blank()) +
      scale_fill_manual(
        values = c(NF="#9ac262",
                   NT="#95637e",
                   SP="#e5c727",
                   SJ="#72cccd"))
    
  })
  
  output$functioningpair_text <- renderUI({
    HTML("<p>First, looking at the mean valence again on the graph on the right, we see that SJ has songs with lowest mean valence followed closely by NF, then NT has slightly higher mean valence and SP clearly has higher mean valence than the rest. The two graphs on the left showcase respectively mean danceability of the songs, which means how suitable the songs are for dancing with higher value being more suitable, and mean energy of the songs, which means the perceived energeticness of the song with higher value being more energetic such as louder and/or faster. Surprisingly, for these two graphs there is the exact same pattern for the four function pairs, with least to greatest being SJ, NF, NT, and SP, and even with similar gaps with SJ and NF being closer, NT being slightly higher, and SP being higher than NT.
</p>")
  })
  
  
  #radar chart page output--------------------------------------------------------------------------------

  
  output$radarChart <- renderPlot({
    attributes <- names(mbtis_means)[-1]
    min_values <- sapply(mbtis_means[attributes], min)
    names(min_values) <- attributes
    max_values <- sapply(mbtis_means[attributes], max)
    names(max_values) <- attributes
    
    
    mbti_type <- input$selectedMBTI
    mbti_data <- mbtis_means %>% filter(mbti == mbti_type)
    radar_data <- rbind(Max = max_values, Min = min_values, mbti_data[-1])
    type_color <- mbti_colors[mbti_type]
    fill_color <- adjustcolor(type_color, alpha.f = 0.3)
    
    radarchart(radar_data, 
               pcol = type_color, 
               pfcol = fill_color, 
               plwd = 2, 
               cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, 20, 5), cglwd = 0.8, 
               title = mbti_type,
               vlcex = 0.8)
  })
  
  output$mbtiImage <- renderUI({
    selected_mbti <- input$selectedMBTI
    img_url <- mbti_images[selected_mbti]
    if (!is.null(img_url)) {
      tags$img(src = img_url, alt = selected_mbti, width = "40%")
    }
  })
  
  output$mbtiName <- renderText({
    selected_mbti <- input$selectedMBTI
    desc <- mbti_name[selected_mbti]
    if (!is.null(desc)) {
      paste(selected_mbti, ":", desc)
    }
  })
  
  
  output$radarcharttext <- renderUI({
    HTML("<p>By optimizing Euclidean distance between mbti data and the mean music metadata from spotify library, we found the top three artists and songs that each mbti might find gravitated to. Young adults who find both Spotify Wrapped and MBTI interesting might use this to find songs that they find interesting.</p>")
  })

  #recommender output--------------------------------------------------------------------------------
  
  output$recommendertable1 <- renderDataTable({
    merged_data
  })
  
  
  output$recommendertable2 <- renderDataTable({
    merged_data_copy
  })
  
  output$recommendertext <- renderUI({
    HTML("<p>By optimizing Euclidean distance between mbti data and the mean music metadata from spotify library, we found the top three artists and songs that each mbti might find gravitated to. Young adults who find both Spotify Wrapped and MBTI interesting might use this to find songs that they find interesting.</p>")
  })
  
  #conclusion--------------------------------------------------------------------------------
  
  output$conclusiontext <- renderUI({
    HTML("<h3>Finding</h3>
         <p>The results revealed to us that there is a correlation between music taste and how people view and interact with the world. For example, extroverted people derive satisfaction from external stimuli, which corresponds to songs with higher energy and danceability. In contrast, introverted people derive satisfaction from introspections, which correspond to songs with instrumental and calm elements.</p>
         <p>Moreover, people with judging (J) and intuition (N) preferences tend to plan and think more. People report choosing to listen to sad music more often when they are alone, when they are in emotional distress or feeling lonely, when they are in reflective or introspective moods (Taruffi and Koelsch, 2014), which might explain their preference to songs with lower valence. 
The diverse distribution of the radar charts also reveals that people with different personality types have different music preferences.</p>
         <p>However, our theory has many limitations. The dataset that we worked on are playlists created by people who self-identified their MBTI types, which is susceptible to stereotypes and popular culture. At the same time, the mean music metadata are aggregate values of 300+ individual playlists, meaning that individual preferences may vary. Music taste is also a nuance reflection of one's personality, emotions, and experiences. Confounding variables like culture, identity, and age also contribute to the human diversity of music taste. Finally, correlations do not imply causation, so these observations should be interpreted cautiously.</p>
         <h4>Future Ideas</h4>
         <p>If we can obtain a less biased database, we can generate a more accurate model that has the potential to calculate the mean music metadata of each country based on the proportion of mbti personalities. The reason for doing this is that we are also curious why certain kinds of music get popular in one country, but not in another one. Maybe American Pop songs are more danceable than Chinese Pop because the American public are more extroverted than the Chinese public? This might be fun to explore.</p>
         <h4>Summary</h4>
         <p>We found noticeable patterns that correlate people’s personality with with their music preference.</p>")
  })
  
  
}

#-----------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)


