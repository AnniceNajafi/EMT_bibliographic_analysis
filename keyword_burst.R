#' Authors@R: person("Annice", "Najafi", email = "annicenajafi27@gmail.com")
#' The following program can be used to generate figure 4 subplots
#' 
#' 
#' Spring 2025, 
#' California

#keyword burst analysis, use this to generate the figure 4

library(dplyr)
library(tidyr)
library(wordcloud)
library(wordcloud2)
library(ggplot2)


df_cleaned <- df %>%
  mutate(Keywords = strsplit(Keywords, ";")) %>% 
  unnest(Keywords) %>% 
  mutate(Keywords = standardize_keywords(trimws(Keywords)))  




keywords_data <- df_cleaned %>%
  group_by(Keywords, Year) %>%
  summarise(Total.Citations = sum(Times.Cited), .groups = "drop")


keyword_citation_growth <- keywords_data %>%
  group_by(Keywords) %>%
  arrange(Year) %>%
  mutate(
    Previous.Citations = lag(Total.Citations, default = 0),  
    Growth = Total.Citations - Previous.Citations 
  ) %>%
  ungroup()


burst_threshold <- 100  
keyword_citation_growth <- keyword_citation_growth %>%
  mutate(Burst = Growth >= burst_threshold)  


top_keywords <- keyword_citation_growth %>%
  group_by(Keywords) %>%
  summarise(Total.Citations.Sum = sum(Total.Citations)) %>%
  arrange(desc(Total.Citations.Sum)) %>%
  slice_head(n = 50)  




top_keywords <- top_keywords %>% 
  filter(!Keywords %in% c("epithelial-mesenchymal transition",
                          "emt",  "epithelial mesenchymal transition",
                          "biomarker", "epithelial-to-mesenchymal transition"))


top_keywords <- top_keywords %>%
  mutate(Keywords = gsub("\\bmirna\\b|\\bmicrornas\\b|\\bmicrorna\\b", "mirna", Keywords, ignore.case = TRUE))


top_keywords <- top_keywords %>%
  mutate(Keywords = gsub("\\bcancer stem cell\\b|\\bcancer stem cells\\b", "cancer stem cell", Keywords, ignore.case = TRUE))

top_keywords <- top_keywords %>%
  mutate(Keywords = gsub("\\bcell migration\\b|\\bmigration\\b", "cell migration", Keywords, ignore.case = TRUE))




year_range <- 1991:2024


burst_lines <- keyword_citation_growth %>%
  filter(Year >= min(year_range) & Year <= max(year_range)) %>% 
  mutate(Burst = ifelse(Burst, "Burst", "No Burst")) 


all_years <- expand.grid(
  Year = year_range,
  Keywords = unique(burst_lines$Keywords)
)


burst_full <- all_years %>%
  left_join(burst_lines, by = c("Year", "Keywords")) %>%
  mutate(Burst = replace_na(Burst, "No Burst")) 


burst_full <- burst_full %>% filter(Keywords %in% top_keywords$Keywords)


ggplot(burst_full, aes(x = Year, y = Keywords, color = Burst)) +
  geom_line(aes(group = Keywords), size = 4) + 
  scale_color_manual(values = c("Burst" = "#433878", "No Burst" = "#FAF7F0")) +  
  scale_x_continuous(breaks = seq(1991, 2024, by = 1)) +  
  labs(
    title = "Keyword Burst Timeline",
    x = "Year",
    y = "Keywords",
    color = "Burst Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "top")






#word cloud


all_keywords <- paste(df_cleaned$Keywords, collapse = "; ")

keywords_list <- unlist(strsplit(all_keywords, ";"))
keywords_list <- trimws(keywords_list)  


keywords_list <- tolower(keywords_list)
keywords_list[keywords_list != "na"]->keywords_list

word_freq <- table(keywords_list)


word_freq <- sort(word_freq, decreasing = TRUE)


wordcloud(
  words = names(word_freq), 
  freq = word_freq, 
  min.freq = 1,         
  max.words = 100,       
  random.order = FALSE,  
  colors = brewer.pal(8, "Dark2")  
)

    
    
    
    
    
    
    
  )
