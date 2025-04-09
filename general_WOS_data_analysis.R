#' Authors@R: person("Annice", "Najafi", email = "annicenajafi27@gmail.com")
#' The following program can be used to generate figure 1 subplots
#' 
#' 
#' Spring 2025, 
#' California


###Load relevant libraries
library(dplyr)
library(stringr)
library(countrycode)
library(SnowballC)
library(tidyr)



df <- read.csv("~/Downloads/EMT_bib_df_converted.csv")


articles <- (df %>% filter(Type %in% c("Article",
                                       "Article; Early Access", "Article; Proceedings Paper",
                                       "News Item", "Article; Data Paper",
                                       "Article; Publication with Expression of Concern")))



reviews <-df %>% filter(Type %in% c("Review", "Review; Book Chapter","Review; Early Access"))




df <- df %>%
  mutate(Category = case_when(
    Type %in% c("Article", "Article; Early Access", "Article; Proceedings Paper",
                "News Item", "Article; Data Paper", "Article; Publication with Expression of Concern") ~ "Article",
    Type %in% c("Review", "Review; Book Chapter", "Review; Early Access") ~ "Review",
    TRUE ~ "Other" 
  ))


df_filtered <- df %>%
  filter(Category %in% c("Article", "Review"))


ggplot(df_filtered, aes(x = Year, fill = Category)) +
  geom_histogram(binwidth = 1, color = "#3B1E54", position = "stack") +
  scale_fill_manual(values = c("Article" = "#9B7EBD", "Review" = "#5B3F8A")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  ) +
  labs(y = "Number of publications", 
       title = "Epithelial-Mesenchymal Transition\n Publications", 
       tag = "A")




df_filtered <- df %>%
  filter(Year >= 2020 & Year <= 2024)


top_journals <- df_filtered %>%
  count(Journal, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Journal)


df_top <- df_filtered %>%
  filter(Journal %in% top_journals)


df_ranked <- df_top %>%
  group_by(Year) %>%
  count(Journal, name = "Frequency") %>%
  arrange(Year, desc(Frequency)) %>%
  mutate(Rank = rank(-Frequency)) %>%
  filter(Rank <= 10)



ggplot(df_ranked, aes(x = Year, y = Rank, group = Journal, color = Journal)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape=8, stroke=2) +
  scale_y_reverse(breaks = 1:10) +
  labs(title = "Rank Plot of Top 10 Most Popular Journals (2020-2024)",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  scale_color_brewer(palette = "Set3") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 10)
  )



top_cited_papers <- df[order(-df$Times.Cited), ][1:500, ]


top_authors <- top_cited_papers$author


all_authors <-unlist(strsplit(top_cited_papers$Author, ", "))

author_counts <- table(all_authors)


sorted_author_counts <- sort(author_counts, decreasing = TRUE)

head(sorted_author_counts, 20)





df$WordBeforeCancer <- str_extract(df$Title, "\\b(\\w+)\\s+cancer")

df$WordBeforeCancer <- tolower(df$WordBeforeCancer)
cancer_types_sorted <- sort(table(df$WordBeforeCancer), decreasing = TRUE)

cancer_types_counts <- data.frame(cancer_type = c("breast cancer",
                                                  "lung cancer",
                                                  "colorectal cancer",
                                                  "gastric cancer",
                                                  "prostate cancer",
                                                  "pancreatic cancer",
                                                  "ovarian cancer",
                                                  "bladder cancer",
                                                  "colon cancer",
                                                  "cervical cancer",
                                                  "liver cancer",
                                                  "thyroid cancer",
                                                  "endometrial cancer",
                                                  "oral cancer",
                                                  "esophageal cancer",
                                                  "neck cancer",
                                                  "gallbladder cancer",
                                                  "gastrointestinal cancer",
                                                  "renal cancer",
                                                  "rectal cancer"
), 
counts = c(
  cancer_types_sorted["breast cancer"],
  cancer_types_sorted["lung cancer"],
  cancer_types_sorted["colorectal cancer"],
  cancer_types_sorted["gastric cancer"],
  cancer_types_sorted["prostate cancer"],
  cancer_types_sorted["pancreatic cancer"],
  cancer_types_sorted["ovarian cancer"],
  cancer_types_sorted["bladder cancer"],
  cancer_types_sorted["colon cancer"],
  cancer_types_sorted["cervical cancer"],
  cancer_types_sorted["liver cancer"],
  cancer_types_sorted["thyroid cancer"],
  cancer_types_sorted["endometrial cancer"],
  cancer_types_sorted["oral cancer"],
  cancer_types_sorted["esophageal cancer"],
  cancer_types_sorted["neck cancer"],
  cancer_types_sorted["gallbladder cancer"],
  cancer_types_sorted["gastrointestinal cancer"],
  cancer_types_sorted["renal cancer"],
  cancer_types_sorted["rectal cancer"]))




ggplot(cancer_types_counts, aes(x = reorder(cancer_type, counts), y = counts, fill = counts)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  scale_fill_gradient(low = "#D4BEE4", high = "#3B1E54") +  
  geom_text(aes(label = counts), hjust = -0.2, color = "black", size = 3.5) + 
  labs(title = "",
       x = "Cancer Type", 
       y = "Count", tag="A", fill="EMT-related") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  ) 


wos_data <- read.csv("~/Downloads/WOS_cancer_articles.csv")
tolower(wos_data$Cancer)->wos_data$Cancer
paste0(wos_data$Cancer, " cancer")->wos_data$cancer_type




ggplot(wos_data, aes(x = reorder(cancer_type, Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  scale_fill_gradient(low = "#D4BEE4", high = "#3B1E54") +  
  geom_text(aes(label = Count), hjust = -0.2, color = "black", size = 3.5) + 
  labs(title = "",
       x = "Cancer Type", 
       y = "Count",
       fill = "WoS",tag="B") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text = element_text(size = 15),
    text = element_text(size = 18)
  ) 





cancer_data <- cancer_types_counts %>%
  mutate(Proportion = counts / wos_data$Count)


average_proportion <- mean(cancer_data$Proportion)


cancer_data <- cancer_data %>%
  mutate(Status = ifelse(Proportion > average_proportion, "Overstudied", "Understudied"))


print(cancer_data)

cancer_data$cancer_type <- gsub(" cancer", "", cancer_data$cancer_type)



ggplot(data = cancer_data) +
  geom_rect(aes(xmin = 0.0004232916, xmax =0.0004731968, ymin = 0, ymax = 0.0075), 
            fill = "#F5F5F5", alpha = 0.3)+
  geom_point(aes(x = reorder(cancer_type, Proportion), y = Proportion, color = Status))+
  #geom_bar(aes(x = reorder(cancer_type, Proportion), y = Proportion), fill = Status, stat = "identity") +
  geom_hline(yintercept = average_proportion, linewidth=1, color = "#00224D") +
  coord_polar() +
  labs(title = "EMT Research Focus by Cancer Type",
       x = "Cancer Type",
       y = "") +
  theme_minimal()+
  scale_color_manual(values=c('#D96098','#BEAEE2'))+
  theme(
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line =  element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20),
    #axis.text = element_text(size = 15),
    #text = element_text(size = 18)
  ) 






xmin_limit <- which(levels(reorder(cancer_data$cancer_type, cancer_data$Proportion)) == "prostate") 
xmax_limit <- which(levels(reorder(cancer_data$cancer_type, cancer_data$Proportion)) == "renal") 
xend_limit <- which(levels(reorder(cancer_data$cancer_type, cancer_data$Proportion)) == "gastric")
ggplot(data = cancer_data) +
 
  geom_rect(aes(
    xmin = xmin_limit, 
    xmax = xmax_limit, 
    ymin = -Inf, 
    ymax = 0.0075
  ), fill = "#78B3CE", alpha = 0.01) +
  
  geom_point(aes(x = reorder(cancer_type, Proportion), y = Proportion, color = Status)) +
  geom_hline(yintercept = average_proportion, linewidth = 1, color = "#00224D") +
  coord_polar() +
  labs(
    title = "EMT Research Focus by Cancer Type",
    x = "Cancer Type",
    y = ""
  ) +
  theme_minimal() +
  
  scale_color_manual(values = c('#D96098', '#BEAEE2')) +
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20)
  )





















