#' Authors@R: person("Annice", "Najafi", email = "annicenajafi27@gmail.com")
#' The following program can be used to find influential EMT authors
#' 
#' Spring 2025, 
#' California


library(fmsb)
library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)

filtered_data <- df %>%
  filter(Type %in% c("Article",
                     "Article; Early Access",
                     "Article; Proceedings Paper",
                     "News Item",
                     "Article; Data Paper",
                     "Article; Publication with Expression of Concern"))


author_article_count <- filtered_data %>%
  separate_rows(Author, sep = ", ") %>%  
  group_by(Author) %>%                 
  summarize(ArticleCount = n()) %>%      
  arrange(desc(ArticleCount)) 


author_article_count <- top.authors %>%
  left_join(author_article_count, by = "Author") %>% 
  mutate(ArticleCount = coalesce(ArticleCount, 0)) 



name_mapping <- c(
  "JP Thiery" = "Jean Thiery",
  "MA Nieto" = "M. Angela Nieto",
  "R Kalluri" = "Raghu Kalluri",
  "A Cano" = "Amparo Cano"
)



author_article_count$Author <- ifelse(author_article_count$Author %in% names(name_mapping), name_mapping[author_article_count$Author], author_article_count$Author)




aggregated_authors <- author_article_count %>%
  group_by(Author) %>%
  summarize(
    ArticleCount = sum(ArticleCount),
    TotalCitations = sum(TotalCitations)           
  ) %>%
  arrange(desc(TotalCitations))   



names_list <- aggregated_authors$Author  





name_mapping <- c(
  "JP Thiery" = "Jean Thiery",
  "MA Nieto" = "M. Angela Nieto",
  "R Kalluri" = "Raghu Kalluri",
  "A Cano" = "Amparo Cano"
)



df_changed <- df %>%
  separate_rows(Author, sep = ", ")

df_changed$Author <- ifelse(df_changed$Author %in% names(name_mapping), name_mapping[df_changed$Author], df_changed$Author)





compute_h_index <- function(citations) {
  sorted_citations <- sort(citations, decreasing = TRUE)  
  h_index <- sum(sorted_citations >= seq_along(sorted_citations)) 
  return(h_index)
}


citations_h_index <- df_changed %>%
  separate_rows(Author, sep = ", ")%>%
  filter(Author %in% names_list)%>%
  
  group_by(Author) %>%
  summarise(h_index = compute_h_index(Times.Cited))






aggregated_authors$H_Index <- citations_h_index$h_index



n <- nrow(aggregated_authors)


pairwise_matrix_TotalCitations <- matrix(1, nrow = n, ncol = n)


for (i in 1:n) {
  for (j in 1:n) {
    pairwise_matrix_TotalCitations[i, j] <- aggregated_authors$TotalCitations[i] / aggregated_authors$TotalCitations[j]
  }
}



pairwise_matrix_H <- matrix(1, nrow = n, ncol = n)


for (i in 1:n) {
  for (j in 1:n) {
    pairwise_matrix_H[i, j] <- aggregated_authors$H_Index[i] / aggregated_authors$H_Index[j]
  }
}



pairwise_matrix_ArticleCount <- matrix(1, nrow = n, ncol = n)


for (i in 1:n) {
  for (j in 1:n) {
    pairwise_matrix_ArticleCount[i, j] <- aggregated_authors$ArticleCount[i] / aggregated_authors$ArticleCount[j]
  }
}



colnames(pairwise_matrix_TotalCitations)<-aggregated_authors$Author
rownames(pairwise_matrix_TotalCitations)<-aggregated_authors$Author

colnames(pairwise_matrix_ArticleCount)<-aggregated_authors$Author
rownames(pairwise_matrix_ArticleCount)<-aggregated_authors$Author

colnames(pairwise_matrix_H)<-aggregated_authors$Author
rownames(pairwise_matrix_H)<-aggregated_authors$Author






criterion.mat <- matrix(c(1, 1/2, 1/9, 2, 1, 1/5, 9, 5, 1), nrow=3)

# criterion.mat <- matrix(c(1.0000000, 0.3988487, 0.7227796, 
#                           2.507216, 1,  0.5518261, 
#                           0.7227796, 1.812165, 1), nrow=3)

colnames(criterion.mat)<-c("H_Index", "Citations",	"ArticleCounts")
rownames(criterion.mat)<-c("H_Index", "Citations",	"ArticleCounts")

pairwise_matrix_ArticleCount[is.nan(pairwise_matrix_ArticleCount)] <- 0
pairwise_matrix_ArticleCount[(pairwise_matrix_ArticleCount)=="Inf"] <- 10^10
criterion.mat->A


comparing.competitors<-list(pairwise_matrix_H, pairwise_matrix_TotalCitations, pairwise_matrix_ArticleCount)

results <- apply.AHP(criterion.mat, list(pairwise_matrix_H, pairwise_matrix_TotalCitations, pairwise_matrix_ArticleCount))



sort(results[[4]])

results[[3]][,colnames(results[[3]]) %in% c("Jing Yang","M. Angela Nieto","Douglas Hanahan","Robert Weinberg","Alain Puisieux")]


plot.spider(results[[3]][,colnames(results[[3]]) %in% c("Jing Yang","M. Angela Nieto","Douglas Hanahan","Robert Weinberg","Alain Puisieux")])


plot.AHP.decision.tree(A, comparing.competitors)











