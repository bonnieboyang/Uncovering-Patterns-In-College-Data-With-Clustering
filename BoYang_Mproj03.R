library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggdendro)

set.seed(100)

rm(list = ls()) # empty the environment

# prepare the data
colleges = read.delim('colleges.tsv', header = TRUE, sep = '\t')
colleges_dict = read.delim('colleges_data_dictionary.tsv', header = TRUE, sep = '\t')
str(colleges)
str(colleges_dict)

# select out the data subset to do k-means 
college_features = colleges %>% 
          select(institution_name, first_gen_share, 
                 poverty_rate, family_income_median,
                 median_earnings, top_ten) %>% 
          na.omit() %>% 
          distinct()

# run k-means clustering
kmeans_cluster_k3 = kmeans(select(college_features, -institution_name, -top_ten), 3)

# check what attributes are in the k-means object
attributes(kmeans_cluster_k3)

# append the cluster assignment onto the dataset
college_features = college_features %>% 
                       mutate(cluster = kmeans_cluster_k3$cluster)

# centers
kmeans_cluster_k3$centers

# Subset the colleges_features dataset on the cluster with the lowest family_income_median
grant_candidates_k3 = college_features %>% filter(cluster == 2)
# grant_candidates = subset(college_features, cluster == 2)

# how many universities are in the cluster of grant receivers?
nrow(grant_candidates_k3)

# Redo the k-means analysis above, create 5 clusters
# recover to the original data
college_features$cluster = NULL

#run k-means clustering with k = 5
kmeans_cluster_k5 = kmeans(select(college_features, -institution_name, -top_ten), 5)

# append the cluster assignment onto the dataset
college_features = college_features %>% mutate(cluster = kmeans_cluster_k5$cluster)

# centers
kmeans_cluster_k5$centers

# Subset the colleges_features dataset on the cluster with the lowest family_income_median
grant_candidates_k5 = college_features %>% filter(cluster == 5)

# how many universities are in the cluster of grant receivers?
nrow(grant_candidates_k5)

# compare the median and range of family_income_median of these universities for k-means clustering with k=3 and k=5
grant_candidates_k3_summary = grant_candidates_k3 %>% summarise(k = nrow(kmeans_cluster_k3$centers), 
                                                                median = median(family_income_median), 
                                                                min = min(family_income_median), 
                                                                max = max(family_income_median))

grant_candidates_k5_summary = grant_candidates_k5 %>% summarise(k = nrow(kmeans_cluster_k5$centers), 
                                                                median = median(family_income_median), 
                                                                min = min(family_income_median), 
                                                                max = max(family_income_median))

# combind the summary result
grant_candidates_compare = rbind(grant_candidates_k3_summary, grant_candidates_k5_summary)


# hierarchical clustering
# remove universities that do not have SAT admission criteria
# also use family_income_median and median_earnings to subset the data
grant_colleges = 
  colleges %>% 
  filter((!is.na(sat_verbal_quartile_1) & family_income_median < 40000 & median_earnings < 30000)) 

# select out top ten schools
top_ten_schools = colleges %>% 
  filter(top_ten == TRUE)

# create the subset for hierarchical clustering 
heir_analysis_data = rbind(grant_colleges, top_ten_schools)

# comparing major percentages using heir_analysis_data dataset. 
# Which universities are the most different from the top ten schools in terms of majors?
major_perc = heir_analysis_data %>% 
        select(institution_name, top_ten, ends_with("_major_perc")) %>% 
        na.omit()

# compute the euclidean distance
euclidean = dist(select(major_perc, -institution_name, -top_ten),
                 method = "euclidean")

# do hierarchical clustering
hier = hclust(euclidean)

# replace labels with institution name
hier$labels = major_perc$institution_name

# plot dendrogram
ggdendrogram(hier, rotate = TRUE, size = 2)

# extract the dendrogram data
dendro_data = dendro_data(hier)

# plot fancy dendrogram
dendro_data$labels = unique(merge(dendro_data$labels, 
                                  select(college_features, institution_name, top_ten),
                                  by.x = "label",
                                  by.y = "institution_name",
                                  all.x = TRUE))

ggplot(segment(dendro_data)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(dendro_data),
            aes(label = label, x = x, y = 0,
                hjust = 0, color = top_ten),
            size = 2) +
  coord_flip() +
  scale_y_reverse(expand = c(0.25, 0)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Which universities are the most different from the top ten schools in terms of majors?
labels = dendro_data$labels
head(labels[order(labels$x),], 9)
#head(labels[order(-labels$x),], 9) # sort by descending

# use ggpairs to compare the grantee schools to the top ten schools
ggpairs(heir_analysis_data, 
        lower = list(mapping = aes(color=top_ten, alpha = 0.2)),
        diag = list(mapping = aes(fill = top_ten,  color = top_ten, aplha = 0.5)),
        upper = list(mapping = aes(group = top_ten)),
        columns = c("first_gen_share", "poverty_rate", 
                    "family_income_median", "median_earnings"))
