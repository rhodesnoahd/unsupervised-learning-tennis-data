# MATH 4323 Group Project
# Noah Rhodes, add others' names

df <- read.csv("tennis-data.csv")

############################ DATA PREPROCESSING ############################

# remove unnecessary columns
unnecessary_columns <- c("WTA", "Tournament", "Wsets", "Lsets", "Date", "Surface", 
                         "Best.of", "Winner", "Loser")
df <- df[,!names(df) %in% unnecessary_columns]

# loop through each column and calculate proportion of missing data in each
sparse_columns = c()
for (i in 1:ncol(df))
{
  proportion_missing <- sum(is.na(df[i])) / nrow(df)
  
  if (proportion_missing > 0.10)
  {
    cat("Column ", i, " is missing ", proportion_missing*100, "% data\n")
    sparse_columns <- append(sparse_columns,i)
  }
}
# drop sparse_columns
df <- df[,-sparse_columns]

# drop rows with missing data
library("tidyr")
df <- df %>% drop_na()

# The data is modified below such that a 1 indicates the winner having the
# better column value. 

# For example, if the WRank (rank of the match winner)
# is < LRank (rank of the match loser) then create a new column Rank
# with a 1 for this row.

# Another example, if B356W (Bet356 betting odds of match winner) is < B365L
# then create a new column B365 with a 1 for this row.

# Better Rank is lower value
# create new Rank column where {WRank < LRank: 1, WRank >= LRank: 0}
df$Rank = ifelse(df$WRank < df$LRank, 1, 0)
# drop WRank & LRank
df <- df[,!names(df) %in% c("WRank", "LRank")]

# Better Pts is higher value
# create new Pts column where {WPts > LPts: 1, WPts <= LPts: 0}
df$Pts = ifelse(df$WPts > df$LPts, 1, 0)
# drop WPts & LPts
df <- df[,!names(df) %in% c("WPts", "LPts")]

# create new Set1 column where {W1 > L1: 1, W1 <= L1: 0}
df$Set1 = ifelse(df$W1 > df$L1, 1, 0)
# drop W1 & L1
df <- df[,!names(df) %in% c("W1", "L1")]

# create new Set2 column where {W2 > L2: 1, W2 <= L2: 0}
df$Set2 = ifelse(df$W2 > df$L2, 1, 0)
# drop W2 & L2
df <- df[,!names(df) %in% c("W2", "L2")]

# Lower odds indicate better player
# create new B365 column where {B356W < B365L: 1, B365W >= B365L: 0}
df$B365 = ifelse(df$B365W < df$B365L, 1, 0)
# drop B365W & B365L
df <- df[,!names(df) %in% c("B365W", "B365L")]

# Lower odds indicate better player
# create new PS column where {PSW < PSL: 1, PSW >= PSL: 0}
df$PS = ifelse(df$PSW < df$PSL, 1, 0)
# drop PSW & PSL
df <- df[,!names(df) %in% c("PSW", "PSL")]

# Lower odds indicate better player
# create new Max column where {MaxW < MaxL: 1, MaxW >= MaxL: 0}
df$Max = ifelse(df$MaxW < df$MaxL, 1, 0)
# drop MaxW & MaxL
df <- df[,!names(df) %in% c("MaxW", "MaxL")]

# Lower odds indicate better player
# create new Avg column where {AvgW < AvgL: 1, AvgW >= AvgL: 0}
df$Avg = ifelse(df$AvgW < df$AvgL, 1, 0)
# drop AvgW & AvgL
df <- df[,!names(df) %in% c("AvgW", "AvgL")]

# Create dummy variables
# install.packages("fastDummies")
library("fastDummies")
df <- dummy_cols(df, select_columns=c("Location", "Tier", "Court", 
                                      "Round", "Comment"),
                 remove_selected_columns=T)

########################## END DATA PREPROCESSING ##########################

################################ CLUSTERING ################################

# for reproducibility
RNGkind(sample.kind = "Rounding")

library(factoextra)
df = data.frame(df)

# K-Means cluster validation using gap statistic
set.seed(2)
km.out <- eclust(df,                     
                 FUNcluster = "kmeans", 
                 nstart=100,
                 nboot=100)
km.out
# cluster validation suggests k=10

# K-Means cluster validation using silhouette coefficient
set.seed(2)
k.max <- 10
silh.coef <- numeric(k.max)
for (k in 2:10)
{
  silh.coef[k] <- eclust(df,
                         FUNcluster = "kmeans",
                         k=k,
                         graph=0,
                         nstart=100)$silinfo$avg.width
}
plot(silh.coef, type="b", pch=19, col=4)
which.max(silh.coef)
# cluster validation suggests k=9

# K-Means clustering with k=9
km.out.k9 <- eclust(df,                     
                    FUNcluster = "kmeans",
                    k=9,
                    nstart=100)
km.out.k9


# Hierarchical clustering
hc.complete <- hclust(dist(df),
                      method = "complete")
plot(hc.complete)

############################## END CLUSTERING ##############################

############################## INTERPRETATION ##############################

# slices of each cluster assignment

# K-Means clustering solution with k=10
df$k10.clust.id <- km.out$cluster
k10.cluster_1 <- df[df$k10.clust.id == 1, ]
k10.cluster_2 <- df[df$k10.clust.id == 2, ]
k10.cluster_3 <- df[df$k10.clust.id == 3, ]
k10.cluster_4 <- df[df$k10.clust.id == 4, ]
k10.cluster_5 <- df[df$k10.clust.id == 5, ]
k10.cluster_6 <- df[df$k10.clust.id == 6, ]
k10.cluster_7 <- df[df$k10.clust.id == 7, ]
k10.cluster_8 <- df[df$k10.clust.id == 8, ]
k10.cluster_9 <- df[df$k10.clust.id == 9, ]
k10.cluster_10 <- df[df$k10.clust.id == 10, ]

# K-Means clustering solution with k=3
df$k9.clust.id <- km.out.k9$cluster
k9.cluster_1 <- df[df$k9.clust.id == 1, ]
k9.cluster_2 <- df[df$k9.clust.id == 2, ]
k9.cluster_3 <- df[df$k9.clust.id == 3, ]
k9.cluster_4 <- df[df$k9.clust.id == 4, ]
k9.cluster_5 <- df[df$k9.clust.id == 5, ]
k9.cluster_6 <- df[df$k9.clust.id == 6, ]
k9.cluster_7 <- df[df$k9.clust.id == 7, ]
k9.cluster_8 <- df[df$k9.clust.id == 8, ]
k9.cluster_9 <- df[df$k9.clust.id == 9, ]

# Hierarchical clustering solution with k=2
df$k2.hclust.id <- cutree(hc.complete, k=2)
k2.hcluster_1 <- df[df$k2.hclust.id == 1, ]
k2.hcluster_2 <- df[df$k2.hclust.id == 2, ]

v1 <- sum(k2.hcluster_1$B365)/nrow(k2.hcluster_1)*100
v2 <- sum(k2.hcluster_1$PS)/nrow(k2.hcluster_1)*100
v3 <- sum(k2.hcluster_1$Max)/nrow(k2.hcluster_1)*100
v4 <- sum(k2.hcluster_1$Avg)/nrow(k2.hcluster_1)*100
(v1+v2+v3+v4)/4

w1 <- sum(k2.hcluster_2$B365)/nrow(k2.hcluster_1)*100
w2 <- sum(k2.hcluster_2$PS)/nrow(k2.hcluster_1)*100
w3 <- sum(k2.hcluster_2$Max)/nrow(k2.hcluster_1)*100
w4 <- sum(k2.hcluster_2$Avg)/nrow(k2.hcluster_1)*100
(w1+w2+w3+w4)/4
