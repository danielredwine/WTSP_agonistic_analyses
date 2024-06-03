# Daniel Redwine
# Last edited: 3 Jun 24

# Clear environment
rm(list = ls())

# Load packages, unfortunately I haven't used many of these packages before
library(tidyverse)
library(flextable)
library(GGally)
library(ggraph)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)

# Load datasets, we'll use both the resight and the agonistic data
# resight data is any individual who was a resight in a video
resight_data <- read.csv("data/WTSP_video_resight_data.csv")

# For these networks the nodes will be the individual WTSP
# Vertices will be when WTSP visit the same platform on the same day
# Adjacency matrix, how many times one node is adjacent to another
# To make this we only need two factors, SampleID and Recording_Session
# Recording_session is date + platform
resight_data <- resight_data %>%
  dplyr::select(Recording_Session, SampleID)

resight_amx <- crossprod(table(resight_data[1:2])) # creates the adjacency matrix
diag(resight_amx) <- 0 # Turns the diagonal to zero, so an individual doesn't vertex to itself
resight_df <- as.data.frame(resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
resight_dfm <- quanteda::as.dfm(resight_df)
# create feature co-occurrence matrix
resight_fcm <- quanteda::fcm(resight_dfm, tri = F)

# Network Analysis of both sites together
quanteda.textplots::textplot_network(
  x = resight_fcm, # a fcm or dfm object
  min_freq = 0.5, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = resight_dfm %>%
    # convert the dfm object to a data frame
    quanteda::convert(to = "data.frame") %>% 
    # exclude the 'doc_id' column
    dplyr::select(-doc_id) %>%
    # calculate the sum of row values for each row
    rowSums() %>%
    # apply the natural logarithm to the resulting sums
    log(),
  vertex_color = "#4D4D4D", # color of vertices (default = "#4D4D4D")
  vertex_size = 2 # size of vertices (default = 2)
)

# I think I want to separate the sites to make it more legible
resight_data <- read.csv("data/WTSP_video_resight_data.csv")

# filter out eastern hills platform for emerald hill
m_resight_data <- resight_data %>%
  filter(Platform != "a1")

# filter out emerald hills platforms for eastern hills
a_resight_data <- resight_data %>% 
  filter(Platform == "a1")

# Select only the two variables we need
m_resight_data <- m_resight_data %>%
  dplyr::select(Recording_Session, SampleID)

a_resight_data <- a_resight_data %>%
  dplyr::select(Recording_Session, SampleID)

# emerald hill first
m_resight_amx <- crossprod(table(m_resight_data[1:2])) # creates the adjacency matrix
diag(m_resight_amx) <- 0 # Turns the diagonal to zero, so an individual doesn't vertex to itself
m_resight_df <- as.data.frame(m_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
m_resight_dfm <- quanteda::as.dfm(m_resight_df)
# create feature co-occurrence matrix
m_resight_fcm <- quanteda::fcm(m_resight_dfm, tri = F)

# Network Analysis of both sites together
quanteda.textplots::textplot_network(
  x = m_resight_fcm, # a fcm or dfm object
  min_freq = 0.4, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = m_resight_dfm %>%
    # convert the dfm object to a data frame
    quanteda::convert(to = "data.frame") %>% 
    # exclude the 'doc_id' column
    dplyr::select(-doc_id) %>%
    # calculate the sum of row values for each row
    rowSums() %>%
    # apply the natural logarithm to the resulting sums
    log(),
  vertex_color = "#4D4D4D", # color of vertices (default = "#4D4D4D")
  vertex_size = 2 # size of vertices (default = 2)
)

# Then Eastern Hills
a_resight_amx <- crossprod(table(a_resight_data[1:2])) # creates the adjacency matrix
diag(a_resight_amx) <- 0 # Turns the diagonal to zero, so an individual doesn't vertex to itself
a_resight_df <- as.data.frame(a_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
a_resight_dfm <- quanteda::as.dfm(a_resight_df)
# create feature co-occurrence matrix
a_resight_fcm <- quanteda::fcm(a_resight_dfm, tri = F)

# Network Analysis of both sites together
quanteda.textplots::textplot_network(
  x = a_resight_fcm, # a fcm or dfm object
  min_freq = 0.4, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = a_resight_dfm %>%
    # convert the dfm object to a data frame
    quanteda::convert(to = "data.frame") %>% 
    # exclude the 'doc_id' column
    dplyr::select(-doc_id) %>%
    # calculate the sum of row values for each row
    rowSums() %>%
    # apply the natural logarithm to the resulting sums
    log(),
  vertex_color = "#4D4D4D", # color of vertices (default = "#4D4D4D")
  vertex_size = 2 # size of vertices (default = 2)
)
