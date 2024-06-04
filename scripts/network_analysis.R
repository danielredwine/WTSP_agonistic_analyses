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
diag(resight_amx) <- 0 # Turns the diagonal to zero
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
diag(m_resight_amx) <- 0 # Turns the diagonal to zero
m_resight_df <- as.data.frame(m_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
m_resight_dfm <- quanteda::as.dfm(m_resight_df)
# create feature co-occurrence matrix
m_resight_fcm <- quanteda::fcm(m_resight_dfm, tri = F)

# Network Analysis Emerald Hill
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
diag(a_resight_amx) <- 0 # Turns the diagonal to zero
a_resight_df <- as.data.frame(a_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
a_resight_dfm <- quanteda::as.dfm(a_resight_df)
# create feature co-occurrence matrix
a_resight_fcm <- quanteda::fcm(a_resight_dfm, tri = F)

# Network Analysis Eastern Hills 
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

# Separate both sites and years 
resight_data <- read.csv("data/WTSP_video_resight_data.csv")

# filter out eastern hills platform for emerald hill and filter out 2024
m23_resight_data <- resight_data %>%
  filter(Platform != "a1" & Year == 2023)

# filter out emerald hills platforms for eastern hills and filter out 2024
a23_resight_data <- resight_data %>% 
  filter(Platform == "a1" & Year == 2023)

# filter out eastern hills platform for emerald hill and filter out 2023
m24_resight_data <- resight_data %>%
  filter(Platform != "a1" & Year == 2024)

# filter out emerald hills platforms for eastern hills and filter out 2023
a24_resight_data <- resight_data %>% 
  filter(Platform == "a1" & Year == 2024)

# Select only the two variables we need
m23_resight_data <- m23_resight_data %>%
  dplyr::select(Recording_Session, SampleID)

a23_resight_data <- a23_resight_data %>%
  dplyr::select(Recording_Session, SampleID)

m24_resight_data <- m24_resight_data %>%
  dplyr::select(Recording_Session, SampleID)

a24_resight_data <- a24_resight_data %>%
  dplyr::select(Recording_Session, SampleID)

# Emerald Hill 2023
m23_resight_amx <- crossprod(table(m23_resight_data[1:2])) # creates the adjacency matrix
diag(m23_resight_amx) <- 0 # Turns the diagonal to zero
m23_resight_df <- as.data.frame(m23_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
m23_resight_dfm <- quanteda::as.dfm(m23_resight_df)
# create feature co-occurrence matrix
m23_resight_fcm <- quanteda::fcm(m23_resight_dfm, tri = F)

# Network Analysis of Emerald Hill 2023
quanteda.textplots::textplot_network(
  x = m23_resight_fcm, # a fcm or dfm object
  min_freq = 0.4, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = m23_resight_dfm %>%
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

# Then Eastern Hills 2023
a23_resight_amx <- crossprod(table(a23_resight_data[1:2])) # creates the adjacency matrix
diag(a23_resight_amx) <- 0 # Turns the diagonal to zero
a23_resight_df <- as.data.frame(a23_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
a23_resight_dfm <- quanteda::as.dfm(a23_resight_df)
# create feature co-occurrence matrix
a23_resight_fcm <- quanteda::fcm(a23_resight_dfm, tri = F)

# Network Analysis Eastern Hills 2023
quanteda.textplots::textplot_network(
  x = a23_resight_fcm, # a fcm or dfm object
  min_freq = 0.4, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = a23_resight_dfm %>%
    # convert the dfm object to a data frame
    quanteda::convert(to = "data.frame") %>% 
    # exclude the 'doc_id' column
    dplyr::select(-doc_id) %>%
    # calculate the sum of row values for each row
    rowSums(),
    # apply the natural logarithm to the resulting sums
  vertex_color = "#4D4D4D", # color of vertices (default = "#4D4D4D")
  vertex_size = 2 # size of vertices (default = 2)
)

# Emerald Hill 2024
m24_resight_amx <- crossprod(table(m24_resight_data[1:2])) # creates the adjacency matrix
diag(m24_resight_amx) <- 0 # Turns the diagonal to zero
m24_resight_df <- as.data.frame(m24_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
m24_resight_dfm <- quanteda::as.dfm(m24_resight_df)
# create feature co-occurrence matrix
m24_resight_fcm <- quanteda::fcm(m24_resight_dfm, tri = F)

# Network Analysis of Emerald Hill 2024
quanteda.textplots::textplot_network(
  x = m24_resight_fcm, # a fcm or dfm object
  min_freq = 0.4, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = m24_resight_dfm %>%
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

# Eastern Hills 2024
a24_resight_amx <- crossprod(table(a24_resight_data[1:2])) # creates the adjacency matrix
diag(a24_resight_amx) <- 0 # Turns the diagonal to zero
a24_resight_df <- as.data.frame(a24_resight_amx) # Turns the adjacency matrix to dataframe

# Below is not my code, still working on figuring it all out
# Using package quanteda for the network analysis
# create a document feature matrix
a24_resight_dfm <- quanteda::as.dfm(a24_resight_df)
# create feature co-occurrence matrix
a24_resight_fcm <- quanteda::fcm(a24_resight_dfm, tri = F)

# Network Analysis of Eastern Hills 2024
quanteda.textplots::textplot_network(
  x = a24_resight_fcm, # a fcm or dfm object
  min_freq = 0.4, # frequency count threshold or proportion for co-occurrence frequencies
  edge_alpha = 0.5,  # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray", # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2, # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = a24_resight_dfm %>%
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