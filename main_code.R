# loading or installing packages
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(grid)){install.packages("grid")}
library(grid)
if(!require(splancs)){install.packages("splancs")}
library(splancs)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(ggrepel)){install.packages("ggrepel")}
library(ggrepel)
if(!require(readr)){install.packages("readr")}
library(readr)
if(!require(scales)){install.packages("scales")}
library(scales)


# Example 1: Statsbomb data

## Get the data using the code in the file "get_SB_event_data_WC2018.R"

source("soccerPassNetEventing.R")
soccerPassNetEventing(gameID = 7584, TeamName = "Japan", poss = T, pass_dir = T, convex = T,
                      minPass = 5, node_pos = "origin", nodeFill = "blue", edgeAlpha = 0.5,
                      label = T, shortNames = T,  maxNodeSize = 15, maxEdgeSize = 2.5,  
                      Flipx = F, field = 1)

ggsave("plots/Japan-Belgium-ver2.png", width = 15, height = 10, units = "in", device = "png")




# Example 2: Metrica Sport data

## Get the data from this repository: https://github.com/metrica-sports/sample-data

source("soccerPassNetTracking.R")
soccerPassNetTracking(gameID = "1", TeamName = "Home", pos_source = "track", node_pos = "origin",
                      context = "attacking", half = "own", field = 2,
                      pass_dir = T, minPass = 2, convex = T, 
                      label = T, shortNames = F, labelSize = 2, 
                      maxNodeSize = 12, maxEdgeSize = 1.5, edgeAlpha = 0.8)

ggsave("1-Home-attack-own-ver2.png", width = 15, height = 10, units = "in", device = "png")