#' Data munging to convert network data into a form amenable 
#' to the construction of multilayer networks using multinet.

library(tidyverse)

# First, read in the social identification network built based on
# proportional similarity in plate stylistic designs between sites 
style_all <- read_csv("BR_UNDIRECTED_edgelist_complete_.csv")

# Multinet is implement in a variant of the C language and as such
# is bound by different rules. One of those is avoiding spaces in
# the actor (or in this case archaeological site) names

# Replace all spaces, dashes, etc. with an underscore
style_all$Source <- str_replace_all(style_all$Source, c(" " = "_", "-" = "_"))
style_all$Target <- str_replace_all(style_all$Target, c(" " = "_", "-" = "_"))
   
# Decompose edge table to edge vectors                           
style_all %>%
  mutate(Layer = ifelse(Time == 1, "Style_pre", "Style_post")) %>%
  select(Source, Target, Layer, weight) %>%
  unite(Style, sep = ", ") 