#' Data munging to convert network data into a form amenable 
#' to the construction of multilayer networks using multinet.

library(tidyverse)

# First, read in the social identification network built based on
# proportional similarity in plate stylistic designs between sites 
style_all <- read_csv("BR_UNDIRECTED_edgelist_complete_.csv")
eco_all <- read_csv("Econet_BR_UNDIRECTED_edgelist_complete_.csv")

# Multinet is implement in a variant of the C language and as such
# is bound by different rules. One of those is avoiding spaces in
# the actor (or in this case archaeological site) names

# Replace all spaces and dashes with an underscore for style layers
style_all$Source <- str_replace_all(style_all$Source, c(" " = "_", "-" = "_"))
style_all$Target <- str_replace_all(style_all$Target, c(" " = "_", "-" = "_"))
   
# Decompose edge table to edge vectors for style layers                     
style_all %>%
  mutate(Layer = ifelse(Time == 1, "Style_pre", "Style_post")) %>%
  select(Source, Target, Layer, weight) %>%
  unite(Style, sep = ",") %>%
  write_csv("style_edge_table_multinet.csv")

# Style node table
style_all %>%
  mutate(Layer = ifelse(Time == 1, "Style_pre", "Style_post")) %>%
  select(Source, Target, Layer) %>%
  gather(Site, Source:Layer, -Layer) %>%
  select(`Source:Layer`, Layer) %>%
  distinct(`Source:Layer`, Layer) %>%
  unite(Nodes_style, sep = ",") %>% 
  write_csv("style_node_table_multinet.csv")

# Economic network layer node cleaning
eco_all$Source <- str_replace_all(eco_all$Source, c(" " = "_", "-" = "_"))
eco_all$Target <- str_replace_all(eco_all$Target, c(" " = "_", "-" = "_"))

# Decompose edge table to edge vectors for economic layers  
eco_all %>%
  mutate(Layer = ifelse(Time == 1, "Eco_pre", "Eco_post")) %>%
  select(Source, Target, Layer, weight) %>%
  unite(Economic, sep = ",") %>%
  write_csv("economic_edge_table_multinet.csv")

# Economic networks node table
eco_all %>%
  mutate(Layer = ifelse(Time == 1, "Eco_pre", "Eco_post")) %>%
  select(Source, Target, Layer) %>%
  gather(Site, Source:Layer, -Layer) %>%
  select(`Source:Layer`, Layer) %>%
  distinct(`Source:Layer`, Layer) %>%
  unite(Nodes_style, sep = ",") %>%
  write_csv("economic_node_table_multinet.csv")
 
test <- read.ml("ceramicMultilayer_complete_in progress.csv")  
test
plot(test)
