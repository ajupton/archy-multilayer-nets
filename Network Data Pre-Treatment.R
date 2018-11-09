#' Data munging to convert network data into a form amenable 
#' to the construction of multilayer networks using multinet
#' and muxViz.

library(tidyverse)
library(multinet)
library(igraph)

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
# '\\.' matches a .
eco_all$Source <- str_replace_all(eco_all$Source, c(" " = "_", "-" = "_", "\\." = ""))
eco_all$Target <- str_replace_all(eco_all$Target, c(" " = "_", "-" = "_", "\\." = ""))

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

# At this point, node and edge table information is combined using the RStudio
# content editor. It's easier working in the content editor because Excel and text
# editing software often append spaces, commas, or other unwanted characters to
# the data, which multinet cannot handle. For information on how to create
# multilayer or multiplex networks in multinet, see the documentation on CRAN
# or you can view the file below once it is posted. 
 
test <- read.ml("ceramicMultilayer_complete_in progress.csv")  
test
plot(test)

# Pre-Treatment for muxViz ####
# muxViz is a powerful tool for multilayer network anlaysis and visualization
# Here, I'll work with the network data I have to create files for use in muxViz

# Style network edge lists for muxViz
# Pre-migration
style_all %>%
  filter(Time == 1) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_style_pre.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)

# Post-migration
style_all %>%
  filter(Time == 2) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_style_post.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)

# Across time
style_all %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_style_all.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)

# Economic network edge lists for muxViz
# Pre-migration
eco_all %>%
  filter(Time == 1) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_eco_pre.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)
  
# Post-migration
eco_all %>%
  filter(Time == 2) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_eco_post.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE) 

# Across time
eco_all %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_eco_all.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Interaction through cultural transmission network edge lists for muxViz

# Jars muxViz ####
# Import jar edgelist and munge the site names
jars <- read_csv('jar_complete_edgelist.csv')
jars$Source <- str_replace_all(jars$Source, c(" " = "_", "-" = "_", "\\." = ""))
jars$Target <- str_replace_all(jars$Target, c(" " = "_", "-" = "_", "\\." = ""))

# First, make directed graph txt files for muxZiv
# Jar directed all
jars %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_jtech_directed_all.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Jar directed pre
jars %>%
  filter(Time == 1) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_jtech_directed_pre.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Jar directed post
jars %>%
  filter(Time == 2) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_jtech_directed_post.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Now, create UNDIRECTED graph txt files for jars
# To do this, any reciprocal edge weights will be the mean of the two 
# directed edge weights

# Jar undirected all
jg <- graph.data.frame(jars, directed = TRUE)
jg_un <- as.undirected(jg, edge.attr.comb = "mean", mode = "collapse")

as.data.frame(as_edgelist(jg_un)) %>%
  mutate(weight = E(jg_un)$weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_jtech_undirected_all.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Jar undirected pre
jars %>%
  filter(Time == 1) %>%
  graph.data.frame(directed = TRUE) -> jg_pre
jg_un_pre <- as.undirected(jg_pre, edge.attr.comb = "mean", mode = "collapse")

as.data.frame(as_edgelist(jg_un_pre)) %>%
  mutate(weight = E(jg_un_pre)$weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_jtech_undirected_pre.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Jar undirected post
jars %>%
  filter(Time == 2) %>%
  graph.data.frame(directed = TRUE) -> jg_post
jg_un_post <- as.undirected(jg_post, edge.attr.comb = "mean", mode = "collapse")

as.data.frame(as_edgelist(jg_un_post)) %>%
  mutate(weight = E(jg_un_post)$weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_jtech_undirected_post.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Plates muxViz #### 
plates <- read_csv('plate_complete_edgelist.csv')
plates$Source <- str_replace_all(plates$Source, c(" " = "_", "-" = "_", "\\." = ""))
plates$Target <- str_replace_all(plates$Target, c(" " = "_", "-" = "_", "\\." = ""))

# Plates directed all
plates %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_ptech_directed_all.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Plates directed pre
plates %>%
  filter(Time == 1) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_ptech_directed_pre.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)

# Plates directed post
plates %>%
  filter(Time == 2) %>%
  select(Source, Target, weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_ptech_directed_post.txt", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)
  
# Now, create UNDIRECTED graph txt files for plates
# To do this, any reciprocal edge weights will be the mean of the two 
# directed edge weights

# Plate undirected all
pg <- graph.data.frame(plates, directed = TRUE)
pg_un <- as.undirected(pg, edge.attr.comb = "mean", mode = "collapse")

as.data.frame(as_edgelist(pg_un)) %>%
  mutate(weight = E(pg_un)$weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_ptech_undirected_all.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Plate undirected pre
plates %>%
  filter(Time == 1) %>%
  graph.data.frame(directed = TRUE) -> pg_pre
pg_un_pre <- as.undirected(pg_pre, edge.attr.comb = "mean", mode = "collapse")

as.data.frame(as_edgelist(pg_un_pre)) %>%
  mutate(weight = E(pg_un_pre)$weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_ptech_undirected_pre.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE)

# Plate undirected post
plates %>%
  filter(Time == 2) %>%
  graph.data.frame(directed = TRUE) -> pg_post
pg_un_post <- as.undirected(pg_post, edge.attr.comb = "mean", mode = "collapse")

as.data.frame(as_edgelist(pg_un_post)) %>%
  mutate(weight = E(pg_un_post)$weight) %>%
  unite(sep = " ") %>%
  write.table("edge_list_ptech_undirected_post.txt", row.names = FALSE,
              col.names = FALSE, quote = FALSE) 
