## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ----preliminary, warning=FALSE, message=FALSE-------------------------------------------

#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("openxlsx", "data.table", "tidyverse", "bibliometrix", 
                          "igraph", "ggraph", "cowplot", "tidygraph", "benchmarkme", 
                          "parallel", "wesanderson", "scales", "countrycode", 
                          "doParallel", "foreach", "sensobol", "sp", "raster","ncdf4", 
                          "readxl"))

# Create custom theme
theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent",
                                           color = NA),
          legend.key = element_rect(fill = "transparent",
                                    color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.margin = margin(0.5, 0.1, 0.1, 0.1),
          legend.box.margin = margin(0.2,-4,-7,-7), 
          plot.margin = margin(3, 4, 0, 4), 
          legend.text = element_text(size = 8), 
          axis.title = element_text(size = 10),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          axis.title.x = element_text(size = 7.3),
          axis.title.y = element_text(size = 7.3),
          strip.text.x = element_text(size = 7.4),
          strip.text.y = element_text(size = 7.4),
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.title = element_text(size = 7.8)) 
}


## ----read_all_datasets, dependson=c("abstract_corpus", "full_text_corpus", "policy_corpus", "split")----

# CREATE VECTORS TO READ IN AND CLEAN THE DATASETS #############################

tmp <- list()
names.files <- c("WORK", "NETWORK")
topics <- c("water", "food")
corpus <- c("abstract.corpus", "policy.corpus", "full.text.corpus") 
cols_of_interest <- c("title", "author", "claim", "citation", 
                      "document.type", "nature.claim")

# Paste all possible combinations of names -------------------------------------

combs <- expand.grid(corpus = corpus, topics = topics, approach = names.files)
all.files <- paste(paste(paste(combs$corpus, combs$topics, sep = "."), combs$approach, sep = "_"), 
                   "xlsx", sep = ".")

# READ IN DATASETS AND TURN TO LOWERCAPS #######################################

tmp <- list()

for (i in 1:length(all.files)) {
  
  tmp[[i]] <- data.table(read.xlsx(all.files[i]))
  
  if (!str_detect(all.files[i], "NETWORK")) { 
    
    tmp[[i]][, title:= tolower(title)]
    
     } else {
    
    tmp[[i]][, (cols_of_interest):= lapply(.SD, tolower), .SDcols = (cols_of_interest)]
  }
}

names(tmp) <- all.files

sub(".*\\.([^\\.]+)_.*", "\\1", all.files)


# CLEAN AND MERGE DATASETS #####################################################

# Work datasets ----------------------------------------------------------------

dataset.works <- all.files[str_detect(all.files, "_WORK")]
dataset.works.topics <- sub(".*\\.([^\\.]+)_.*", "\\1", dataset.works)

tmp.works <- tmp[dataset.works]
names(tmp.works) <- dataset.works.topics
lapply(tmp.works, function(dt) dt[, .(doi, title, claim.in.text)]) %>%
  rbindlist(., idcol = "topic") %>%
  .[, .N, .(topic, claim.in.text)]

# Network datasets -------------------------------------------------------------

dataset.networks <- all.files[str_detect(all.files, "NETWORK")]
dataset.networks.topics <- sub(".*\\.([^\\.]+)_.*", "\\1", dataset.networks)

tmp2 <- tmp[dataset.networks]
names(tmp2) <- dataset.networks.topics

network.dt <- rbindlist(tmp2, idcol = "topic") %>%
  .[, policy:= grepl("^policy", doi)]  %>%
  .[, document.type:= trimws(document.type)] %>%
  .[, document.type:= tolower(document.type)]

# Retrieve year ----------------------------------------------------------------

network.dt[, year:= as.integer(sub(".* (\\d{4})[a-z]?$", "\\1", author))]

# move policy to author --------------------------------------------------------

network.dt[, author:= ifelse(policy == TRUE, doi, author)]

# CHECK NUMBER OF FAO AQUASTAT CITES ###########################################

aquastat.cites <- network.dt[citation %like% "fao aquastat"] %>%
  .[, .N, .(citation, topic)] 

aquastat.cites

oldest.aquastat.cite <- min(as.integer(sub(".* (\\d{4})[a-z]?$", "\\1", 
                                           aquastat.cites$citation)), 
    na.rm = TRUE)

# CHECK NUMBER OF FAOSTAT CITES ################################################

faostat.cites <- network.dt[citation %like% "faostat"] %>%
  .[, .N, .(citation, topic)] 

faostat.cites

oldest.faostat.cite <- min(as.integer(sub(".* (\\d{4})[a-z]?$", "\\1", 
                                           faostat.cites$citation)), 
                            na.rm = TRUE)

# WRITE LOOKUP TABLE TO CHECK ALREADY RETRIEVED STUDIES ########################

lookup.dt <- network.dt[, .(doi, title, author, topic)] %>%
  .[order(title)] %>%
  unique(.) 

lookup.dt[, .(number.rows = nrow(.SD)), topic]

# Export lookup tables ---------------------------------------------------------

write.xlsx(lookup.dt, "lookup.dt.xlsx")
write.xlsx(lookup.dt[topic == "water"], "lookup.water.dt.xlsx")
write.xlsx(lookup.dt[topic == "food"], "lookup.food.dt.xlsx")

# Remove the year from mentions to FAO Aquastat --------------------------------

pattern <- "\\b(?:19|20)\\d{2}\\b"  # Matches years between 1900 and 2099

for (col in c("citation", "author")) {
  matches <- grepl("^fao aquastat\\s+\\d+$", network.dt[[col]], ignore.case = TRUE)
  network.dt[matches, (col) := gsub("\\d+", "", network.dt[[col]][matches], perl = TRUE)]
  network.dt[, (col) := trimws(network.dt[[col]])]
}

# Remove the year from mentions to FAOSTAT --------------------------------

for (col in c("citation", "author")) {
  matches <- grepl("^faostat\\s+\\d+$", network.dt[[col]], ignore.case = TRUE)
  network.dt[matches, (col) := gsub("\\d+", "", network.dt[[col]][matches], perl = TRUE)]
  network.dt[, (col) := trimws(network.dt[[col]])]
}

# Rename columns ---------------------------------------------------------------

setnames(network.dt, c("author", "citation"), c("from", "to"))

# Rename category --------------------------------------------------------------

network.dt[, category:= ifelse(!classification == "F", "Uncertain", "Fact")]

# Create copy and remove duplicated --------------------------------------------

network.dt.claim <- copy(network.dt)
network.dt.claim <- unique(network.dt.claim, by = c("from", "to", "document.type", 
                                                    "nature.claim"))
cols_to_change <- colnames(network.dt)
network.dt.claim[, (cols_to_change):= lapply(.SD, trimws), .SDcols = (cols_to_change)]
network.dt.claim[, (cols_to_change):= lapply(.SD, str_squish), .SDcols = (cols_to_change)]

fwrite(network.dt.claim, "network.dt.claim.csv")

# Convert all to lower caps ----------------------------------------------------

network.dt <- network.dt[, .(from, to, year, document.type, nature.claim, 
                             classification, category, topic)]
cols_to_change <- colnames(network.dt)
network.dt[, (cols_to_change):= lapply(.SD, trimws), .SDcols = (cols_to_change)]
network.dt[, (cols_to_change):= lapply(.SD, str_squish), .SDcols = (cols_to_change)]


## ----descriptive_plots, dependson="read_all_datasets", fig.height=2, fig.width=6.3, dev="pdf"----

# PLOT DESCRIPTIVE STATISTICS ###################################################

total.rows <- network.dt[, .(number.rows = nrow(.SD)), topic]

# Check proportion of studies by nature of claim -------------------------------

network.dt.claim[, .N, .(nature.claim, topic)] %>%
  merge(., total.rows, by = "topic") %>%
  .[, fraction:= N / number.rows] %>%
  print()

# Count document type by nature of claim ---------------------------------------

a <- network.dt[, .N, .(nature.claim, document.type, topic)] %>%
  merge(., total.rows, by = "topic") %>%
  .[, proportion:= N / number.rows] %>%
  na.omit() %>%
  ggplot(., aes(reorder(nature.claim, proportion), proportion)) +
  coord_flip() +
  geom_bar(stat = "identity") + 
  facet_grid(topic~document.type) +
  scale_y_continuous(breaks = breaks_pretty(n = 2)) +
  labs(x = "", y = "Fraction") +
  theme_AP()

# Count how many documents make the claim and cite / do not cite, 
# by document.type -------------------------------------------------------------

b <- network.dt[, .(without.citation = sum(is.na(to)), 
                    with.citation = .N - sum(is.na(to))), .(document.type, topic)] %>%
  melt(., measure.vars = c("without.citation", "with.citation")) %>%
  merge(., total.rows, by = "topic") %>%
  .[, proportion:= value / number.rows] %>%
  ggplot(., aes(document.type, proportion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = breaks_pretty(n = 2)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = "Fraction") +
  facet_grid(topic~variable) + 
  theme_AP()

# merge -------------------------------------------------------------------------

plot.claim <- plot_grid(a, b, ncol = 2, rel_widths = c(0.6, 0.4), labels = "auto")
plot.claim


## ----citations_support_claim, dependson="read_all_datasets", fig.height=1.4, fig.width=2.5, warning=FALSE, dev="pdf"----

# PLOT DISTRIBUTION OF CITATION SUPPORTING THE CLAIM ###########################

plot.supporting.claim <- network.dt[, .N, .(from, topic)] %>%
  .[order(-N)] %>%
  ggplot(., aes(N)) +
  geom_histogram() + 
  facet_wrap(~topic, scale = "free") +
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  scale_y_continuous(breaks = breaks_pretty(n = 3)) +
  theme_AP() +
  labs(x = "Nº citations to claim per paper", y = "Nº papers")

plot.supporting.claim


## ----network_metrics, dependson="read_all_datasets"--------------------------------------

# CALCULATE NETWORK METRICS ####################################################

# only complete cases ----------------------------------------------------------

network.dt.complete <- network.dt[complete.cases(network.dt$to), ]
split.networks <- split(network.dt.complete, network.dt.complete$topic)

# Export-----
write.xlsx(network.dt.complete, "network.dt.complete.xlsx")

# Transform to graph -----------------------------------------------------------

citation_graph <- lapply(split.networks, function(dt) 
  graph_from_data_frame(d = dt, directed = TRUE))
  
# Calculate network metrics ----------------------------------------------------

lapply(citation_graph, function(x) edge_density(x))

# Modularity: 
# - c.1: Strong community structure, where nodes within groups are highly connected.
# - c. -1: Opposite of community structure, where nodes between groups are more connected.
# - c. 0: Indicates absence of community structure or anti-community structure in the network.
wtc <- lapply(citation_graph, function(x) cluster_walktrap(x))
lapply(wtc, function(x) modularity(x))

network_metrics <- lapply(citation_graph, function(x) 
  data.table(node = V(x)$name,
             
             # Degree of a node: The number of connections or 
             # edges linked to that node. 
             # It represents how well-connected or central a 
             # node is within the graph.
             degree = degree(x, mode = "in"),
             
             degree.out = degree(x, mode = "out"),
             
             # Betweenness centrality of a node: Measures the 
             # extent to which a node lies on the shortest 
             # paths between all pairs of other nodes in the graph. 
             # Nodes with high betweenness centrality act as 
             # bridges or intermediaries, facilitating 
             # communication and information flow between other nodes.
             betweenness = betweenness(x),
             
             # Closeness centrality of a node: Measures how 
             # close a node is to all other nodes in the graph, 
             # taking into account the length of the shortest paths. 
             # Nodes with high closeness centrality are able to 
             # efficiently communicate or interact with other 
             # nodes in the graph.
             closeness = closeness(x),
             pagerank = page_rank(x)$vector)
)

# Define the max number of rows
max.number <- 3

degree.nodes <- lapply(network_metrics, function(dt) dt[order(-degree)][1:max.number])
degree.nodes.out <- lapply(network_metrics, function(dt) dt[order(-degree.out)][1:max.number])
betweenness.nodes <- lapply(network_metrics, function(dt) dt[order(-betweenness)][1:max.number])
pagerank.nodes <- lapply(network_metrics, function(dt) dt[order(-closeness)][1:max.number])

degree.nodes
degree.nodes.out
betweenness.nodes
pagerank.nodes


## ----add_features, dependson=c("read_all_datasets", "network_metrics")-------------------

# ADD FEATURES TO NODES ########################################################

# Retrieve a vector with the node names ----------------------------------------

graph <- lapply(split.networks, function(nt)
  tidygraph::as_tbl_graph(nt, directed = TRUE))

vec.names <- lapply(graph, function(graph)
  graph %>%
    activate(nodes) %>%
    pull() %>%
    data.table(name = .))


# Merge with info from the network.dt ------------------------------------------

tmp.network <- split(network.dt, network.dt$topic)

vec.nature.claim <- list()

for(i in names(tmp.network)) {
  
  vec.nature.claim[[i]] <- merge(merge(vec.names[[i]], unique(tmp.network[[i]][, .(from, year, nature.claim)]), 
                                       by.x = "name", by.y = "from", all.x = TRUE), 
                                 unique(tmp.network[[i]][, .(from, document.type, classification, category)]), 
                                 by.x = "name", by.y = "from", all.x = TRUE)
}

# Merge with the correct order -------------------------------------------------

order_indices <- final.vec.nature.claim <- final.vec.document.type <- 
  final.vec.year <- final.vec.classification <- final.vec.category <- list()

for (i in names(vec.names)) {
  
  order_indices[[i]] <- match(vec.names[[i]]$name, vec.nature.claim[[i]]$name)
  final.vec.nature.claim[[i]] <- vec.nature.claim[[i]][order_indices[[i]], ] %>%
    .[, nature.claim] 
  final.vec.document.type[[i]] <- vec.nature.claim[[i]][order_indices[[i]], ] %>%
    .[, document.type] 
  final.vec.year[[i]] <- vec.nature.claim[[i]][order_indices[[i]], ] %>%
    .[, year] %>%
    as.numeric()
  final.vec.classification[[i]] <- vec.nature.claim[[i]][order_indices[[i]], ] %>%
    .[, classification]
  final.vec.category[[i]] <- vec.nature.claim[[i]][order_indices[[i]], ] %>%
    .[, category]
}

# Attach to the graph ----------------------------------------------------------

graph.final <- list()

for (i in names(graph)) {
  
  graph.final[[i]] <- graph[[i]] %>%
    activate(nodes) %>%
    mutate(nature.claim = final.vec.nature.claim[[i]], 
           document.type = final.vec.document.type[[i]], 
           year = final.vec.year[[i]],
           degree = network_metrics[[i]]$degree, 
           classification = final.vec.classification[[i]],
           category = final.vec.category[[i]],
           degree.out = network_metrics[[i]]$degree.out,
           betweenness = network_metrics[[i]]$betweenness, 
           pagerank = network_metrics[[i]]$pagerank)
  
  
}

for (i in names(graph.final)) {
  
  graph.final[[i]] <- graph.final[[i]] %>%
    activate(edges) %>%
    mutate(edge_color = .N()$nature.claim[to])
}


## ----export_nodes_edges, dependson="add_features"----------------------------------------

# EXPORT NODES AND EDGES #######################################################

for (i in topics) {
  
  nodes <- graph.final[[i]] %>%
    activate(nodes) %>%
    data.frame() %>%
    data.table() 
  
  edges <- graph.final[[i]] %>%
    activate(edges) %>%
    data.frame() %>%
    data.table() 
  
  write.xlsx(nodes, paste(i, ".nodes.xlsx", sep = ""))
  write.xlsx(edges, paste(i, ".edges.xlsx", sep = ""))
  
}


## ----calculate_proportion, dependson=c("add_features", "read_all_datasets")--------------

# NUMBER OF NODES ##############################################################

lapply(graph.final, function(graph) V(graph))

# NUMBER OF EDGES ##############################################################

lapply(graph.final, function(graph) ecount(graph))



## ----scale_free_plot, dependson=c("add_features", "read_all_datasets"), fig.height=1.6, fig.width=2.5, dev = "pdf"----

# SCALE-FREE PLOT ##############################################################

# Prepare data -----------------------------------------------------------------

dt.food <- graph.final[[1]] %>%
  activate(nodes) %>%
  data.frame() %>%
  data.table() %>%
  .[, topic:= "food"]

dt.water <- graph.final[[2]] %>%
  activate(nodes) %>%
  data.frame() %>%
  data.table() %>%
  .[, topic:= "water"]

tmp <- rbind(dt.food, dt.water)

# Calculate the degree distribution --------------------------------------------

degree_distribution <- tmp[, .(P_k = .N / nrow(tmp)), .(degree, topic)]
  
plot.degree.distribution <- degree_distribution %>%
  ggplot(., aes(degree, P_k)) +
  geom_point(size = 1) + 
  scale_y_log10() + 
  scale_x_log10() +
  facet_wrap(~topic) +
  labs(x = "Degree", y = "P(k)") +
  theme_AP()

plot.degree.distribution


## ----merge_descriptive_plots, dependson=c("scale_free_plot", "citations_support_claim", "descriptive_plots"), fig.height=3.5, fig.width=5.7, dev = "pdf"----

# MERGE DESCRIPTIVE PLOTS ######################################################

bottom <- plot_grid(plot.supporting.claim, plot.degree.distribution, ncol = 2, 
                    labels = c("c", "d"))
plot_grid(plot.claim, bottom, ncol = 1, rel_heights = c(0.6, 0.4))



## ----calculate_all_paths, dependson=c("add_features", "read_all_datasets")---------------

# CALCULATE ALL POSSIBLE PATHS #################################################

# Define function --------------------------------------------------------------

count_paths <- function(g) {
  
  # Extract the top 5 nodes with highest betweenness ---------------------------
  
  top_nodes <- g %>%
    activate(nodes) %>%
    top_n(5, betweenness) %>%
    pull(name)
  
  # Initialize counts ----------------------------------------------------------
  
  total_paths_count <- 0
  top_nodes_paths_count <- 0
  
  g <- as.igraph(g)
  
  results <- foreach(i = V(g), 
                     .combine = "c", 
                     .packages = "igraph") %:%
    
    foreach(j = V(g), 
            .combine = "c") %dopar% {
              
              total_paths_pair <- 0
              top_nodes_paths_pair <- 0
              
              if (i != j) {
                
                # Calculate all possible paths ---------------------------------
                
                paths <- all_simple_paths(g, from = i, to = j)
                total_paths_pair <- length(paths)
                
                # Check how many paths pass through the top betweenness nodes --
                
                for (path in paths) {
                  
                  if (any(names(path) %in% top_nodes)) {
                    top_nodes_paths_pair <- top_nodes_paths_pair + 1
                    
                  }
                }
              }
              
              # Return the count of all paths and paths through top nodes ------
              
              return(c(total_paths_pair, top_nodes_paths_pair))
            }
  
  # Aggregate results ----------------------------------------------------------
  
  total_paths_count <- sum(results[seq(1, length(results), by = 2)])
  top_nodes_paths_count <- sum(results[seq(2, length(results), by = 2)])
  
  return(c(total_paths_count, top_nodes_paths_count))
  
}

# Define parallel computing ----------------------------------------------------

cl <- makeCluster(floor(detectCores() * 0.75))
registerDoParallel(cl)

# Run the function -------------------------------------------------------------

results.counts <- lapply(graph.final, function(graph) 
  count_paths(graph))

# Stop the cluster -------------------------------------------------------------

stopCluster(cl)


## ----results_count_paths, dependson="calculate_all_paths"--------------------------------

# SHOW TOTAL NUMBER OF PATHS AND PROPORTION OF PATHS PASSING 
# THROUGH THE FIVE NODES WITH THE HIGHEST BETWEENNESS ##########################

results.counts

lapply(results.counts, function(x) x[[2]] / x[[1]])



## ----plot_network, dependson=c("add_features", "read_all_datasets"), fig.height=6, fig.width=7, dev = "pdf"----

# PLOT NETWORK #################################################################

seed <- 1234
selected_colors <- c("darkblue", "lightgreen", "orange", "red", "grey")

# by nature of claim -----------------------------------------------------------

# Label the nodes with highest degree ------------------------------------------

p1 <- p2 <- p3 <- p4 <- p5 <- list()

for(i in names(graph.final)) {
  
  set.seed(seed)
  
  p1[[i]] <- ggraph(graph.final[[i]], layout = "igraph", algorithm = "nicely") + 
    geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                   end_cap = circle(1, "mm"), 
                   aes(color = edge_color)) +
    scale_edge_color_manual(values = selected_colors, guide = "none") + 
    geom_node_point(aes(color = nature.claim, size = degree)) +
    geom_node_text(aes(label = ifelse(degree >= min(degree.nodes[[i]]$degree), name, NA)), 
                   repel = TRUE, size = 2.2) +
    labs(x = "", y = "") +
    scale_color_manual(name = "", 
                       values = selected_colors) +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "right") 
}
  
p1

for(i in names(graph.final)) {
  
  set.seed(seed)
  
  p5[[i]] <- ggraph(graph.final[[i]], layout = "stress") + 
    geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                   end_cap = circle(1, "mm"), 
                   aes(color = edge_color)) +
    scale_edge_color_manual(values = selected_colors, guide = "none") + 
    geom_node_point(aes(color = nature.claim, size = degree)) +
    geom_node_text(aes(label = ifelse(nature.claim == "modelling", name, NA)), 
                   repel = TRUE, size = 2.2) +
    labs(x = "", y = "") +
    scale_color_manual(name = "", 
                       values = selected_colors) +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "right") 
}

p5

# Label the nodes with highest betweenness -------------------------------------

for (i in names(graph.final)) {
  
  set.seed(seed)
  
  p2[[i]] <- ggraph(graph.final[[i]], layout = "igraph", algorithm = "nicely") + 
    geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                   end_cap = circle(1, "mm")) + 
    geom_node_point(aes(color = nature.claim, size = betweenness)) +
    geom_node_text(aes(label = ifelse(betweenness >= min(betweenness.nodes[[i]]$betweenness), 
                                      name, NA)), 
                   repel = TRUE, size = 2.2) +
    labs(x = "", y = "") +
    scale_color_manual(name = "", 
                       values = selected_colors) +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "right") 
}

p2


# by document.type--------------------------------------------------------------

for (i in names(graph.final)) {
  
  set.seed(seed)
  
  p3[[i]] <- ggraph(graph.final[[i]], layout = "igraph", algorithm = "nicely") + 
    geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                   end_cap = circle(1, "mm")) + 
    geom_node_point(aes(color = document.type, size = degree)) +
    geom_node_text(aes(label = ifelse(degree >= min(degree.nodes[[i]]$degree), name, NA)), 
                   repel = TRUE, size = 2.2) +
    labs(x = "", y = "") +
    scale_color_discrete(name = "") +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "right") 
}

p3

# Label nodes that are modelling exercises -------------------------------------

for (i in names(graph.final)) {
  
  set.seed(seed)
  
  p4[[i]] <- ggraph(graph.final[[i]], layout = "igraph", algorithm = "nicely") + 
    geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                   end_cap = circle(1, "mm")) + 
    geom_node_point(aes(color = nature.claim)) +
    geom_node_text(aes(label = ifelse(nature.claim == "modelling", name, NA)), 
                   repel = TRUE, size = 2.2) +
    labs(x = "", y = "") +
    scale_color_manual(name = "", 
                       values = selected_colors) +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "right") 
}

p4


## ----plot_evolution_nature.claim, dependson="add_features", dev = "pdf", fig.height=2.2, fig.width=5----

# PLOT EVOLUTION NATURE CLAIM THROUGH TIME #####################################

out <- list()

for (i in names(graph.final)) {
 
    selected_colors <- c("darkblue", "lightgreen", "orange", "red", "grey")
  
  out[[i]] <- graph.final[[i]] %>%
    activate(nodes) %>%
    data.frame() %>%
    data.table() %>%
    .[, .N, .(year, nature.claim)] %>%
    ggplot(., aes(year, N, fill = nature.claim)) +
    scale_fill_manual(values = selected_colors, name = "") +
    geom_area() +
    scale_x_continuous(breaks = breaks_pretty(n = 3)) +
    theme_AP() + 
    ggtitle(names(graph.final[i])) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 8))
  
}

legend <- get_legend(out[[2]] + theme(legend.position = "right"))
bottom <- plot_grid(out[[1]] + labs(x = "Year", y = "Nº studies"), 
                    out[[2]] + labs(x = "Year", y = ""))
plot.network.claims <- plot_grid(bottom, legend, rel_widths = c(0.75, 0.25), 
                                 ncol = 2)
plot.network.claims


## ----function_uncertainty, dependson="add_features"--------------------------------------

# COUNT PROPORTION OF NODES THAT STATE AS FACT A CLAIM UTTERED AS UNCERTAIN ####

uncertainty_plot_fun <- function(graph) {
  
# Extract name of all studies ----------------------------------------------------
  
all.names <- graph %>%
    activate(nodes) %>%
    pull(name)
  
# Extract name of studies stating claim as fact --------------------------------
  
  f.names <- graph %>%
    activate(nodes) %>%
    data.frame() %>%
    filter(classification == "F") %>%
    pull(name)
  
  # Add names to edges -----------------------------------------------------------
  
  add.names.edges <- graph %>%
    activate(edges) %>%
    mutate(from.name = all.names[from], 
           to.name = all.names[to])
  
  # Calculate, for each study stating claim as fact, the studies it cites --------
  out.classes <- lapply(f.names, function(x) {
    
    out_nodes <- add.names.edges %>%
      activate(edges) %>%
      filter(from.name == x) %>%
      pull(to.name)
    
  })
  
  # unlist names of studies cited by studies uttering claim as fact --------------
  
  di <- sort(unlist(out.classes))
  
  # Extract only those that do not state claim as fact ---------------------------
  
  nodes.no.fact <- graph %>%
    activate(nodes) %>%
    data.frame() %>%
    data.table() %>%
    .[name %in% di] %>%
    .[!classification == "F"] %>%
    .$name
  
  name.edges <- add.names.edges  %>%
    activate(edges) %>%
    data.frame() %>%
    filter(from.name %in% f.names & to.name %in% nodes.no.fact) %>%
    .[, c("from.name", "to.name")] %>%
    c() %>%
    unlist() %>%
    unique(.)
  
  output <- add.names.edges  %>%
    activate(nodes) %>%
    filter(name %in% name.edges) %>%
    activate(edges) %>%
    filter(from.name %in% name.edges & to.name %in% name.edges)
  
  return(output)
  
}


## ----plot_uncertainty_facts, dependson="add_features", fig.height=3.2, fig.width=4.7, dev="pdf"----

# PLOT GRAPH UNCERTAINTIES TURNED INTO FACTS ####################################

out <- lapply(graph.final, function(x) uncertainty_plot_fun(x))

p7 <- list()

for (i in names(out)) {
  
  set.seed(seed)
  
  p7[[i]] <- ggraph(out[[i]], layout = "igraph", algorithm = "nicely") + 
    geom_edge_link(arrow = arrow(length = unit(1.6, 'mm')), 
                   end_cap = circle(1, "mm")) + 
    geom_node_point(aes(color = category, size = degree, shape = classification)) +
    scale_color_manual(values = c("lightgreen", "orange")) +
    scale_shape_discrete(labels = c("Approximate", "Fact", "Lower threshold", "Range", "Upper threshold")) +
    labs(x = "", y = "") +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "right", 
          legend.text = element_text(size = 7.2)) 
}

p7


## ----plot_uncertainty_merged, dependson="plot_uncertainty_facts", fig.width=4, fig.height=5.5, dev = "pdf"----

# MERGE PLOTS ##################################################################

plot.uncertainty.paths <- plot_grid(p7[[1]], p7[[2]], ncol = 1, labels = c("e", "f"))
plot.uncertainty.paths


## ----paths_from_unc_to_facts, dependson="add_features"-----------------------------------

# FUNCTION TO CALCULATE ALL PATHS BETWEEN PAIRS OF NODES #######################

calculate_paths <- function(graph) {
  
  # Convert to igraph ----------------------------------------------------------
  
  igraph_graph <- as.igraph(graph)
  
  # Get all unique pairs of nodes ----------------------------------------------
  
  node_pairs <- expand.grid(from = V(igraph_graph), to = V(igraph_graph))
  node_pairs <- node_pairs[node_pairs$from != node_pairs$to, ]
  
  # Function to calculate all simple paths between a pair of nodes--------------
  
  calculate_paths <- function(from, to) {
    paths <- all_simple_paths(igraph_graph, from = from, to = to)
    lapply(paths, names)
  }
  
  # Apply the function to all node pairs and unnest the results-----------------
  
  all_paths <- node_pairs %>%
    rowwise() %>%
    mutate(paths = list(calculate_paths(from, to))) %>%
    unnest(cols = c(paths))
  
  out <- sum(sapply(all_paths$paths, function(x) length(x)))
  
  return(out)

}

# CALCULATE ALL PATHS / PATHS TURNING HYPOTHESIS INTO FACTS ####################

all.paths <- hypothesis.into.facts.paths <- list()

for (i in names(graph.final)) {
  
  all.paths[[i]] <- calculate_paths(graph.final[[i]])
  hypothesis.into.facts.paths[[i]] <- uncertainty_plot_fun(graph.final[[i]]) %>%
    calculate_paths(.)
 
}

# Print results: proportion of paths turning uncertainties into facts ----------------------------------------------------------------

for (i in names(all.paths)) {
  print(hypothesis.into.facts.paths[[i]] / all.paths[[i]])
}


## ----proportion_paths, dependson="add_features"------------------------------------------

# DEFINE FUNCTION ##############################################################

proportion_paths <- function(graph) {
  
  # Turn into data.frame -------------------------------------------------------
  
  end_nodes <- graph %>% 
    activate(nodes) %>% 
    filter(degree.out == 0) %>%
    data.frame()
  
  end_node_indices <- end_nodes$name
  
  # Loop to store all paths to all end-nodes -----------------------------------
  all_paths <- list()
  
  for (v in igraph::V(as.igraph(graph))) {
    
    paths_from_v <- igraph::all_simple_paths(as.igraph(graph), 
                                             from = v, 
                                             to = end_node_indices)
    
    if (length(paths_from_v) > 0) {
      
      all_paths <- c(all_paths, paths_from_v)
    }
  }
  
  # Extract the label of the last node in each path ----------------------------
  
  end_labels <- sapply(all_paths, function(path) {
    
    last_node <- tail(path, 1)
    last_node_name <- V(as.igraph(graph))[last_node]$name
    
    graph %>% 
      activate(nodes) %>%
      filter(name == last_node_name) %>%
      pull(nature.claim)
    })
  
  # Proportion of paths ending in "no citation", "no claim" and "modelling" ----
  
  no_citation_paths <- sum(end_labels == "no citation", na.rm = TRUE)
  no_claim_paths <- sum(end_labels == "no claim", na.rm = TRUE)
  modelling_paths <- sum(end_labels == "modelling", na.rm = TRUE)
  na_paths <- sum(is.na(end_labels))
  total_paths <- length(end_labels)
  
  proportion_no_citation <- no_citation_paths / total_paths
  proportion_no_claim <- no_claim_paths / total_paths
  proportion_modelling <- modelling_paths / total_paths
  proportion_na <- na_paths / total_paths
  
  
  # Wrap up for output ---------------------------------------------------------
  
  output <- data.table("no citation" = proportion_no_citation, 
                       "no claim" = proportion_no_claim,
                       "modelling" = proportion_modelling, 
                       "NA" = proportion_na)
  return(output)

}

# RUN FUNCTION #################################################################

out <- lapply(graph.final, function(graph) proportion_paths(graph))
out



## ----sum_no.claim_no.citation, dependson="proportion_paths"------------------------------

# SUM PROPORTION NO CLAIM AND NO CITATION ######################################

lapply(out, function(x) x[, `no citation` + `no claim`])


## ----plot_proportion_paths, dependson="proportion_paths", fig.height=2.4, fig.width=2.5, dev = "pdf"----

# PLOT PROPORTION OF PATHS ENDING IN MODELLING, NO CLAIM AND NO CITATION #######

plot.ending.modelling <- rbindlist(out, idcol = "belief") %>%
  melt(., measure.vars = colnames(.)[-1]) %>%
  ggplot(., aes(belief, value, fill = variable)) +
  geom_bar(stat = "identity", 
           position = position_dodge(0.5)) +
  labs(x = "", y = "Fraction") +
  scale_fill_manual(values = c("orange","red", "lightgreen", "grey"), 
                    name = "") + 
  theme_AP() + 
  theme(legend.position = c(0.6, 0.9), 
        legend.text = element_text(size = 7))

plot.ending.modelling


## ----plot_network_time, dependson="add_features", dev = "pdf"----------------------------

# PLOT NETWORK THROUGH TIME ####################################

plot.years <- list()

for (i in c("water", "food")) {
  
  # Extract vector with names ----------------------------------------------------

  location_aquastat <- graph.final[[i]] %>%
  activate(nodes) %>%
  data.frame() %>%
  pull(name) %>%
  grep("aquastat", .)
  
    location_faostat <- graph.final[[i]] %>%
    activate(nodes) %>%
    data.frame() %>%
    pull(name) %>%
    grep("faostat", .)
  
  # Extract vector with years ----------------------------------------------------
  
  v_years <- graph.final[[i]] %>%
  activate(nodes) %>%
  data.frame() %>%
  pull(year) 
  
  # Substitute fao aquastat/faostat without year with the oldest citations -----
  
  v_years[location_aquastat] <- oldest.aquastat.cite
  v_years[location_faostat] <- oldest.faostat.cite

# Find NA values ---------------------------------------------------------------
  
  na_indices <- is.na(v_years)
  sum(na_indices)
  
  # Generate random values to replace NA -----------------------------------------
  
  random_values <- sample(2000:2020, sum(na_indices), replace = TRUE)
  
  # Replace NA with random values ------------------------------------------------
  
  v_years[na_indices] <- random_values
  
  # Define the coordinates--------------------------------------------------------
  
  y_positions <- runif(length(v_years), min = -3, max = 3)  # Random y-axis position
  layout <- cbind(v_years, y_positions)  # Use actual years for x-axi
  layout_matrix <- as.matrix(layout)
  colnames(layout_matrix) <- c("x", "y")
  
  # PLOT NETWORK THROUGH TIME ####################################################
  
  # Set seed ---------------------------------------------------------------------
  
  set.seed(seed)
  
  # Plot -------------------------------------------------------------------------

plot.years[[i]] <- ggraph(graph.final[[i]], layout = layout_matrix, algorithm = "nicely") +
  geom_edge_link(arrow = arrow(length = unit(1.8, "mm")), 
                 end_cap = circle(1, "mm"), 
                 color = "grey", 
                 alpha = 0.4) + 
  geom_node_point(aes(color = nature.claim, size = degree)) +
  geom_node_text(aes(label = ifelse(nature.claim == "modelling", name, NA)), 
                 repel = TRUE, size = 2.5) +
  scale_color_manual(name = "", 
                     values = selected_colors) +
  scale_x_continuous(name = "Year", 
                     limits = range(v_years), 
                     breaks = seq(min(v_years), 
                                  max(v_years), by = 5)) +
  labs(x = "Year", y = "") +
  theme_AP() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) 

}

plot.years


## ----network.time.sam, dependson="add_features", dev = "pdf", fig.height=6.5, fig.width=6.5----

# ANOTHER VISUALIZATION FOR YEARS BASED ON POLAR COORDINATES ###################

plot.years <- list()

for (i in c("water", "food")) {
  
  # Replace NA values in year with random samples from 2000 to 2020 ------------
  
  g <- graph.final[[i]] %>%
    activate(nodes) %>%
    mutate(year = ifelse(is.na(year), sample(2000:2020, replace = TRUE), year)) %>%
    mutate(
      year_normalized = (year - min(year)) / (2024 - min(year)),  # Normalize relative to 2024
      radius = year_normalized
    )
  
  # Assign the calculated positions --------------------------------------------
  
  g <- g %>%
    mutate(
      angle = seq(0, 2 * pi, length.out = n() + 1)[1:n()],
      x = radius * cos(angle),
      y = radius * sin(angle)
    )
  
  # Determine the range of years ----------------------------------------------
  min_year <- min(g %>% pull(year))
  
  # Dynamically determine the start year for the concentric circles -----------
  start_year <- floor(min_year / 10) * 10  # Round down to the nearest decade
  end_year <- 2024  # Explicitly set the end year to 2024
  year_intervals <- seq(start_year, end_year, by = 10)

  # Create concentric circles every ten years ---------------------------------
  circle_data <- lapply(year_intervals, function(yr) {
    r <- (yr - min_year) / (end_year - min_year)
    tibble(
      x = r * cos(seq(0, 2 * pi, length.out = 100)),
      y = r * sin(seq(0, 2 * pi, length.out = 100)),
      year = yr,
      label_x = r,  # Label position on the x-axis (angle = 0)
      label_y = 0   # Label position on the y-axis (angle = 0)
    )
  }) %>% bind_rows()
  
  # Remove duplicate labels ---------------------------------------------------
  label_data <- circle_data %>%
    distinct(year, .keep_all = TRUE)  # Keep only unique year labels
  
  # Plot -----------------------------------------------------------------------
  
  # Set seed ------------------------------
  
  set.seed(seed)
  
  plot.years[[i]] <- ggraph(g, layout = "manual", x = x, y = y) +
    # Add concentric circles
    geom_edge_link(arrow = arrow(length = unit(1.8, "mm")), 
                   end_cap = circle(1, "mm"), 
                   alpha = 0.07, 
                   aes(color = edge_color)) + 
    scale_edge_color_manual(values = selected_colors, guide = "none") +
    geom_node_point(aes(color = nature.claim, size = degree)) +
    geom_node_text(aes(label = ifelse(nature.claim == "modelling", name, NA)), 
                   repel = TRUE, size = 2.5) +
    scale_color_manual(name = "", 
                       values = selected_colors) +
    geom_path(data = circle_data, aes(x = x, y = y, group = factor(year)), 
              color = "black", linetype = "dashed") +
    # Add year labels
    geom_text(data = label_data, aes(x = label_x, y = label_y, label = year), 
              hjust = -0.2, vjust = 0.5, size = 3) +
    labs(x = "", y = "") +
    theme_AP() +
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          legend.position = "top")
}

plot.years


## ----network_split_years, dependson="add_features"---------------------------------------

# FUNCTION TO PLOT EVOLUTION OF NETWORK THROUGH TIME ###########################

network_through_time_fun <- function(graph, Year, seed) {
  
  # Extract all names ----------------------------------------------------------
  
  all.names <- graph %>%
    activate(nodes) %>%
    pull(name)
  
  # Add names to edges ---------------------------------------------------------
  
  add.names.edges <- graph %>%
    activate(edges) %>%
    mutate(from.name = all.names[from], 
           to.name = all.names[to])
  
  # Extract nodes by year ------------------------------------------------------
  
  names.targeted <- add.names.edges %>%
    activate(edges) %>%
    filter(year < Year) %>%
    data.frame() %>%
    .[, c("from.name", "to.name")] %>%
    c() %>%
    unlist() %>%
    unique(.)
  
  name.nodes <- add.names.edges %>%
    activate(nodes) %>%
    filter(name %in% names.targeted) %>%
    activate(edges) %>%
    filter(from.name %in% names.targeted & to.name %in% names.targeted)
  
  set.seed(seed)
  
  # Plot -----------------------------------------------------------------------
  
  out <- ggraph(name.nodes, layout = "igraph", algorithm = "nicely") + 
    geom_edge_link(arrow = arrow(length = unit(1, 'mm')), 
                   end_cap = circle(0.3, "mm")) + 
    geom_node_point(aes(color = nature.claim, size = degree)) +
    geom_node_text(aes(label = ifelse(nature.claim == "modelling", name, NA)), 
                   repel = TRUE, size = 2.2) +
    scale_color_manual(name = "", 
                       values = selected_colors) +
    labs(x = "", y = "") +
    theme_AP() + 
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "none") 
  
  return(out)
  
}

# DEFINE YEARS OF INTEREST #####################################################

years.vector <- c(seq(2000, 2020, 10), 2024)

# RUN FUNCTION #################################################################

plots.through.time <- list()

for (i in names(graph.final)) {
  
  plots.through.time[[i]] <- lapply(years.vector, function(year)
    network_through_time_fun(graph = graph.final[[i]], Year = year, seed = seed) + 
      ggtitle(year))
}


## ----plot_years_more, dependson="network_split_years", fig.height=6, fig.width=7, dev = "pdf"----

da <- list()

for (i in names(plots.through.time)) {
  
  for (j in 1:length(plots.through.time[[i]])) {
    
    da[[i]][[j]] <- plots.through.time[[i]][[j]] + 
      geom_node_point(aes(color = nature.claim)) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            legend.position = "right") 
  }
}

da


## ----plot_network_split_years, dependson="network_split_years", fig.height=6.5, dev = "pdf"----

# PLOT #########################################################################

# Extract legend ---------------------------------------------------------------

legend.plot <- list()

for (i in names(plots.through.time)) {
  
  legend.plot[[i]] <- get_legend(plots.through.time[[i]][[length(plots.through.time[[i]])]] + 
                                   theme(legend.position = "top"))
}

# Plot -------------------------------------------------------------------------

bottom <- out.plot <- list()

for (i in names(plots.through.time)) {
  
  bottom[[i]] <- do.call(plot_grid, c(plots.through.time[[i]], 
                                      nrow = floor(length(years.vector) / 2)))
  out.plot[[i]] <- plot_grid(legend.plot[[i]], 
                             bottom[[i]], ncol = 1, rel_heights = c(0.1, 0.9))
  
}

out.plot


## ----analysis_network_paths, dependson="add_features"------------------------------------

# COUNT THE NUMBER OF NODES WITH PATHS ULTIMATELY LEADING TO NODES
# THAT DO NOT MAKE THE CITATION ################################################

# Function: loop through each node that do not make the claim to find all nodes 
# connected to it --------------------------------------------------------------

nodes_to_no_claim_node_fun <- function(g, terminal_nodes) {
  
  if (!is.igraph(g)) {
    g <- as.igraph(g)
  }
  
  all_predecessors <- vector("list", length(terminal_nodes))
  
  for (i in seq_along(terminal_nodes)) {
    
    terminal_node <- terminal_nodes[i]
    predecessors <- subcomponent(g, terminal_node, mode = "in")
    all_predecessors[[i]] <- predecessors
  }
  
  unique_predecessors <- unique(names(unlist(all_predecessors)))
  
  return(unique_predecessors)
  
}


## ----analysis_paths_nodes, dependson="analysis_network_paths"----------------------------

# CALCULATE

# Extract name of all nodes ----------------------------------------------------
all_nodes <- lapply(graph.final, function(graph)
  graph %>%
    activate(nodes) %>%
    pull(name))

# Extract name of nodes that do not make the claim -----------------------------

no.claim_nodes <- lapply(graph.final, function(graph)
  graph %>%
    activate(nodes) %>%
    filter(degree.out == 0 & nature.claim == "no claim") %>%
    pull(., "name"))

# Extract name of nodes that do not make the claim and those that make 
# the claim but do not cite anybody --------------------------------------------

no.claim.and.no.citation.nodes <- lapply(graph.final, function(graph) 
  graph %>%
  activate(nodes) %>%
  filter(degree.out == 0 & nature.claim == "no claim" | nature.claim == "no citation" ) %>%
  pull(., "name"))

# Run the function -------------------------------------------------------------

tmp <- list()

for(i in names(graph.final)) {
  
  tmp[[i]] <- lapply(list(no.claim_nodes[[i]], 
                          no.claim.and.no.citation.nodes[[i]]), function(x)
    sort(nodes_to_no_claim_node_fun(graph.final[[i]], terminal_nodes = x)))
}

for(i in names(graph.final)) {
  names(tmp[[i]]) <- c("path ending in no claim", 
                               "path ending in no claim or no citation")
}

# Calculate proportions --------------------------------------------------------

out <- list()

for(i in names(tmp)) {
  out[[i]] <- lapply(tmp[[i]], function(x) length(x) / length(all_nodes[[i]]))
}

out


## ----fun_amplification, dependson="add_features"-----------------------------------------

# CALCULATE AMPLIFICATION FUNCTION #############################################

# Prepare data -----------------------------------------------------------------

vec.topics <- c("food", "water")

graph <- modelling.papers <- list()

for (i in 1:length(vec.topics)) {
  
  graph[[i]] <- network.dt.complete[topic == vec.topics[[i]]] %>%
    graph_from_data_frame(.)
  
  modelling.papers[[i]] <- network.dt.complete[topic == vec.topics[[i]] & 
                                                 nature.claim == "modelling"] %>%
    .[, from] %>%
    unique()
}

names(graph) <- vec.topics
names(modelling.papers) <- vec.topics

# DEFINE AMPLIFICATION FUNCTIONS ###############################################

# Citation amplification function ----------------------------------------------

citation_amplification_index <- function(graph, source_paper, modelling_papers) {
  
  # Get all simple paths from paper P ------------------------------------------
  
  all_paths <- all_simple_paths(graph, from = source_paper, mode = "out")
  
  # Filter out paths of length 1 that lead to modelling papers -----------------
  
  filtered_paths <- Filter(function(path) {

    if (length(path) > 2) {
      
      return(TRUE)
      
    } else {
      
      # If length is 2, check if terminal node is modelling paper --------------
      
      return(!(tail(path, n = 1) %in% modelling_papers))
    }
  }, all_paths)
  
  # Calculate the number of paths ----------------------------------------------
  
  citation_amplification_index <- length(filtered_paths)
  
  return(citation_amplification_index)
}

# Function to apply previous function to all papers ----------------------------

calculate_all_cai <- function(graph, modelling_papers) {

  papers <- V(graph)$name
  
  # Initialize an empty data.table to store results ----------------------------
  
  result_dt <- data.table(
    paper = character(),
    cai = numeric()
  )
  
  # Iterate over each paper ----------------------------------------------------
  
  for (paper in papers) {
    
    cai <- citation_amplification_index(graph, paper, modelling_papers)
    result_dt <- rbind(result_dt, data.table(paper = paper, cai = cai))
  }
  
  return(result_dt)
}

# CALCULATE AMPLIFICATION INDEX ################################################

# Calculate average amplification index of the networks ------------------------
# (e.g., the number paths initiated by the average paper
# leading to studies that do # not flow directly to "primary" data)

out <- list()

for(i in names(modelling.papers)) {
  
  out[[i]] <- calculate_all_cai(graph[[i]], modelling.papers[[i]])
}

# ARRANGE DATA #################################################################

tmp <- rbindlist(out, idcol = "topic")

# SUMMARY STATISTICS ###########################################################

tmp[, mean(cai), topic]

# First 5 papers amplifying the most -------------------------------------------

tmp2 <- tmp[, .SD, topic] %>%
  .[order(-cai, topic)] %>%
  .[, head(.SD, 5), topic]

tmp2


## ----plot_amplification, dependson="fun_amplification", fig.height=2, fig.width=3--------

# PLOT DISTRIBUTION OF AMPLIFICATION INDIXES ###################################

plot.amplification <- list()

# PLOT #########################################################################

plot.amplification <- ggplot(tmp, aes(cai)) +
  geom_histogram() + 
  facet_wrap(~topic, scales = "free") +
  theme_AP() +
  labs(y = "Counts", x = "Amplification index")

plot.amplification


## ----plot_amplification_network, dependson="fun_amplification", dev = "pdf"--------------

# PLOT THE NETWORK OF TOP AMPLIFYING PAPERS ####################################

# Define the starting nodes ----------------------------------------------------

vec.names.amplification <- tmp2[, slice_max(.SD, cai, n = 1), topic] %>%
  .[, paper]

# Reorder so the top node for food comes first ---------------------------------

vec.names.amplification <- vec.names.amplification[order(vec.names.amplification)]

out <- list()

for (i in 1:length(vec.names.amplification)) {
  
  start_node <- vec.names.amplification[[i]]
  
  # Get all simple paths from the starting node to all reachable nodes
  paths <- all_simple_paths(graph[[i]], from = V(graph[[i]])[name == start_node])
  
  # Convert paths to a list of edge pairs for easy visualization
  edge_list <- lapply(paths, function(path) {
    edges <- as_edgelist(induced_subgraph(graph[[i]], path), names = TRUE)
    data.frame(from = edges[, 1], to = edges[, 2])
  })
  
  # Combine all path edges into a single data frame
  path_edges <- do.call(rbind, edge_list)
  
  #Get the unique nodes involved in the paths
  path_nodes <- unique(c(path_edges$from, path_edges$to))
  
  # Create the subgraph of all paths starting from the target node
  subgraph <- induced_subgraph(graph[[i]], vids = V(graph[[i]])[name %in% path_nodes])
  
  # Convert the subgraph to a tidygraph object
  subgraph_tbl <- as_tbl_graph(subgraph)
  
  # Retrieve a vector with the node names ----------------------------------------
  
  vec.names <- subgraph_tbl %>%
    activate(nodes) %>%
    pull() %>%
    data.table(name = .)
  
  nature.claim.vec <- graph.final[[i]] %>%
    activate(nodes) %>%
    data.frame() %>%
    data.table() %>%
    .[name %in% vec.names$name] %>%
    .[, nature.claim]
  
  subgraph_tbl <- subgraph_tbl %>%
    activate(nodes) %>%
    mutate(nature.claim = nature.claim.vec)
  
  # Plot the subgraph with ggraph
  
  if (i == 1) {
    
    selected_colors <- c("darkblue", "orange", "red")
    
  } else {
  
  selected_colors <- c("darkblue", "lightgreen", "orange", "red")
  
  }
  
  set.seed(123)
  
  out[[i]] <- ggraph(subgraph_tbl, layout = "igraph", algorithm = "nicely") +  
    geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                   end_cap = circle(1, "mm")) +
    geom_node_point(size = 1.5, aes(color = nature.claim)) + 
    scale_color_manual(name = "", 
                       values = selected_colors) +
    theme_AP() +
    labs(x = "", y = "") +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          legend.position = "none", 
          legend.text = element_text(size = 7.3))
}

out


## ----plot_ampl_merged, dependson="plot_amplification", fig.height=4.7, fig.width=3.1, dev = "pdf"----

plot.ampl.merged <- plot_grid(out[[1]], 
                              out[[2]], ncol = 1, 
                              labels = c("c", "d"))

plot.ampl.merged 


## ----plot_merged_ind_ampl, dependson=c("plot_ampl_merged", "plot_amplification"), fig.width=3, fig.height=5.6, dev="pdf"----

full.amplification.plot <- plot_grid(plot.ending.modelling + 
                                       guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
                                       theme(legend.position = "top"), 
                                     plot.amplification, 
                                     rel_heights = c(0.6, 0.4),
                                     ncol = 1, labels = "auto")

full.amplification.plot



## ----plot_merge_amp_modelling, dependson=c("plot_merged_ind_ampl", "plot_ampl_merged")----

full.amplification.plot <- plot_grid(full.amplification.plot, 
                                     plot.ampl.merged, ncol = 1, 
                                     rel_heights = c(0.4, 0.6), 
                                     labels = c("", ""))

full.amplification.plot



## ----plot_amplification_uncertainty, dependson=c("plot_merged_ind_ampl", "plot_uncertainty_merged", "plot_proportion_paths", "plot_merge_amp_modelling"), fig.height=6, fig.width=6.2, dev = "pdf"----


plot_grid(full.amplification.plot, plot.uncertainty.paths, ncol = 2, rel_widths = c(0.4, 0.6))



## ----both_networks, dependson="network_metrics"------------------------------------------

# CHECK FULL NETWORK AND OVERLAP BETWEEN WATER AND FOOD NETWORK ################

# Prepare data ----------------------------------------------------------------

dd <- tidygraph::as_tbl_graph(network.dt.complete, directed = TRUE)

# Extract vector with names ----------------------------------------------------

all.names <- dd %>%
  activate(nodes) %>%
  pull(name)

# Retrieve names from water and food belief system -----------------------------

names.food <- network.dt.complete[topic == "food", to]
names.water <- network.dt.complete[topic == "water", to]

# Define intersections and differences -----------------------------------------

names.only.food <- setdiff(names.food, names.water)
names.only.water <- setdiff(names.water, names.food)
names.both <- intersect(names.water, names.food)

# New column defining whether nodes are in water, food or in both networks -----

final.graph <- dd %>%
  activate(nodes) %>%
  mutate(topic.final = ifelse(name %in% names.only.food, "food", 
                              ifelse(name %in% names.only.water, "water", 
                                     ifelse(name %in% names.both, "both", "uncited"))), 
         topic.final = factor(topic.final, levels = c("food", "water", "both", "uncited")))

final.graph <- final.graph %>%
  activate(edges) %>%
  mutate(edge_color = .N()$topic.final[to])

# SOME STATS ###################################################################

dt.nodes <- final.graph %>%
  activate(nodes) %>%
  data.frame() %>%
  data.table() 

# Fraction of network overlap --------------------------------------------------

dt.nodes[, .N, topic.final] %>%
  .[, fraction:= N / nrow(dt.nodes)] %>%
  print


## ----plot_network_complete, dependson="both_networks", dev = "pdf", fig.height=5.5, fig.width=5.5----

# PLOT MERGED NETWORK ##########################################################

selected.colors <- c("brown", "blue", "yellow", "grey")

ggraph(final.graph, layout = "graphopt") + 
  geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                 end_cap = circle(1, "mm"), 
                 aes(color = edge_color)) +
  geom_node_point(aes(color = topic.final), size = 1) +
  scale_edge_color_manual(values = selected.colors, guide = "none") +
  scale_color_manual(name = "", 
                     values = selected.colors) +
  labs(x = "", y = "") +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "top") 


## ----plot_shared_networks, dependson="network_metrics", dev = "pdf", fig.height= 3.2, fig.width=4.5----

# PLOT ONLY THE NETWORK OF NODES BEING CITED FOR BOTH BELIEFS ##################

# Prepare data -----------------------------------------------------------------

intersect.network <- network.dt.complete[to %in% names.both] %>%
  tidygraph::as_tbl_graph(., directed = TRUE)

intersect.graph <- network.dt.complete[to %in% names.both] %>%
  graph_from_data_frame(d = ., directed = TRUE)

# Calculate metrics ------------------------------------------------------------

intersect.metrics <- data.table(node = V(intersect.graph)$name, 
                                 degree = degree(intersect.graph, mode = "in"), 
                                 degree.out = degree(intersect.graph, mode = "out"),
                                 betweenness = betweenness(intersect.graph),
                                 closeness = closeness(intersect.graph),
                                 pagerank = page_rank(intersect.graph)$vector)

degree.nodes <- intersect.metrics[order(-degree)][1:3]
degree.out.nodes <- intersect.metrics[order(-degree.out)][1:3]
betweenness.nodes <- intersect.metrics[order(-betweenness)][1:3]

# Retrieve a vector with the node names ----------------------------------------

vec.names <- intersect.network %>%
  activate(nodes) %>%
  pull() %>%
  data.table(name = .)

order <- match(vec.names$name, intersect.metrics$node)
tmp <- intersect.metrics[order]

intersect.graph.final <- intersect.network %>%
  activate(nodes) %>%
  mutate(degree = tmp$degree,
         degree.out = tmp$degree.out, 
         betweenness = tmp$betweenness)

intersect.graph.final <- intersect.graph.final %>%
  activate(nodes) %>%
  mutate(topic = ifelse(name %in% names.both, "both beliefs", "citing"))

# PLOT #########################################################################

set.seed(12)

ggraph(intersect.graph.final, layout = "graphopt") + 
  geom_edge_link(arrow = arrow(length = unit(1.3, 'mm')), 
                 end_cap = circle(1, "mm"), 
                 aes(color = topic), 
                 alpha = 0.3) +
  geom_node_point(aes(color = topic, size = degree)) +
  scale_edge_color_manual(values = c("brown", "blue"), 
                          name = "") +
  scale_size_continuous(range = c(0.5, 2.5)) +
  scale_color_manual(name = "", 
                     values = c("purple", "grey")) +
  labs(x = "", y = "") +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right")

set.seed(12)

ggraph(intersect.graph.final, layout = "graphopt") + 
  geom_edge_link(arrow = arrow(length = unit(1.3, 'mm')), 
                 end_cap = circle(1, "mm"), 
                 aes(color = topic), 
                 alpha = 0.3) +
  geom_node_point(aes(color = topic, size = betweenness)) +
  scale_edge_color_manual(values = c("brown", "blue"), 
                          name = "") +
  scale_size_continuous(range = c(0.5, 2.5)) +
  scale_color_manual(name = "", 
                     values = c("purple", "grey")) +
  labs(x = "", y = "") +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right")


## ----aquastat_analysis-------------------------------------------------------------------

# STUDY OF AQUASTAT PERCENTAGES ################################################

# Read in aquastat dataset -----------------------------------------------------

aquastat.dt <- read.xlsx("aquastat_dt.xlsx") %>%
  data.table() %>%
  .[Year == 2020] %>%
  setnames(., c("Value", "Area"), c("percentage", "country")) %>%
  .[, .(country, percentage)] %>%
  .[, data:= "aquastat 2020"] %>%
  .[, country:= countrycode(country, origin = "country.name", destination = "country.name")] 

aquastat.dt[, continent:= countrycode(country, origin = "country.name", destination = "continent")]
  
# Read in world resources institute dataset ------------------------------------

wri <- fread("world_resources_institut_guide_to_the_global_environment_1994.csv") %>%
  .[order(country)] %>%
  .[, data:= "wri 1994"] %>%
  .[, country:= countrycode(country, origin = "country.name", destination = "country.name")]

wri[, continent:= countrycode(country, origin = "country.name", destination = "continent")]


## ----plot_aquastat_analysis, dependson="aquastat_analysis", fig.height=1.8, fig.width=4----

# Compare distributions --------------------------------------------------------

dt.comparison <- rbind(aquastat.dt, wri) %>%
  .[, data:= factor(data, levels = c("wri 1994", "aquastat 2020"))]

dt.stats.comparison <- dt.comparison[, .(mean = mean(percentage, na.rm = TRUE), 
                                         median = median(percentage, na.rm = TRUE)), data] %>%
  melt(., measure.vars = c("mean", "median"))
  
  
ggplot(dt.comparison, aes(percentage)) +
  geom_histogram(color = "black", fill = "grey") +
  facet_wrap(~data) +
  geom_vline(data = dt.stats.comparison, aes(xintercept = value, color = variable)) +
  scale_color_discrete(name = "") +
  geom_vline(xintercept = 70, lty = 2) +
  theme_AP()


## ----plot_aquastat_analysis_country, dependson="aquastat_analysis", fig.width=3.5--------

# At the country level ---------------------------------------------------------

tmp <- aquastat.dt[wri, on = c("country", "continent")] %>%
  .[, .(country,continent, percentage, i.percentage)] %>%
  setnames(., c("percentage", "i.percentage"), c("aquastat 2020", "wri 1994")) %>%
  melt(., measure.vars = c("aquastat 2020", "wri 1994")) %>%
  .[, country:= ifelse(country == "Trinidad & Tobago", "Trinidad and Tobago", country)] %>%
  na.omit() %>%
  split(., .$continent)

out <- list()

for(i in names(tmp)) {
  
  out[[i]] <- ggplot(tmp[[i]], aes(reorder(country, value), 
                                   value, color = variable)) +
    coord_flip() +
    scale_color_discrete(name = "") +
    geom_point() +
    theme_AP() +
    theme(legend.position = "top") +
    labs(x = "", y = "percentage") +
    ggtitle(names(tmp[i]))
}

out


## ----aquastat_all_years, fig.height=2, fig.width=5, warning=FALSE, dev="pdf"-------------

# AQUASTAT ALL YEARS ############################################################

# DATA FOR THE WATER BELIEF ####################################################

# Read in dataset --------------------------------------------------------------

aquastat.aww <- fread("aquastat.fraction.agriculture.withdrawals.csv")
aquastat.aww.stats <- aquastat.aww[, .(mean = mean(Value, na.rm = TRUE), 
                                       median = median(Value, na.rm = TRUE)), Year] %>%
  melt(., measure.vars = c("mean", "median"))

# Weighted average --------------

aquastat.aww[, weights:= Value / sum(Value), Year]
weighted.average.dt <- aquastat.aww[, .(value = sum(Value * weights)), Year] %>%
  .[, variable:= "weighted \n average"]

# Plot -------------------------------------------------------------------------

a <- aquastat.aww.stats %>%
  rbind(weighted.average.dt) %>%
  ggplot(., aes(Year, value, group = variable, color = variable)) +
  geom_line() +
  scale_color_discrete(name = "") +
  geom_hline(yintercept = 70, lty = 2) +
  theme_AP() +
  labs(x = "Year", y = "Percentage") +
  theme(legend.position = "none")

a

b <- aquastat.aww[, .(above.70 = sum(Value > 70), 
                      below.70 = sum(Value < 70)), Year] %>%
  melt(., measure.vars = c("above.70", "below.70")) %>%
  ggplot(., aes(Year, value, color = variable)) +
  geom_line() +
  theme_AP() +
  scale_color_manual(name = "", labels = c(">70%", "<70%"), 
                     values = c("blue", "orange")) +
  
  labs(x = "Year", y = "Nº countries") +
  theme(legend.position = c(0.78, 0.34))

b

# Fraction of estimated and imputed values -------------------------------------

n.countries <- aquastat.aww[, .(total.countries = .N), Year]
fraction.estimate <- aquastat.aww[, .N, .(Symbol, Year)] %>%
  merge(., n.countries, by = "Year") %>%
  .[, fraction:= N / total.countries] %>%
  ggplot(., aes(Year, N, color = Symbol)) +
  geom_line() +
  labs(x = "Year", y = "Nº countries") +
  scale_color_discrete(name = "") +
  theme_AP() +
  theme(legend.position = c(0.85, 0.25), 
        legend.text = element_text(size = 7))

dt.stats.year <- aquastat.aww.stats %>%
  rbind(weighted.average.dt) %>%
  .[Year == max(Year)]

water.histogram <- aquastat.aww[Year == max(Year)] %>%
  ggplot(., aes(Value)) +
  geom_histogram(color = "black", fill = "grey") +
  geom_vline(xintercept = 70, lty = 2) +
  geom_vline(data = dt.stats.year, aes(xintercept = value, color = variable)) +
  labs(x = "Percentage", y = "Nº countries") + 
  theme_AP() + 
  theme(legend.position = "none")

water.plots.aquastat <- plot_grid(a, water.histogram, b, fraction.estimate, 
                                  ncol = 4, labels = c("b", ""))

# DATA FOR THE FOOD BELIEF #####################################################

dt <- fread("aquastat.fraction.grain.irrigated.csv")

# Check the variable examined --------------------------------------------------

unique(dt$Variable)

# Calculate mean and median ----------------------------------------------------

aquastat.grain.stats <- dt[, .(mean = mean(Value), 
                               median = median(Value), 
                               N.countries = .N), Year] %>%
  melt(., measure.vars = c("mean", "median"))

# Calculated weighted average --------------------------------------------------

dt[, weights:= Value / sum(Value), Year]
weighted.average.dt <- dt[, .(value = sum(Value * weights), 
                              N.countries = .N), Year] %>%
  .[, variable:= "weighted \n average"]

# Plot -------------------------------------------------------------------------

a.crop <- aquastat.grain.stats %>%
  rbind(weighted.average.dt) %>%
  ggplot(., aes(Year, value, group = variable, color = variable)) +
  geom_line() +
  scale_color_discrete(name = "") +
  geom_hline(yintercept = 40, lty = 2) +
  theme_AP() +
  labs(x = "", y = "Percentage") +
  theme(legend.position = c(0.7, 0.38), 
        legend.text = element_text(size = 7))

a.crop

b.crop <- dt[, .(above.40 = sum(Value > 40), 
                 below.40 = sum(Value < 40)), Year] %>%
  melt(., measure.vars = c("above.40", "below.40")) %>%
  ggplot(., aes(Year, value, color = variable)) +
  geom_line() +
  theme_AP() +
  scale_color_manual(name = "", labels = c(">40%", "<40%"), 
                     values = c("blue", "orange")) +
  
  labs(x = "", y = "Nº countries") +
  theme(legend.position = c(0.78, 0.34), 
        legend.text = element_text(size = 7))

b.crop

n.countries.crop <- dt[, .(total.countries = .N), Year]
fraction.estimate <- dt[, .N, .(Symbol, Year)] %>%
  merge(., n.countries, by = "Year") %>%
  .[, fraction:= N / total.countries] %>%
  ggplot(., aes(Year, N, color = Symbol)) +
  geom_line() +
  labs(x = "", y = "Nº countries") +
  scale_color_manual(name = "", values = c("black", "#00BFC4")) +
  theme_AP() +
  theme(legend.position = c(0.85, 0.25), 
        legend.text = element_text(size = 7))

fraction.estimate

dt.stats.year.crops <- aquastat.grain.stats %>%
  rbind(weighted.average.dt) %>%
  .[Year == max(Year)] 

crop.histogram <- dt[Year == max(Year)] %>%
  ggplot(., aes(Value)) +
  geom_histogram(color = "black", fill = "grey") +
  geom_vline(xintercept = 40, lty = 2) +
  geom_vline(data = dt.stats.year.crops, aes(xintercept = value, color = variable)) +
  labs(x = "", y = "Nº countries") + 
  theme_AP() +
  theme(legend.position = "none")

crop.histogram

crop.plots.aquastat <- plot_grid(a.crop, crop.histogram, b.crop, fraction.estimate, 
                                 labels = c("a", ""), ncol = 4)



## ----aquastat_merge_years, dependson="aquastat_all_years", fig.width=6.3, fig.height=3, warning=FALSE, dev="pdf"----

plot_grid(crop.plots.aquastat, water.plots.aquastat, ncol = 1)



## ----past_data_gleick--------------------------------------------------------------------

# ANALYSIS OF PAST DATA NOT CITED ##############################################

# Read in data -----------------------------------------------------------------

sheets <- excel_sheets("Global_Projections_for_Water_Use.xlsx")
past.data <- lapply(sheets, function(sheet) 
  data.table(read.xlsx("Global_Projections_for_Water_Use.xlsx", sheet = sheet)))
names(past.data) <- sheets

# Falkenmark and Lindh 1974 ----------------------------------------------------

past.data$`Falkenmark and Lindh 1974` %>%
  .[, fraction := withdrawal / .[Sector == "Total", withdrawal]] %>%
  .[, fraction2:= `withdrawal.(90%.industrial.reuse)` / .[Sector == "Total", 
                                                          `withdrawal.(90%.industrial.reuse)`]] %>%
  print()

# L'vovich 1974 ----------------------------------------------------------------

past.data$`L'vovich 1974` %>%
  .[sector == "Irrigated agriculture", fraction := withdrawals / 
      .[sector == "TOTAL", withdrawals]] %>%
  print()

# L'vovich 1974b ---------------------------------------------------------------

past.data$`L'vovich 1974b` %>%
  .[sector == "Irrigated agriculture", fraction := withdrawals / 
      .[sector == "TOTAL", withdrawals]] %>%
  print()

# Gleick 1997 ------------------------------------------------------------------

past.data$`gleick 1997`[, total:= agriculture + domestic + industrial] %>%
  .[, fraction.irrigation:= agriculture / total] %>%
  print()

# Seckler et al 1998 -----------------------------------------------------------

past.data$`seckler et al 1998`[, fraction.irrigation:= agricultural / total] %>%
  print()

# Falkenmark and Lindh 1974 ----------------------------------------------------

past.data$`Falkenmark and Lindh 1974` %>%
  .[, fraction := withdrawal / .[Sector == "Total", withdrawal]] %>%
  print()


## ----functions_data----------------------------------------------------------------------

# DEFINE FUNCTIONS #############################################################

# Transform columns to qunif with min and max reflecting uncertainty -----------

transform_columns_fun <- function(mat, distr) {
  
  for (i in 1:nrow(distr)) {
    
    country <- distr$Country[i]
    
    min_val <- distr$min[i]
    max_val <- distr$max[i]
    
    mat[[country]] <- qunif(mat[[country]], min = min_val, max = max_val)
  }
  
  return(mat)
}

## Function to transform longitude and latitude to country.
# It is borrowed from Andy:
# https://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-coords2country = function(points) {

coords2country = function(points) {
  
  countriesSP <- rworldmap::getMap(resolution = 'low')
  pointsSP = sp::SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))
  indices = sp::over(pointsSP, countriesSP)
  indices$ADMIN
  #indices$ISO3 # returns the ISO3 code
  #indices$continent # returns the continent (6 continent model)
  #indices$REGION # returns the continent (7 continent model)
  
}

# Function to retrieve .nc data from Huang et al 2018 --------------------------

get_huang_fun <- function(nc_file, variable, year) {
  
  nc_data <- nc_open(nc_file)
  
  # Extract longitude, latitude, and water withdrawal data (withd_elec)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  withd_elec <- ncvar_get(nc_data, variable)
  time <- ncvar_get(nc_data, "month")
  
  # Close the NetCDF file after reading
  nc_close(nc_data)
  
  # Earth's radius in meters
  earth_radius <- units::set_units(6371000, "m")
  
  # Function to convert degrees to radians
  deg_to_rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  # Calculate the grid area (m²) for each grid cell
  # Create a data.table with lon and lat values
  dt <- data.table(lon = lon, lat = lat)
  
  # Convert latitudes to radians for area calculation
  dt[, lat_rad := deg_to_rad(lat)]
  
  # Assume the grid cells have equal spacing (this should match your grid resolution)
  # If you have grid cell dimensions, use them here. Otherwise, use a 0.5 degree grid as an example.
  delta_lon <- deg_to_rad(0.5)  # Assuming 0.5-degree grid spacing in longitude
  delta_lat <- deg_to_rad(0.5)  # Assuming 0.5-degree grid spacing in latitude
  
  # Calculate the area of each grid cell (in square meters)
  dt[, area_cell := as.numeric(earth_radius^2 * delta_lon * delta_lat * cos(lat_rad))]
  
  # Generate the corresponding year and month for each time step
  start_year <- 1971
  start_month <- 1
  n_months <- length(time)
  
  # Generate year and month for each time step
  years <- rep(start_year:(start_year + (n_months %/% 12)), each = 12, length.out = n_months)
  months <- rep(1:12, length.out = n_months)
  
  # Convert withdrawal from mm/month to km³/month
  for (month_idx in 1:n_months) {
    dt[[paste0("year_", years[month_idx], "_month_", months[month_idx])]] <- as.numeric((withd_elec[, month_idx] * 1e-3 * dt$area_cell / 1e9))
  }
  
  # Select only columns that contain 'year_2010'
  columns_selected <- grep(year, colnames(dt), value = TRUE)
  
  # Create a new data.table with only the relevant columns for 2010
  dt_2010 <- dt[, c("lon", "lat", columns_selected), with = FALSE]
  
  countries <- coords2country(dt_2010)
  
  dada <- cbind(countries, dt_2010) %>%
    melt(., measure.vars = columns_selected) %>%
    .[, sum(value, na.rm = TRUE), countries] %>%
    .[order(countries)] %>%
    .[, Country:= countrycode(countries, origin = "country.name", destination = "country.name")]
  
  
  return(dada)
  
}

# Function to retrieve .nc4 files from ISIMIP ----------------------------------

get_isimip_fun <- function(nc_file, variable, year, start_year) {
  
  nc_data <- nc_open(nc_file)
  
  # Extract longitude, latitude, and water withdrawal data
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  aindww <- ncvar_get(nc_data, variable)
  time <- ncvar_get(nc_data, "time")
  
  # Close the NetCDF file after reading
  nc_close(nc_data)
  
  # Create a grid of lon and lat coordinates
  lon_lat_grid <- expand.grid(lon = lon, lat = lat)
  
  # Earth's radius in meters
  earth_radius <- units::set_units(6371000, "m")
  
  # Function to convert degrees to radians
  deg_to_rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  # Calculate the grid area (m²) for each grid cell
  lon_lat_grid$lat_rad <- deg_to_rad(lon_lat_grid$lat)
  
  # Assume the grid cells have equal spacing (match your grid resolution)
  delta_lon <- deg_to_rad(0.5)  # Assuming 0.5-degree grid spacing in longitude
  delta_lat <- deg_to_rad(0.5)  # Assuming 0.5-degree grid spacing in latitude
  
  # Calculate the area of each grid cell (in square meters)
  lon_lat_grid$area_cell <- as.numeric(earth_radius^2 * delta_lon * delta_lat * cos(lon_lat_grid$lat_rad))
  
  # Convert aindww from kg/m²/s to m³/month
  # 1 kg of water = 1 liter = 1e-3 m³
  # Multiply by the number of seconds in a month (30.44 days average)
  seconds_per_month <- 30.44 * 24 * 3600  # Average month duration in seconds
  
  # Generate the corresponding year and month for each time step
  start_year <- start_year
  n_months <- length(time)
  
  years <- rep(start_year:(start_year + (n_months %/% 12)), each = 12, length.out = n_months)
  months <- rep(1:12, length.out = n_months)
  
  # Convert withdrawal from kg/m²/s to km³/month for each grid cell
  results <- list()
  for (month_idx in 1:n_months) {
    aindww_month <- aindww[, , month_idx]  # Extract the 2D slice (lon x lat) for the given month
    
    # Flatten the 2D data to match the grid
    aindww_month_flat <- as.numeric(aindww_month)
    
    # Calculate water withdrawal in km³/month for each grid cell
    lon_lat_grid[[paste0("year_", years[month_idx], "_month_", months[month_idx])]] <- 
      aindww_month_flat * 1e-3 * lon_lat_grid$area_cell * seconds_per_month / 1e9  # Convert to km³
  }
  
  # Select only columns that contain the selected year
  columns_selected <- grep(year, colnames(lon_lat_grid), value = TRUE)
  
  # Create a new data table with only the relevant columns for the specified year
  dt_selected <- lon_lat_grid[, c("lon", "lat", columns_selected)] %>%
    na.omit()
  
  countries <- coords2country(dt_selected)
  
  result <- cbind(countries, dt_selected) %>%
    data.table() %>%
    melt(., measure.vars = columns_selected) %>%
    .[, sum(value, na.rm = TRUE), countries] %>%
    .[order(countries)] %>%
    .[, Country := countrycode(countries, origin = "country.name", destination = "country.name")]
  
  return(result)
}



## ----define_settings_unc-----------------------------------------------------------------

# DEFINE SETTINGS ##############################################################

N <- 2^20
type <- "QRN"
order <- "first"
matrices <- "A"


## ----gww_unc, dependson=c("define_settings_unc", "functions_data")-----------------------

# UNCERTAINTY IN IRRIGATION ####################################################

# Read dataset -----------------------------------------------------------------

gww <- fread("global_water_withdrawals.csv") 
gww[, Country:= countrycode(Country, origin = "country.name", destination = "country.name")]

countries <- unique(gww$Country) 

# Define sample matrix ---------------------------------------------------------

params <- countries
mat <- data.table(sobol_matrices(matrices = matrices, N = N, params = params, 
                                 order = order, type = type))


# Transform to appropriate distributions ---------------------------------------

mat <- transform_columns_fun(mat = mat, distr = gww)

# Calculate output vector ------------------------------------------------------

y.irrigation <- rowSums(mat)
hist(y.irrigation)


## ----industry_and_domestic, dependson=c("define_settings_unc", "functions_data")---------

# DATASETS WITH INDUSTRY AND DOMESTIC VALUES ###################################

# Aquastat dataset -------------------------------------------------------------

# https://data.apps.fao.org/aquastat/?lang=en&share=f-79eb812e-6cb9-406f-86f2-b7ee414c0ec6

aquastat.ind.dom <- fread("industrial_domestic_aquastat.csv") %>%
  .[, Country:= countrycode(Area, origin = "country.name", destination = "country.name")]

aquastat.tmp <- aquastat.ind.dom[, .(Country, Variable, Value)] %>%
  .[Country %in% countries] %>%
  dcast(., Country ~ Variable, value.var = "Value") %>%
  setnames(., colnames(.)[2:3], c("Industry", "Domestic")) %>%
  .[order(Country)]

# Liu dataset ------------------------------------------------------------------

liu.dt <- data.table(read.xlsx("liu_dataset.xlsx")) %>%
  .[, Country:= countrycode(country, origin = "country.name", destination = "country.name")] %>%
  .[Country %in% countries]

# Gleick dataset ---------------------------------------------------------------

gleick.dt <- data.table(read.xlsx("gleick_table.xlsx"))
colnames_numeric <- colnames(gleick.dt)[-c(1,2)]
gleick.dt <- gleick.dt[, (colnames_numeric):= lapply(.SD, as.numeric), .SDcols = (colnames_numeric)] %>%
  .[, total.irrigation:= (agricultural / 100) * total.withdrawal] %>%
  .[, total.industry:= (industrial / 100) * total.withdrawal] %>%
  .[, total.domestic:= (domestic / 100) * total.withdrawal] %>%
  .[, Country:= countrycode(country, origin = "country.name", destination = "country.name")] %>%
  .[Country %in% countries]


## ----industry_datasets, dependson=c("define_settings_unc", "functions_data")-------------

# INDUSTRY #####################################################################

# Khan et al 2023 dataset ------------------------------------------------------

path.projections <- "./files/khan_et_al_2023/industry"
list.of.files <- list.files(path.projections, pattern = "\\.csv$")
combinations <- lapply(list.of.files, function(x) strsplit(x, "_")[[1]][1:4]) %>%
  do.call(rbind, .) %>%
  data.frame()
colnames(combinations) <- c("SSP", "RCP", "Climate", "Use")

# Create parallel cluster ------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

result <- foreach(i = 1:length(list.of.files),
                  .combine = "rbind",
                  .packages = c("data.table", "countrycode", "tidyverse",
                                "sp", "rworldmap")) %dopar% {
                                  
                                  out <- fread(paste("./files/khan_et_al_2023/industry", 
                                                     list.of.files[i], sep = "/"))
                                  
                                  out <- out[, `:=`(SSP = combinations[i, 1],
                                                    RCP = combinations[i, 2],
                                                    Climate = combinations[i, 3],
                                                    Use = combinations[i, 4])]
                                  
                                  Country <- coords2country(out[1:nrow(out), 2:3])
                                  
                                  df <- cbind(Country, out) %>%
                                    .[, Continent := countrycode(Country, origin = "country.name", 
                                                                 destination = "continent")] %>%
                                    .[, Country:= countrycode(Country, origin = "country.name", 
                                                              destination = "country.name")] %>%
                                    .[, Dataset:= list.of.files[i]]
                                  
                                  df
                                }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)

# Arrange Khan data ------------------------------------------------------------

khan.industry <- result[, sum(`2010`), .(Country, Dataset)] %>%
  .[Country %in% countries] %>%
  .[, .(min = min(V1), max = max(V1)), Country] %>%
  .[order(Country)]

# Huang et al 2018 dataset -----------------------------------------------------

path.projections <- "./files/huang_et_al_2018/industrial"
list.of.files <- list.files(path.projections, pattern = "\\.nc$")
file <- paste("./files/huang_et_al_2018/industrial", 
              list.of.files, sep = "/")
variables <- sub("\\..*", "", list.of.files)

# Retrieve data ----------------------------------------------------------------

out <- list()

for (i in 1:length(file)) {
  
  out[[i]] <- get_huang_fun(nc_file = file[[i]], variable = variables[[i]], year = "year_2010")
}

huang.industry <- data.table(Country = out[[1]]$Country, 
                             industrial.huang = out[[1]]$V1 + 
                               out[[2]]$V1 + 
                               out[[3]]$V1) %>%
  .[Country %in% countries]

# ISIMIP: Create vector with list of files -------------------------------------

path <- "./files/isimip/industrial"
list.of.files <- list.files(path)
files.directory <- paste(path, list.of.files, sep = "/")
variable <- "aindww"
year <- "year_2010"
names.models <- lapply(list.of.files, function(x) sub("_.*", "", x))
start_year <- 1971

# Create parallel cluster -------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

result <- foreach(i = 1:length(files.directory),
                  .packages = c("data.table", "countrycode", "tidyverse",
                                "sp", "rworldmap", "ncdf4")) %dopar% {
                                  
                  start_year <- ifelse(names.models[i] == "cwatm" | names.models[i] == "h08", 1850,
                                       ifelse(names.models[i] == "miroc-integ-land", 1901, 1971))
                                  
                      get_isimip_fun(nc_file = files.directory[i], 
                                     variable = variable, 
                                     year = year, 
                                     start_year = start_year)
                                }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)

# Arrange data -----------------------------------------------------------------

names(result) <- list.of.files

isimip.industry <- rbindlist(result, idcol = "ID") %>%
  .[, .(min = min(V1), max = max(V1)), Country] %>%
  .[Country %in% countries]


# Merge all industry datasets --------------------------------------------------

industry.sample.matrix <- merge(khan.industry, aquastat.tmp[, .(Country, Industry)], 
                                by = "Country") %>%
  merge(., liu.dt[, .(Country, ind)], by = "Country") %>%
  merge(., isimip.industry[, .(Country, min, max)], by = "Country") %>%
  merge(., huang.industry[, .(Country, industrial.huang)], by = "Country") %>%
  merge(., gleick.dt[, .(Country, total.industry)], by = "Country") %>%
  melt(., measure.vars = colnames(.)[-1]) %>%
  .[, .(min = min(value), max = max(value)), Country]

# Define sample matrix ---------------------------------------------------------

params <- industry.sample.matrix$Country

mat <- data.table(sobol_matrices(matrices = matrices, N = N, params = params, 
                                 order = order, type = type))

# Transform to appropriate distributions ---------------------------------------

mat <- transform_columns_fun(mat = mat, distr = industry.sample.matrix)

# Calculate output vector ------------------------------------------------------

y.industry <- rowSums(mat, na.rm = TRUE)
hist(y.industry)
max(y.industry)


## ----domestic_values, dependson=c("define_settings_unc", "functions_data")---------------

# DOMESTIC #####################################################################

# Khan et al 2023 dataset ------------------------------------------------------

path.projections <- "./files/khan_et_al_2023/domestic"
list.of.files <- list.files(path.projections, pattern = "\\.csv$")
combinations <- lapply(list.of.files, function(x) strsplit(x, "_")[[1]][1:4]) %>%
  do.call(rbind, .) %>%
  data.frame()
colnames(combinations) <- c("SSP", "RCP", "Climate", "Use")

# Create parallel cluster -------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

result <- foreach(i = 1:length(list.of.files),
                  .combine = "rbind",
                  .packages = c("data.table", "countrycode", "tidyverse",
                                "sp", "rworldmap")) %dopar% {
                                  
                                  out <- fread(paste("./files/khan_et_al_2023/domestic", 
                                                     list.of.files[i], sep = "/"))
                                  
                                  out <- out[, `:=`(SSP = combinations[i, 1],
                                                    RCP = combinations[i, 2],
                                                    Climate = combinations[i, 3],
                                                    Use = combinations[i, 4])]
                                  
                                  Country <- coords2country(out[1:nrow(out), 2:3])
                                  
                                  df <- cbind(Country, out) %>%
                                    .[, Continent := countrycode(Country, origin = "country.name", 
                                                                 destination = "continent")] %>%
                                    .[, Country:= countrycode(Country, origin = "country.name", 
                                                              destination = "country.name")] %>%
                                    .[, Dataset := list.of.files[i]]
                                  
                                  df
                                }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)

# Arrange Khan data ------------------------------------------------------------

khan.domestic <- result[, sum(`2010`), .(Country, Dataset)] %>%
  .[Country %in% countries] %>%
  .[, .(min = min(V1), max = max(V1)), Country] %>%
  .[order(Country)]

# Huang et al 2018 dataset -----------------------------------------------------

path.projections <- "./files/huang_et_al_2018/domestic"
list.of.files <- list.files(path.projections, pattern = "\\.nc$")
file <- paste(path.projections, 
              list.of.files, sep = "/")
variables <- sub("\\..*", "", list.of.files)

# Retrieve data ----------------------------------------------------------------

huang.domestic <- get_huang_fun(nc_file = file, variable = variables, year = "year_2010") %>%
  .[Country %in% countries]

# ISIMIP: Create vector with list of files -------------------------------------

path <- "./files/isimip/domestic"
list.of.files <- list.files(path)
files.directory <- paste(path, list.of.files, sep = "/")
variable <- "adomww"
year <- "year_2010"
names.models <- lapply(list.of.files, function(x) sub("_.*", "", x))
start_year <- 1971

# Create parallel cluster -------------------------------------------------------

numCores <- detectCores() * 0.75
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Run for loop -----------------------------------------------------------------

result <- foreach(i = 1:length(files.directory),
                  .packages = c("data.table", "countrycode", "tidyverse",
                                "sp", "rworldmap", "ncdf4")) %dopar% {
                                  
                                  start_year <- ifelse(names.models[i] == "miroc-integ-land" | 
                                                         names.models[i] == "cwatm", 1901, 1971)
                                  
                                  get_isimip_fun(nc_file = files.directory[i], 
                                                 variable = variable, 
                                                 year = year, 
                                                 start_year = start_year)
                                }

# Stop the cluster after the computation ---------------------------------------

stopCluster(cl)

# Arrange data -----------------------------------------------------------------

names(result) <- list.of.files

isimip.domestic <- rbindlist(result, idcol = "ID") %>%
  .[, .(min = min(V1), max = max(V1)), Country] %>%
  .[Country %in% countries]


# Merge all domestic datasets --------------------------------------------------

domestic.sample.matrix <- merge(khan.domestic, 
                                aquastat.tmp[, .(Country, Domestic)], 
                                by = "Country") %>%
  merge(., huang.domestic[, .(Country, V1)], by = "Country") %>%
  merge(., liu.dt[, .(Country, muni)], by = "Country") %>%
  merge(., isimip.domestic[, .(Country, min, max)], by = "Country") %>%
  merge(., gleick.dt[, .(Country, total.domestic)], by = "Country") %>%
  melt(., measure.vars = colnames(.)[-1]) %>%
  .[, .(min = min(value), max = max(value)), Country] %>%
  na.omit()

# Define sample matrix ---------------------------------------------------------

params <- domestic.sample.matrix$Country

mat <- data.table(sobol_matrices(matrices = matrices, N = N, params = params, 
                                 order = order, type = type))

# Transform to appropriate distributions ---------------------------------------

mat <- transform_columns_fun(mat = mat, distr = domestic.sample.matrix)

# Calculate output vector ------------------------------------------------------

y.domestic<- rowSums(mat, na.rm = TRUE)
hist(y.domestic)


## ----plot_unc_sectors, dependson=c("domestic_values", "industry_datasets", "gww_unc"), dev = "pdf", fig.height=3, fig.width=4----

# CALCULATE PERCENTAGES ########################################################

percentages.dt <- data.table(irrigation = y.irrigation, 
                             industry = y.industry, 
                             domestic = y.domestic) %>%
  .[, total:= rowSums(.SD)] %>%
  .[, percent.irrig:= (irrigation / total) * 100] %>%
  .[, percent.indus:= (industry / total) * 100] %>%
  .[, percent.domestic:= (domestic / total) * 100]

# Stats ------------------------------------------------------------------------

percentages.dt[, .(min = min(percent.irrig), max = max(percent.irrig))]
quantile(percentages.dt$percent.irrig, probs = c(0.025, 0.975))

# Plot -------------------------------------------------------------------------

vector.sectors <- c("irrigation", "industry", "domestic")

sectors.plot <- melt(percentages.dt, 
                     measure.vars = vector.sectors) %>%
  ggplot(., aes(value)) +
  geom_histogram(color = "black", fill = "grey") + 
  facet_wrap(~variable, scale = "free_x", ncol = 4) + 
  scale_x_continuous(breaks = breaks_pretty(n = 3)) + 
  labs(x = bquote("Km"^3 * "/yr"), y = "Counts") +
  theme_AP()

sectors.plot

total.plot <- percentages.dt %>%
  ggplot(., aes(total)) +
  geom_histogram(color = "black", fill = "grey") + 
  scale_x_continuous(breaks = breaks_pretty(n = 3)) +
  labs(x = bquote("Km"^3 * "/yr"), y = "Counts") +
  theme_AP()

percentages.plot <- percentages.dt[, .(percent.irrig, percent.indus, percent.domestic)] %>%
  setnames(., colnames(.), vector.sectors) %>% 
  melt(., measure.vars = vector.sectors) %>%
  ggplot(., aes(value, fill = variable)) +
  labs(x = "Water withdrawal (%)", y = "Count") +
  scale_fill_discrete(name = "") +
  geom_histogram(alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 70, lty = 2) +
  theme_AP() +
  theme(legend.position = c(0.45, 0.85))

percentages.plot

bottom <- plot_grid(total.plot, percentages.plot, ncol = 2, labels = c("b", "c"))
plot_grid(sectors.plot, bottom, ncol = 1, labels = c("a", ""))


## ----session_information-----------------------------------------------------------------

# SESSION INFORMATION ##########################################################

sessionInfo()

## Return the machine CPU ------------------------------------------------------

cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores -------------------------------------------------

cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads ---------------------------------------------------

cat("Num threads: "); print(detectCores(logical = FALSE))

