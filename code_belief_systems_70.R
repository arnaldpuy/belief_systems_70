## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ----preliminary, warning=FALSE, message=FALSE----------------------------------------------

#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("openxlsx", "data.table", "tidyverse", "bibliometrix", 
                          "igraph", "ggraph", "cowplot", "tidygraph", "benchmarkme", 
                          "parallel", "wesanderson", "scales"))

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
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.title = element_text(size = 9)) 
}


## ----load_and_read, warning=FALSE-----------------------------------------------------------

# CREATION OF VECTORS WITH NAMES ###############################################

database <- c("wos", "scopus", "dimensions")
topic <- c("water", "food")

# Create all possible combinations
combinations <- expand.grid(database = database, topic = topic)

# Combine the vectors with an underscore
file.name <- paste(combinations$database, "dt", combinations$topic, sep = "_")

# READ IN THE DATA #############################################################

# Loop to create the file names ------------------------------------------------

for (i in 1:length(file.name)) {
  
  database.type <- str_extract(file.name, "^(wos|scopus|dimensions)")
  
  if(isTRUE(database.type[i] == "wos")) {
    
    file.name[i] <- paste(file.name[i], "bib", sep = ".")
    
  } else {
    
    file.name[i] <- paste(file.name[i], "csv", sep = ".")
  }
  
}

# vector with new column names -------------------------------------------------

new_colnames <- c("doi", "authors", "year", "title", "journal", "abstract", "database")
to_lower <- c("authors", "title", "journal", "abstract")

# Loop to read in the datasets -------------------------------------------------

out <- list()

for (i in 1:length(file.name)) {
  
  database.type <- str_extract(file.name[i], "^(wos|scopus|dimensions)")
  
  if(isTRUE(database.type == "wos")) {
    
    out[[i]] <- convert2df(file = file.name[i],
               dbsource = "wos", 
               format = "bibtex") %>%
      data.table() %>%
      .[, .(DI, AU, PY, TI, SO, AB)] %>%
      .[, database:= "wos"]
    
  } else if (isTRUE(database.type == "dimensions")) {
    
    out[[i]] <- fread(file.name[i], skip = 1) %>%
      .[, .(DOI, Authors, PubYear, Title, `Source title`, Abstract)] %>%
      .[, database:= "dimensions"]
    
  } else if(isTRUE(database.type == "scopus")) {
    
    out[[i]] <- fread(file.name[i]) %>%
      .[, .(DOI, Authors, Year, Title, `Source title`, Abstract)] %>%
      .[, database:= "scopus"]
  }
  
  setnames(out[[i]], colnames(out[[i]]), new_colnames) %>%
    .[, (to_lower):= lapply(.SD, tolower), .SDcols = (to_lower)] %>%
    .[, abstract:= sub("references.*", "", abstract)]
  
}

names(out) <- combinations$topic

# CLEAN THE DATASETS ###########################################################

# Arrange ----------------------------------------------------------------------

dt <- rbindlist(out, idcol = "topic")

tmp <- split(dt, list(dt$topic, dt$database))

cols_to_merge_by <- c("doi", "year", "title", "journal", "abstract")

dt.water <- merge(merge(tmp$water.dimensions, tmp$water.scopus, by = cols_to_merge_by, 
            all = TRUE), tmp$water.wos, by = cols_to_merge_by, 
      all = TRUE)

dt.food <- merge(merge(tmp$food.dimensions, tmp$food.scopus, by = cols_to_merge_by, 
            all = TRUE), tmp$food.wos, by = cols_to_merge_by, 
      all = TRUE)

# Filer out duplicated studies by doi ------------------------------------------

tmp.list <- list(dt.water, dt.food)
duplicated.dois <- final.dt <- list()

for (i in 1:length(tmp.list)) {
  
  duplicated.dois[[i]] <- duplicated(tmp.list[[i]]$doi, incomparables = NA, na.rm = TRUE)
  final.dt[[i]] <- tmp.list[[i]][!duplicated.dois[[i]]][, location.belief.system:= "abstract"]
  
}

names(final.dt) <- topic

# Check if there is any duplicated doi -----------------------------------------

any(duplicated(final.dt$food$doi, na.rm = TRUE, incomparables = NA))

# Export to xlsx ---------------------------------------------------------------

for (i in names(final.dt)) {
  
  write.xlsx(final.dt[[i]][, .(doi, year, title, abstract, location.belief.system)], 
             paste("final.dt", names(final.dt[i]), "xlsx", sep = "."))
}


## ----abstract_corpus------------------------------------------------------------------------

final.dt.water.screened <- data.table(read.xlsx("final.dt.water_screened.xlsx"))
final.dt.food.screened <- data.table(read.xlsx("final.dt.food_screened.xlsx"))
screened.dt <- list(final.dt.water.screened, final.dt.food.screened)
names(screened.dt) <- c("water", "food")

lapply(screened.dt, function(x) x[, .N, screening])

# Export for close-reading only the references that do include
# the belief system in the abstract --------------------------------------------

for (i in names(screened.dt)) {
  
  screened.dt[[i]][screening == "T"] %>%
    unique(., by = "title") %>%
    .[, .(doi, title, year)] %>%
    write.xlsx(., paste("abstract.corpus", i, "xlsx", sep = "."))
  
}


## ----policy_corpus, dependson="full_text_corpus"--------------------------------------------

# LOAD IN DIMENSIONS DATASETS (POLICY TEXT) ####################################

# Function to load and preprocess data -----------------------------------------

load_and_preprocess_data <- function(file_path, topic) {
  fread(file_path, skip = 1)[, topic := topic]
}

colnames.full.text <- c("doi", "year", "title", "journal", "topic")
keywords <- c("water", "irrigat")

# Load data --------------------------------------------------------------------

dt.policy.water <- load_and_preprocess_data("dimensions_dt_policy.csv", "water")
dt.policy.food <- load_and_preprocess_data("dimensions_dt_policy_food.csv", "food")

dimensions.full.text.policy <- rbind(dt.policy.food, dt.policy.water) %>%
  .[, .(`Policy document ID`, PubYear, Title, `Publishing Organization`, 
        `Sustainable Development Goals`, `Source Linkout`, topic)]

dimensions.full.text.policy[, .N, topic]

# Create a logical condition for pattern matching using grepl
pattern_condition_policy <- sapply(keywords, function(keyword) 
  grepl(keyword, dimensions.full.text.policy$Title, ignore.case = TRUE))

# Combine conditions with OR using rowSums
matching.rows.policy <- dimensions.full.text.policy[rowSums(pattern_condition_policy) > 0]

matching.rows.policy[, .N, topic]

# Export -----------------------------------------------------------------------

for (i in c("water", "food")) {
  
  matching.rows.policy[topic == i] %>%
    write.xlsx(paste("policy.corpus", i, "xlsx", sep = "."))
}


## ----full_text_corpus-----------------------------------------------------------------------

# LOAD IN DIMENSIONS DATASET (FULL TEXT) #######################################

full.text.corpus.water <- fread("full.text.corpus.water.csv")


## ----split----------------------------------------------------------------------------------

# SPLIT THE DATASET INTO N FOR RESEARCH ########################################

# Function to split dataset in n chunks ----------------------------------------

split_dt_fun <- function(dt, num_parts) {
  
  split_dt <- list()
  
  # Calculate the number of rows in each part
  rows_per_part <- nrow(dt) %/% num_parts
  
  # Split the data.table into roughly equal parts
  for (i in 1:num_parts) {
    
    start_row <- (i - 1) * rows_per_part + 1
    end_row <- i * rows_per_part
    
    if (i == num_parts) {
      
      end_row <- nrow(dt)
    }
    split_dt[[i]] <- dt[start_row:end_row, ]
  }
  
  return(split_dt)
  
}

# Create the datasets for close reading ----------------------------------------

times.nanxin <- 2
times.arnald <- 1
nanxin <- paste(rep("nanxin", times.nanxin), 1:times.nanxin, sep = "")
arnald <- paste(rep("arnald", times.arnald), 1:times.arnald, sep = "")
names_surveyors <- c(arnald, nanxin, "seth", paste("student", 1:4, sep = ""))
n.surveyors <- length(names_surveyors)

survey.dt.split <- split_dt_fun(dt = full.text.corpus.water, num_parts = n.surveyors)
names(survey.dt.split) <- names_surveyors

# Export -----------------------------------------------------------------------

for (i in 1:length(survey.dt.split)) {
  
  write.xlsx(survey.dt.split[[i]], 
             file = paste0(names(survey.dt.split)[i], ".dt", ".xlsx"))
  
}


## ----read_all_datasets, dependson=c("abstract_corpus", "full_text_corpus", "policy_corpus", "split")----

# CREATE VECTORS TO READ IN AND CLEAN THE DATASETS #############################

tmp <- list()
names.files <- c("WORK", "NETWORK")
topics <- c("water")
corpus <- c("abstract.corpus", "policy.corpus", "full.text.corpus") 
cols_of_interest <- c("title", "author", "claim", "citation")

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


# CLEAN AND MERGE DATASETS #####################################################

dataset.networks <- all.files[str_detect(all.files, "NETWORK")]
network.dt <- tmp[dataset.networks] %>%
  rbindlist() %>%
  .[, policy:= grepl("^policy", doi)]

network.dt[, author:= ifelse(policy == TRUE, doi, author)]

# CHECK NUMBER OF FAO AQUASTAT CITES ###########################################

network.dt[citation %like% "fao aquastat"] %>%
  .[, .N, citation]

# WRITE LOOKUP TABLE TO CHECK ALREADY RETRIEVED STUDIES ########################

lookup.dt <- network.dt[, .(doi, title, author)] %>%
  .[order(title)] %>%
  unique(.) 

nrow(lookup.dt)

write.xlsx(lookup.dt, "lookup.dt.xlsx")

# Remove the year from mentions to FAO Aquastat --------------------------------

pattern <- "\\b(?:19|20)\\d{2}\\b"  # Matches years between 1900 and 2099

for (col in c("citation", "author")) {
  matches <- grepl("^fao aquastat\\s+\\d+$", network.dt[[col]], ignore.case = TRUE)
  network.dt[matches, (col) := gsub("\\d+", "", network.dt[[col]][matches], perl = TRUE)]
  network.dt[, (col) := trimws(network.dt[[col]])]
}

# Rename columns ---------------------------------------------------------------

setnames(network.dt, c("author", "citation"), c("from", "to"))

# Create copy and remove duplicated --------------------------------------------

network.dt.claim <- copy(network.dt)
network.dt.claim <- unique(network.dt.claim, 
                           by = c("from", "to", "document.type", "nature.claim"))

fwrite(network.dt.claim, "network.dt.claim.csv")

# Convert all to lower caps ----------------------------------------------------

network.dt <- network.dt[, .(from, to, document.type, nature.claim)]
cols_to_change <- colnames(network.dt)
network.dt[, (cols_to_change):= lapply(.SD, trimws), .SDcols = (cols_to_change)]


## ----descriptive_plots, dependson="read_all_datasets", fig.height=1.8, fig.width=6.5--------

# PLOT DESCRIPTIVE STATISTICS ##################################################

total.rows <- nrow(network.dt)

# Check proportion of studies by nature of claim -------------------------------

network.dt.claim[, .N, nature.claim] %>%
  .[, total:= total.rows] %>%
  .[, fraction:= N / total] %>%
  print()

# Count document type by nature of claim ---------------------------------------

a <- network.dt[, .N, .(nature.claim, document.type)] %>%
  .[, total.rows:= total.rows] %>%
  .[, proportion:= N / total.rows] %>%
  na.omit() %>%
  ggplot(., aes(reorder(nature.claim, proportion), proportion)) +
  coord_flip() +
  geom_bar(stat = "identity") + 
  facet_wrap(~document.type) +
  scale_y_continuous(breaks = breaks_pretty(n = 2)) +
  labs(x = "", y = "Fraction") +
  theme_AP()

# Count how many documents make the claim and cite / do not cite, 
# by document.type -------------------------------------------------------------

b <- network.dt[, .(without.citation = sum(is.na(to)), 
               with.citation = .N - sum(is.na(to))), document.type] %>%
  melt(., measure.vars = c("without.citation", "with.citation")) %>%
  .[, total.rows:= total.rows] %>%
  .[, proportion:= value / total.rows] %>%
  ggplot(., aes(document.type, proportion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = breaks_pretty(n = 2)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = "Fraction") +
  facet_wrap(~variable) + 
  theme_AP()

# merge ------------------------------------------------------------------------

plot_grid(a, b, ncol = 2, rel_widths = c(0.63, 0.37), labels = "auto")


## ----citations_support_claim, dependson="read_all_datasets", fig.height=1.8, fig.width=2.2, warning=FALSE----

# PLOT DISTRIBUTION OF CITATION SUPPORTING THE CLAIM ###########################

network.dt[, .N, from] %>%
  .[order(-N)] %>%
  ggplot(., aes(N)) +
  geom_histogram() + 
  theme_AP() +
  labs(x = "Nº citations supporting claim \n per paper", y = "Counts")


## ----network_metrics, dependson="read_all_datasets"-----------------------------------------

# CALCULATE NETWORK METRICS ####################################################

# only complete cases ----------------------------------------------------------

network.dt.complete <- network.dt[complete.cases(network.dt$to), ]

# Transform to graph -----------------------------------------------------------

citation_graph <- graph_from_data_frame(d = network.dt.complete, directed = TRUE)

# Calculate network metrics ----------------------------------------------------

edge_density(citation_graph)

# Modularity: 
# - c.1: Strong community structure, where nodes within groups are highly connected.
# - c. -1: Opposite of community structure, where nodes between groups are more connected.
# - c. 0: Indicates absence of community structure or anti-community structure in the network.
wtc <- cluster_walktrap(citation_graph)
modularity(wtc)

network_metrics <- data.table(node = V(citation_graph)$name,
                              
                              # Degree of a node: The number of connections or 
                              # edges linked to that node. 
                              # It represents how well-connected or central a 
                              # node is within the graph.
                              degree = degree(citation_graph, mode = "in"),
                              
                              degree.out = degree(citation_graph, mode = "out"),
                              
                              # Betweenness centrality of a node: Measures the 
                              # extent to which a node lies on the shortest 
                              # paths between all pairs of other nodes in the graph. 
                              # Nodes with high betweenness centrality act as 
                              # bridges or intermediaries, facilitating 
                              # communication and information flow between other nodes.
                              betweenness = betweenness(citation_graph),
                              
                              # Closeness centrality of a node: Measures how 
                              # close a node is to all other nodes in the graph, 
                              # taking into account the length of the shortest paths. 
                              # Nodes with high closeness centrality are able to 
                              # efficiently communicate or interact with other 
                              # nodes in the graph.
                              closeness = closeness(citation_graph),
                              pagerank = page_rank(citation_graph)$vector
)

# Define the max number of rows
max.number <- 3

degree.nodes <- network_metrics[order(-degree)][1:max.number]
degree.nodes.out <- network_metrics[order(-degree.out)][1:max.number]
betweenness.nodes <- network_metrics[order(-betweenness)][1:max.number]
pagerank.nodes <- network_metrics[order(-closeness)][1:max.number]

degree.nodes
degree.nodes.out
betweenness.nodes
pagerank.nodes


## ----add_features, dependson=c("read_all_datasets", "network_metrics")----------------------

# ADD FEATURES TO NODES ########################################################

# Retrieve a vector with the node names ----------------------------------------

graph <- tidygraph::as_tbl_graph(network.dt.complete, directed = TRUE) 
vec.names <- graph %>%
  activate(nodes) %>%
  pull() %>%
  data.table(name = .)

# Merge with info from the network.dt ------------------------------------------

vec.nature.claim <- merge(merge(vec.names, unique(network.dt[, .(from, nature.claim)]), 
                                by.x = "name", by.y = "from", all.x = TRUE), 
                          unique(network.dt[, .(from, document.type)]), 
                          by.x = "name", by.y = "from", all.x = TRUE)

# Merge with the correct order -------------------------------------------------

order_indices <- match(vec.names$name, vec.nature.claim$name)
final.vec.nature.claim <- vec.nature.claim[order_indices, ] %>%
  .[, nature.claim] 
final.vec.document.type <- vec.nature.claim[order_indices, ] %>%
  .[, document.type] 

# Attach to the graph ----------------------------------------------------------

graph <- graph %>%
  activate(nodes) %>%
  mutate(nature.claim = final.vec.nature.claim, 
         document.type = final.vec.document.type, 
         degree = network_metrics$degree, 
         degree.out = network_metrics$degree.out,
         betweenness = network_metrics$betweenness, 
         pagerank = network_metrics$pagerank)


## ----calculate_proportion, dependson="add_features"-----------------------------------------

# NUMBER OF NODES ##############################################################

V(graph)

# NUMBER OF EDGES ##############################################################

ecount(graph)

# PROPORTION OF ALL PATHS THAT PASS THROUGH FIVE HIGHEST BETWEENNESS NODES ######

bc <- betweenness(graph)
nodes_of_interest <- sort(bc, decreasing = TRUE)[1:5] 
total_paths <- choose(vcount(graph), 2)  # Total number of paths
total_paths
sum(nodes_of_interest) / total_paths

# PROPORTION OF LINKS CONNECTED TO THE 5 NODES WITH HIGHEST DEGREE #############

dg <- degree(graph)
nodes_of_interest_degree <- sort(dg, decreasing = TRUE)[1:5] 
total_edges <- ecount(graph)  # Total number of edges
sum(nodes_of_interest_degree) / total_edges



## ----plot_network, dependson="add_features", fig.height=6, fig.width=7----------------------

# PLOT NETWORK #################################################################

seed <- 123

# by nature of claim -----------------------------------------------------------

set.seed(seed)

# Label the nodes with highest degree ------------------------------------------

ggraph(graph, layout = "igraph", algorithm = "nicely") + 
  geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                 end_cap = circle(1, "mm")) + 
  geom_node_point(aes(color = nature.claim, size = degree)) +
  geom_node_text(aes(label = ifelse(degree >= min(degree.nodes$degree), name, NA)), 
                 repel = TRUE, size = 2.2) +
  labs(x = "", y = "") +
  scale_color_manual(name = "", 
                     values = wes_palette(name = "Cavalcanti1", 5)) +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right") 

set.seed(seed)

# Label the nodes with highest betweenness -------------------------------------

ggraph(graph, layout = "igraph", algorithm = "nicely") + 
  geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                 end_cap = circle(1, "mm")) + 
  geom_node_point(aes(color = nature.claim, size = betweenness)) +
  geom_node_text(aes(label = ifelse(betweenness >= min(betweenness.nodes$betweenness), name, NA)), 
                 repel = TRUE, size = 2.2) +
  labs(x = "", y = "") +
  scale_color_manual(name = "", 
                     values = wes_palette(name = "Cavalcanti1", 5)) +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right") 

# by document.type--------------------------------------------------------------

set.seed(seed)

ggraph(graph, layout = "igraph", algorithm = "nicely") + 
  geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                 end_cap = circle(1, "mm")) + 
  geom_node_point(aes(color = document.type, size = degree)) +
  geom_node_text(aes(label = ifelse(degree >= min(degree.nodes$degree), name, NA)), 
                 repel = TRUE, size = 2.2) +
  labs(x = "", y = "") +
  scale_color_discrete(name = "") +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right") 

# Label nodes that are modelling exercises -------------------------------------

set.seed(seed)

ggraph(graph, layout = "igraph", algorithm = "nicely") + 
  geom_edge_link(arrow = arrow(length = unit(1.8, 'mm')), 
                 end_cap = circle(1, "mm")) + 
  geom_node_point(aes(color = nature.claim)) +
  geom_node_text(aes(label = ifelse(nature.claim == "modelling", name, NA)), 
                 repel = TRUE, size = 2.2) +
  labs(x = "", y = "") +
  scale_color_manual(name = "", 
                     values = wes_palette(name = "Cavalcanti1", 5)) +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "right") 


## ----analysis_network_paths, dependson="add_features"---------------------------------------

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


## ----analysis_paths_nodes, dependson="analysis_network_paths"-------------------------------

# CALCULATE

# Extract name of all nodes ----------------------------------------------------

all_nodes <- graph %>%
  activate(nodes) %>%
  pull(name)

# Extract name of nodes that do not make the claim -----------------------------

no.claim_nodes <- graph %>%
  activate(nodes) %>%
  filter(degree.out == 0 & nature.claim == "no claim") %>%
  pull(., "name")

# Extract name of nodes that do not make the claim and those that make 
# the claim but do not cite anybody --------------------------------------------

no.claim.and.no.citation.nodes <- graph %>%
  activate(nodes) %>%
  filter(degree.out == 0 & nature.claim == "no claim" | nature.claim == "no citation" ) %>%
  pull(., "name")

# Run the function -------------------------------------------------------------

out <- lapply(list(no.claim_nodes, no.claim.and.no.citation.nodes), function(x)
  sort(nodes_to_no_claim_node_fun(graph, terminal_nodes = x)))

names(out) <- c("path ending in no claim", "path ending in no claim or no citation")
out

# Calculate proportions --------------------------------------------------------

lapply(out, function(x) length(x) / length(all_nodes))


## ----session_information--------------------------------------------------------------------

# SESSION INFORMATION ##########################################################

sessionInfo()

## Return the machine CPU
cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores
cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads
cat("Num threads: "); print(detectCores(logical = FALSE))

