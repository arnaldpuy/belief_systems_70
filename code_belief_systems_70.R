## ----setup, include=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ----preliminary, warning=FALSE--------------------------------------------------------------------

#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("openxlsx", "data.table", "tidyverse", "bibliometrix", 
                          "igraph", "ggraph", "cowplot", "tidygraph", "benchmarkme", 
                          "parallel"))

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


## ----load_and_read, warning=FALSE------------------------------------------------------------------

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


## ----abstract_corpus-------------------------------------------------------------------------------

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


## ----full_text_corpus------------------------------------------------------------------------------

# LOAD IN DIMENSIONS DATASET (FULL TEXT) #######################################

# Load dataset of the full text ------------------------------------------------

colnames.full.text <- c("doi", "year", "title", "journal", "topic")
keywords <- c("water", "irrigat")

# Function to load and preprocess data -----------------------------------------

load_and_preprocess_data <- function(file_path, topic) {
  fread(file_path, skip = 1)[, topic := topic]
}

# Load and preprocess water data -----------------------------------------------

dimensions.full.text.water <- load_and_preprocess_data("dimensions_dt_full_text_water_2022_2023.csv", "water")

dimensions.full.text.food <- rbind(
  load_and_preprocess_data("dimensions_dt_full_text_food_2022.csv", "food"),
  load_and_preprocess_data("dimensions_dt_full_text_food_2023.csv", "food")
)

# Combine water and food data --------------------------------------------------

result <- rbind(dimensions.full.text.water, dimensions.full.text.food) %>%
  .[, .(DOI, PubYear, Title, `Source title`, topic)] %>%
  setnames(., colnames(.), colnames.full.text) %>%
  # remove the references that are included in the dataset that 
  # collects mentions in the abstracts 
  .[!.$doi %in% final.dt.water.screened$doi] 

# Create a logical condition for pattern matching using grepl ------------------

pattern_condition <- sapply(keywords, function(keyword) 
  grepl(keyword, result$title, ignore.case = TRUE))

full.text.dt <- result[rowSums(pattern_condition) > 0] 

# Sample just 50% for the analysis ---------------------------------------------

# Create function
random_sample <- function(input_dt) {
  set.seed(123)  # Set a seed for reproducibility
  sampled_dt <- input_dt[sample(.N, .N * 0.5), ]
  return(sampled_dt)
}

# sample
full.text.sampled <- full.text.dt[, random_sample(.SD), topic] 

full.text.sampled[, .N, topic]

# Export -----------------------------------------------------------------------

for (i in c("water", "food")) {
  
  full.text.sampled[topic == i] %>%
    write.xlsx(paste("full.text.corpus", i, "xlsx", sep = "."))
}


## ----policy_corpus, dependson="full_text_corpus"---------------------------------------------------

# LOAD IN DIMENSIONS DATASETS (POLICY TEXT) ####################################

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


## ----split-----------------------------------------------------------------------------------------

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

times.nanxin <- 4
times.arnald <- 2
nanxin <- paste(rep("nanxin", times.nanxin), 1:times.nanxin, sep = "")
arnald <- paste(rep("arnald", times.arnald), 1:times.arnald, sep = "")
names_surveyors <- c(arnald, nanxin, "seth", paste("student", 1:4, sep = ""))
n.surveyors <- length(names_surveyors)

full.text.corpus.water <- read.xlsx("full.text.corpus.water.xlsx")

survey.dt.split <- split_dt_fun(dt = full.text.corpus.water, num_parts = n.surveyors)
names(survey.dt.split) <- names_surveyors

# Export -----------------------------------------------------------------------

for (i in 1:length(survey.dt.split)) {
  
  write.xlsx(survey.dt.split[[i]], 
             file = paste0(names(survey.dt.split)[i], ".dt", ".xlsx"))
  
}


## ----preliminary_analysis_abstract_water-----------------------------------------------------------

# CREATE VECTORS TO READ IN AND CLEAN THE DATASETS #############################

tmp <- list()
names.files <- c("WORK", "NETWORK")
cols_of_interest <- c("title", "author", "claim", "citation")
files.abstract.water <- paste(paste("abstract.corpus.water_", names.files, sep = ""), "xlsx", sep = ".")

# READ IN DATASETS AND TURN TO LOWERCAPS #######################################

for (i in 1:length(files.abstract.water)) {
  
  tmp[[i]] <- data.table(read.xlsx(files.abstract.water[i]))
  
  if (i == 1) {
    
    tmp[[i]][, title:= tolower(title)]
    
  } else {
    
    tmp[[i]][, (cols_of_interest):= lapply(.SD, tolower), .SDcols = (cols_of_interest)]
  }
}

names(tmp) <- names.files

# CLEAN DATASET ################################################################

abstract.water.dt <- merge(tmp[[1]][claim.in.text == "F"], tmp[[2]], by = c("doi", "title"), all = TRUE)
abstract.water.dt[, claim.in.text:= ifelse(is.na(claim.in.text), "TRUE", "FALSE")]
abstract.water.dt[, c(cols_of_interest, "nature.claim"):= lapply(.SD, trimws), .SDcols = c(cols_of_interest, "nature.claim")]
abstract.water.dt[, year:= ifelse(is.na(year), as.numeric(gsub("\\D", "", abstract.water.dt$author)), year)]

## ----plot_bars, dependson="preliminary_analysis_abstract_water", fig.height=2.5, fig.width=4, warning=FALSE----

# PRELIMINARY ANALYSIS #########################################################

a <- tmp$WORK[claim.in.text %in% c("T", "F")] %>%
  .[, .N, claim.in.text] %>%
  ggplot(., aes(claim.in.text, N)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Nº studies") +
  theme_AP()

b <- tmp$NETWORK %>%
  .[complete.cases(.$nature.claim), ] %>%
  .[, nature.claim:= trimws(nature.claim)] %>%
  .[, .N, nature.claim] %>%
  ggplot(., aes(reorder(nature.claim, -N), N)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  geom_bar(stat = "identity") + 
  labs(x = "", y = "") +
  theme_AP()

c <- tmp$NETWORK %>%
  .[, .N, document.type] %>%
  ggplot(., aes(reorder(document.type, -N), N)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = "") +
  theme_AP()

plot_grid(a, b, c, ncol = 3)


## ----network_analysis_water_abstract---------------------------------------------------------------

# NETWORK ANALYSIS #############################################################

# Arrange data -----------------------------------------------------------------
network.dt <- copy(tmp$NETWORK)

# Remove the year from mentions to FAO Aquastat --------------------------------

pattern <- "\\b(?:19|20)\\d{2}\\b"  # Matches years between 1900 and 2099

for (col in c("citation", "author")) {
  matches <- grepl("^fao aquastat\\s+\\d+$", network.dt[[col]], ignore.case = TRUE)
  network.dt[matches, (col) := gsub("\\d+", "", network.dt[[col]][matches], perl = TRUE)]
  network.dt[, (col) := trimws(network.dt[[col]])]
}

setnames(network.dt, c("author", "citation"), c("from", "to"))
network.dt <- network.dt[, .(from, to, document.type, nature.claim)]
cols_to_change <- colnames(network.dt)
network.dt[, (cols_to_change):= lapply(.SD, trimws), .SDcols = (cols_to_change)]

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

network_metrics[order(-degree)][1:5]
network_metrics[order(-betweenness)][1:5]
network_metrics[order(-closeness)][1:5]


## ----plot_network_water_abstract, dependson="network_analysis_water_abstract", dev = "pdf", fig.height=8.5, fig.width=7----

# PLOT NETWORK #################################################################

# Retrieve a vector with the node names ----------------------------------------

graph <- tidygraph::as_tbl_graph(network.dt.complete, directed = TRUE) 
vec.names <- graph %>%
  activate(nodes) %>%
  pull() %>%
  data.table(name = .)

# Merge with info from the network.dt ------------------------------------------

vec.nature.claim <- merge(vec.names, unique(network.dt[, .(from, nature.claim)]), 
      by.x = "name", by.y = "from", all.x = TRUE) 

# Merge with the correct order -------------------------------------------------

order_indices <- match(vec.names$name, vec.nature.claim$name)
final.vec.nature.claim <- vec.nature.claim[order_indices, ] %>%
  .[, nature.claim] 

# Attach to the graph ----------------------------------------------------------

graph <- graph %>%
  activate(nodes) %>%
  mutate(nature.claim = final.vec.nature.claim)

# Plot network -----------------------------------------------------------------

ggraph(graph, layout = "igraph", algorithm = "nicely") + 
  geom_edge_link(arrow = arrow(length = unit(1.5, 'mm')), 
                 end_cap = circle(1, "mm")) + 
  geom_node_point(size = 2, aes(color = nature.claim)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 2.1) +
  labs(x = "", y = "") +
  scale_color_discrete(name = "") +
  theme_AP() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        legend.position = "top") 


## ----session_information---------------------------------------------------------------------------

# SESSION INFORMATION ##########################################################

sessionInfo()

## Return the machine CPU
cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores
cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads
cat("Num threads: "); print(detectCores(logical = FALSE))






#dois <- na.omit(tmp$NETWORK$doi)
#cr_citation_count(doi = dois)
