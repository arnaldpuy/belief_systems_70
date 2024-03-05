
#   PRELIMINARY FUNCTIONS ######################################################

sensobol::load_packages(c("openxlsx", "data.table", "tidyverse", "bibliometrix"))

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

# READ IN DIMENSIONS AND WOS DATASETS ##########################################

# dimensions dataset -----------------------------------------------------------

dimensions.dt <- fread("dimensions_dt.csv", skip = 1) %>%
  .[, .(DOI, Authors, PubYear, Title, `Source title`)] 

# WoS dataset ------------------------------------------------------------------

wos.dt <- convert2df(file = "wos_dt.bib",
           dbsource = "wos", 
           format = "bibtex") %>%
  data.table() %>%
  .[, .(DI, AU, PY, TI, SO)]

# scopus dataset ---------------------------------------------------------------

scopus.dt <- fread("scopus.dt.csv") %>%
  .[, .(DOI, Authors, Year, Title, `Source title`)]

# Clean column names and arrange -----------------------------------------------

new_colnames <- c("doi", "authors", "year", "title", "journal")
to_lower <- c("authors", "title", "journal")

list.dt <- list(dimensions.dt, wos.dt, scopus.dt) %>%
  lapply(., function(x) 
    setnames(x, colnames(x), new_colnames) %>%
      .[, (to_lower):= lapply(.SD, tolower), .SDcols = (to_lower)])

cols_to_merge_by <- c("doi", "year", "title", "journal")

survey.dt <- merge(merge(list.dt[[1]], list.dt[[2]], by = cols_to_merge_by, 
                         all = TRUE), list.dt[[3]], by = cols_to_merge_by, 
                   all = TRUE)

# Filer out duplicated studies by doi ------------------------------------------

duplicated.dois <- duplicated(survey.dt$doi, incomparables=NA, na.rm = TRUE)
survey.dt <- survey.dt[!duplicated.dois][, location.belief.system:= "abstract"]

# Write dataset ----------------------------------------------------------------

write.xlsx(survey.dt, "survey.dt.xlsx")

# SPLIT THE DATASET INTO 6 FOR RESEARCH ########################################

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

names_surveyors <- c("arnald", "nanxin", "seth", paste("student", 1:4, sep = ""))
n.surveyors <- length(names_surveyors)

survey.dt.split <- split_dt_fun(dt = survey.dt, num_parts = n.surveyors)
names(survey.dt.split) <- names_surveyors

for (i in 1:length(survey.dt.split)) {
  
  write.xlsx(survey.dt.split[[i]], 
             file = paste0(names(survey.dt.split)[i], ".dt", ".xlsx"))
  
}





duplicated(survey.dt$doi, na.rm = TRUE)




# Create a vector with some repeated elements and NA values
my_vector <- c(1, 2, 3, 1, 4, 5, 2, NA, 7, NA, NA)

# Check for duplicated elements excluding NA values
duplicated_vector <- duplicated(my_vector, fromLast = FALSE, incomparables = NA, na.rm = TRUE)

# Print the original vector and the logical vector indicating duplicates
print(my_vector)
print(duplicated_vector)






dt <- read.xlsx("data_belief_systems_70.xlsx")

                        