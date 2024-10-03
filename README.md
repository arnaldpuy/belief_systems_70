
# The belief in irrigation as essential for food and water security

[Arnald Puy](https://www.arnaldpuy.com/), Seth N. Linga, Samuel Flinders, Bethan Callow, Grace Allen, Beatrice Cross, Nanxin Wei

This study examines the origin, spread and empirical foundations of the belief that irrigation agriculture withdraws 70% of all 
freshwater available and produces 40% of all crops consumed worldwide.

## Abstract

In progress

## Replication

### Data

This repository contains all the data and files produced in this study.

#### Primary data

The files below are necessary to replicate our study. Files with ''water'' relate to 
the 70% belief and those with ''food'' to the 40% belief. Labels like ''abstract'', 
''policy'' or ''full.text'' include references returned by dimensions after running the
search queries in abstracts/keywords/title, in policy documents only or in the 
full text. Files ending in ''_WORK.xlsx'' list all surveyed  studies (including 
false positives), while ''_NETWORK.xlsx'' contains documents with or cited for the 
claim, forming the belief network.

* `abstract.corpus.water_WORK.xlsx`   
* `policy.corpus.water_WORK.xlsx`      
* `full.text.corpus.water_WORK.xlsx`  

* `abstract.corpus.water_NETWORK.xlsx` 
* `policy.corpus.water_NETWORK.xlsx`  
* `full.text.corpus.water_NETWORK.xlsx` 

* `abstract.corpus.food_WORK.xlsx`     
* `policy.corpus.food_WORK.xlsx`       
* `full.text.corpus.food_WORK.xlsx`    

* `abstract.corpus.food_NETWORK.xlsx`  
* `policy.corpus.food_NETWORK.xlsx`     
* `full.text.corpus.food_NETWORK.xlsx`

#### Generated data

The files below contain the list of nodes and edges for the water and the food belief system network 
respectively.

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the results replicated from either of these files.
The user must run the code from the same folder where the files in the primary data section are stored for a successful compilation.
