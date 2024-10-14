[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13885281.svg)](https://doi.org/10.5281/zenodo.13885281)

# The belief in irrigation as essential for food and water security

[Arnald Puy](https://www.arnaldpuy.com/), Seth N. Linga, Samuel Flinders, Bethan Callow, Grace Allen, Beatrice Cross, Nanxin Wei

This study examines the origin, spread and empirical foundations of the belief that 
irrigation agriculture withdraws 70% of all freshwater available and produces 40% 
of all crops consumed worldwide.

## Abstract

*A key tenet in sustainability science is that irrigation agriculture is crucial 
to global food and water security because it produces 40\% of all crops and withdraws 
70\% of all freshwater resources. Here we examine 3.5K documents and demonstrate 
that the percentages supporting these claims have spread through the literature 
by unwarranted amplification, with 60-85\% of all citation paths ending in documents 
that did not produce data supporting the claim or that did not actually make the claim. 
We also show that global crop production and freshwater withdrawals by irrigation are 
far more uncertain, respectively ranging between XXX and 50–90\% given current data, 
and that these ranges are a lower bound on the true uncertainty. Our study highlights 
the need to scrutinize foundational claims in sustainability science and embrace 
uncertainty to foster resilient research and policy-making.*

## Replication

### Data

This repository contains all the data and files produced in this study.

#### Generated data

The files below are necessary to replicate our study. Files with ''water'' relate to 
the 70% belief and those with ''food'' to the 40% belief. Labels like ''abstract'', 
''policy'' or ''full.text'' include references returned by Dimensions after running the
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

The files below contain the nodes and edges for the water and the food belief system network 
respectively. They also include information for every node with regards to the study
making the claim, the type of document, the nature of the claim or the strength of the claim
(see the paper for further information about the classification).

* `water.nodes.xlsx`
* `food.nodes.xlsx`
* `water.edges.xlsx`
* `food.edges.xlsx`

#### Secondary data

We include the estimations pre-2000 mentioned by [Gleick 2000](https://books.google.co.uk/books/about/The_World_s_Water_2000_2001.html?id=b61zOkAs5NcC&redir_esc=y), 
which we discuss in the manuscript, in the file

* `Global_Projections_for_Water_Use.xlsx`

We also enclose in `.csv` the [Aquastat](https://data.apps.fao.org/aquastat/?lang=en) data 
on the percentage of freshwater withdrawn by agriculture and the percentage of grain 
irrigated at the country level, which  are discussed and analyzed in the manuscript.

* `aquastat.fraction.grain.irrigated.csv`
* `aquastat.fraction.agriculture.withdrawals.csv`

As per the data produced by other global models and scholars, we refer the interested reader
to the following links and repositories:

* [Liu et al 2016](https://pubs.acs.org/doi/10.1021/acs.est.6b01065)
* [Huang et al 2018](https://hess.copernicus.org/articles/22/2117/2018/).
* [Khan et al 2023](https://www.nature.com/articles/s41597-023-02086-2)
* [ISI-MIP](https://www.isimip.org/)

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the primary data section are stored for a successful 
compilation.

