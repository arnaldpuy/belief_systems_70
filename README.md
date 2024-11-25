[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13885281.svg)](https://doi.org/10.5281/zenodo.13885281)

# Widely cited global irrigation statistics lack empirical support

[Arnald Puy](https://www.arnaldpuy.com/), Seth N. Linga, Samuel Flinders, Bethan Callow, 
Grace Allen, Beatrice Cross, Bruce Lankford, Carmen Aguiló, Nanxin Wei

This study examines the origin, spread and empirical foundations of the belief that 
irrigation agriculture withdraws 70% of all freshwater available and produces 40% 
of all crops consumed worldwide.

## Abstract

*A prevailing notion in sustainability science is that irrigation agriculture underpins 
global food and water security because it accounts for 40% of crop production and 70% 
of freshwater withdrawals. Through a network citation analysis of 3,500 documents, 
we reveal that this belief has spread through the literature with minimal empirical 
support: 60–80% of all citation paths lead to sources that lack supporting data or 
that do not even contain the 40% or 70% numbers. We also prove that these figures 
mask a much more uncertain contribution of irrigation to global crop production 
and water withdrawals, which can lie anywhere between 18–50% and 45–90% respectively. 
These ranges should be understood as lower bounds on the true uncertainty. 
Our findings underscore the need to rigorously evaluate foundational claims in 
sustainability science and embrace ambiguity to produce robust research and policy-making.*

## Replication

### Data

This repository contains the data and files produced in this study.

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

For irrigation, domestic and industrial water withdrawal:

* [Liu et al 2016](https://pubs.acs.org/doi/10.1021/acs.est.6b01065)
* [Huang et al 2018](https://hess.copernicus.org/articles/22/2117/2018/)
* [Khan et al 2023](https://www.nature.com/articles/s41597-023-02086-2)
* [ISI-MIP](https://www.isimip.org/)
* [Gleick 2000](https://books.google.co.uk/books?id=b61zOkAs5NcC&printsec=copyright&redir_esc=y#v=onepage&q&f=false)
* [Flörke et al 2013](https://www.sciencedirect.com/science/article/abs/pii/S0959378012001318)

For irrigated areas:

* [Siebert et al 2005](https://hess.copernicus.org/articles/9/535/2005/) 
* [Meier et al 2018](https://hess.copernicus.org/articles/22/1119/2018/)
* [Salmon et al 2015](https://www.sciencedirect.com/science/article/abs/pii/S0303243415000240?via%3Dihub)
* [Thenkabail et al 2006](https://www.iwmi.org/Publications/IWMI_Research_Reports/PDF/pub105/RR105.pdf)

For agricultural land:

* [Tubiello et al 2023](https://essd.copernicus.org/articles/15/4997/2023/)
* [Potapov et al 2021](https://www.nature.com/articles/s43016-021-00429-z)

For average production of wheat / maize:

* [Dadrasi et al 2023](https://www.nature.com/articles/s41598-023-43191-x)
* [FAO's GAEZ](https://gaez.fao.org/pages/crop-summary)
* [Mueller et al 2012](https://www.nature.com/articles/nature11420)
* [IFPRI 2024](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SWPENT)

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the primary data section are stored for a successful 
compilation.

