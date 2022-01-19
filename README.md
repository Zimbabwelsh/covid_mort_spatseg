This code generates the data files for analyses used in Griffith et al. 2021 (Structural Inequalities in COVID-19 mortality in England and Wales, <a href="https://jech.bmj.com/content/75/12/1165">JECH</a> [JECH]).

The preprint of the paper is available on medrxiv [here]("medrxiv.org/content/10.1101/2021.02.15.21251771v1").

00_data_reshaping.R constructs the dataset reqeuired for the analyses, and contains links to the locations of the data itself. 

fig_generation_revisions.R demonstrates how to take MLwiN (v3.03) extracted MCMC chain output and use them to construct the results (and supplemental results) figures. 

multilevel_example.R contains the code to generate the multilevel example in Figure 1.
