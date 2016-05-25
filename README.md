Opinion Centrality v.1.1
=======
*Nodal centrality for multiplex networks*

* Copyright 2015-16 Alexandre Reiffers & Vincent Labatut. 

Opinion Centrality is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/MultiplexCentrality
* Contact: Alexandre Reiffers <alexandre.reiffers@univ-avignon.fr>

-----------------------------------------------------------------------

# Description
This set of `R` scripts was designed for two purposes:

1. Process the opinion centrality, a new centrality measure described in our paper.
2. Compare it to other existing multiplex centrality measures.

Our scripts were applied to a collection of multiplex networks obtained from public sources (see the Data section below).
These data are present in this GitHub project (folder `data`).


# Organization
The `main.R` script can be used to reproduce the whole process: compute the opinion centrality, then compare it to other centralities. 
Note that this script will generate a `plots` folder containing a bunch of files (and it may take a while).

The `model.R` script contains a `process.opinion.centrality` function, which allows the processing of the opinion measure.
Its first parameter is a multiplex network, represented as a list of igraph graphs. 


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/)
   * [`magic`](https://cran.r-project.org/web/packages/magic/index.html)
   * [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html)
   * [`reshape2`](https://cran.r-project.org/web/packages/reshape2/index.html)
   * [`corrplot`](https://cran.r-project.org/web/packages/corrplot/)
3. Download this project from GitHub and unzip.
4. Launch `R`, setup the working directory with `setwd` so that it points at the root of this project. 


# Use
In order to process the opinion measure and compare it to other multiplex centrality measures:

1. Open the `R` console.
2. Set the project root as the working directory, using `setwd("<my directory>")`.
3. Possibly comment/uncomment certain lines in the main script `main.R` (to disable/enable certain operations), then launch it.

The script will produce the following files in the folder `plots`, placed in subfolders whose names correspond to the considered networks:
* `opinion-centrality.csv`: table containing the opinion centrality values.
* `corr_plots`: Spearman's correlation between the opinion centrality and the others measures (and itself with various parameter values). Also contains a CSV file with the actual values.
* `graphs`: aggregated version of the graph, plotted to visually compare the opinion centrality (node color) and some other multiplex measure (node size). The folder also contains a graphml file representing the same graph.
* `histograms`: distribution of the opinion centrality.
* `rank_barplots`: rank difference between the opinion centrality and the other multiplex measures. Each bar represents a node, its height is the rank difference when switching from the considered measure and the opinion centrality, and the nodes are ordered by decreasing rank according the considered measure.
* `rank_lineplots`: also represents the rank difference between the opinion centrality and the other measures, but this time the nodes are ordered by increasing rank difference.


# Extension
You may want to apply the scripts to other networks. If you are just interested in the opinion centrality, directly use the `process.opinion.centrality` function in `model.R`. 
If you also want to perform the comparison, note that the other multiplex measures need to be processed first, using any tool you think is appropriate. 

For our article, we used [MuxViz](http://muxviz.net/), an open source tool by Manlio De Domenico. Note his [personal Web page](http://deim.urv.cat/~manlio.dedomenico/data.php) additionally proposes many multilayer networks to play with.  

When placing new data in the data folder, be sure to respect the same organization and file formats than the networks already present.
Finally, you need to add the appropriate information regarding your new networks in the `data.R` script, and adapt the beginning of `main.R` (and possibly comment the datasets you do not want to process there).


# Dependencies
* [`igraph`](http://igraph.org/r/) package: used to build and handle graphs.
* [`magic`](https://cran.r-project.org/web/packages/magic/index.html) package: used in the gradient function.
* [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/index.html) package: used to generate plots.
* [`reshape2`](https://cran.r-project.org/web/packages/reshape2/index.html) package: used to generate plots. 
* [`corrplot`](https://cran.r-project.org/web/packages/corrplot/) package: used to generate plots.
* [`MuxViz`](http://muxviz.net/) software: used to process other multiplex centrality measures (optional).


# Data
We use data publicly available from two sources:
* [Manlio De Domenico's page](http://deim.urv.cat/~manlio.dedomenico/data.php)
* [Ucinet IV Datasets](http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm)


# References
 * **[RL'15]** A. Reiffers & V. Labatut, *Opinion-Based Centrality in Multiplex Networks: A Convex Optimization Approach*, Submitted, 2015.
