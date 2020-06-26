# HTScapacity
An R package for analyzing assay capacity in HTS studies

## HTScapacity can be installed from github

install.packages("devtools") # If not already installed

devtools::install_github("xhDouglas/HTScapacity")

## Data

Related datasets are stored in Excel files in the subdirectory "extdata". 

These data may also be obtained through running the following command lines in R after installing HTScapacity

  library(HTScapacity)

  data("data.mucinHTS", package="HTScapacity")

  data("data.CVB3CRISPR", package="HTScapacity")

  data("data.hcvHTS", package="HTScapacity")

The mucin RNAi and HCV RNAi screens were published in 

Zhang, X. D. et al. The use of strictly standardized mean difference for hit selection in primary RNA interference high-throughput screening experiments. Journal of biomolecular screening 12, 497-509 (2007).

Zhang, X. D. Optimal High-Throughput Screening: Practical Experimental Design and Data Analysis for Genome-Scale RNAi Research.  (Cambridge University Press, 2011).
 

The CVB3 CRISPR/CAS9 screen was published originally in 

Kim, H. S. et al. Arrayed CRISPR screen with image-based assay reliably uncovers host genes required for coxsackievirus infection. Genome Res 28, 859-868 (2018).


The codes for generating the figures and tables in the paper for assay capacity are stored in the subdirectory "Code.main" in the package.

## Functions

HTScapacity contains the following major functions.

  capacityPlot.homoVAR.fn

  capacityPlot.homoVAR.simple.fn

  FoldHit.homoVAR.plot.fn

  FoldHit.homoVAR.UMVUE.fn

  FoldHit0.homoVAR.fn

  FoldHit0core.homoVAR.fn

  FoldHitC.homoVAR.fn

  zFactor.fn

## Author

  Xiaohua Douglas Zhang, Ph.D., Professor, University of Macau and University of Kentucky

## Paper
  Xiaohua Douglas Zhang. Assay capacity: a new paradigm for assessing and controlling quality in high-throughput screening studies (2020)


