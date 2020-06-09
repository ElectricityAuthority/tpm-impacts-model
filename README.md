# Introduction

This repository includes the code for the Authority's TPM impacts model. An earlier Excel version of the model called '2019 Proposal impacts modelling', was originally originally released as part of the [2019 Issues Paper](https://ea.govt.nz/development/work-programme/pricing-cost-allocation/transmission-pricing-review/consultations/). The original Excel version of the model is available via [EMI](https://www.emi.ea.govt.nz/Wholesale/Datasets/_AdditionalInformation/SupportingInformationAndAnalysis/2019/20190723_TPM_2019_IssuesPaper/), along with the data files and vSPD version used to generate the inputs to the model. This version of the model includes further changes made from the February - March 2020 supplementary consultation. The changes from the original model are discussed in the additional [documentation file](documentation/impact_model_documentation.html).

# Setup

This project uses R 3.6.1. [renv](https://rstudio.github.io/renv/) manages the R dependencies of this project. To install the libraries needed to run this project, run the following from the project:

```{r}
source('renv/activate.R')
renv::restore()
```

The data files used as inputs to the model are included in this repository, hence there is no setup required. These are also available from [EMI](https://www.emi.ea.govt.nz/Wholesale/Datasets/_AdditionalInformation/SupportingInformationAndAnalysis/2019/20190723_TPM_2019_IssuesPaper/).

# Running the impacts model

the `rprogs/main.R` file is used to rerun the impacts model. Sourcing this file will regenerate the results files, found in the results directory. For more information on the options available in the main file and the calculations performed, see the additional [documentation file](documentation/impact_model_documentation.html)