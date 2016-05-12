<img src="inst/logo.png" align="left"> 

# txVis - Treatment visualization

An R package for the visualization of patient treatment data.  Treatment vary with time and across patients.  Visualization tools in R provide the opportunity to perform exploratory data analysis, combine or split treatment groups, or improve hypothesis generation.

Clean and interactive visualizations add value to research, and the `txVis` platform, with R, provides the ability to generate reproducible analytic workflows to improve patient outcomes.

## Features

  * Developed using open source tools, for open and reproducible workflows
  
## Installation

`txVis` can be easily installed from GitHub:

```
library(devtools)
devtools::install_github('johnwilsonICON/TxVis')
```

Some of the plotting tools require installation of secondary packages not currently available from the CRAN network and may require additional installations.

## Basic Usage

To use `TxVis` a user must first create a data object.  Once the object is generated it is relatively simple to create clear & beautiful data visualizations for treatment patterns:

```
hlth_data <- create_txVis(patient   = treat$patient, 
                         treatment = treat$treatment,
                         start     = treat$start,
                         end       = treat$end,
                         date_format = "%B %d, %Y")
                         


```
## Contribution

###Bug Reports & Feature Requests

Please use the [issue tracker](https://github.com/johnwilsonICON/TxVis/issues) to report any bugs or file feature requests.

### Developers
We welcome contributions from the community.  Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

To contribute to this project please clone and provide pull requests:

```
git clone git@github.com:johnwilsonICON/TxVis.git
```