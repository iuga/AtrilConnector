# Atril Connector

Publishing your notebooks from [RStudio](https://www.rstudio.com) to [Atril](https://www.atril.me/) is extremely simple. Once you are satisfied with your work and ready to publish, just click the 'Upload & Publish' button on the Addins dropdown, and follow the instructions. Once published, the post will become available with the given short-link. You can also update previous reports following the same steps.

# Quick Start

## Installation

```r
install.packages("devtools")
devtools::install_github("iuga/AtrilConnector")
```

## Usage

1. Open the notebook you want to publish
2. Open the addin on RStudio, click on "Upload & Publish"
3. Copy & Paste your private token from your [settings page](https://www.atril.me/#/settings)
4. Select the destination community
5. Click on Publish and wait for the final URL
