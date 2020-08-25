
*[![Build Status](https://travis-ci.com/espm-157/fish-hua-lum.svg?token=7gSxV1VqqHz7TUXHHWGp&branch=master)](https://travis-ci.com/espm-157/fish-hua-lum)*

## Team Members:

- Alice Hua, alicehua11
- Julie Lum, julielumy

This repository contains our analysis of the Impacts of Biodiversity Loss on Ocean Ecosystem Services by [Worm et al 2006](http://doi.org/10.1126/science.1132294).

## Analysis

All analysis is in the assignment should be in the `assignment` directory.  You will find this in the `.Rmd` notebook.

## Special files

Our repository also includes the special files found here:

### Common files

- `README.md`: this file, a general overview of the repository in markdown format.  
- `.gitignore`: Optional file, ignore common file types we don't want to accidentally commit to GitHub. Most projects should use this. 
- `<fish-hua-lum>.Rproj`: an R-Project file created by RStudio for it's own configuration.  Some people prefer to `.gitignore` this file.


### Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION`: a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.
- `tests/render_rmds.R`: an R script that is run to execute the above described tests, rendering all `.Rmd` notebooks. 




