<table>
<tr>
<td><img src="img/mlba-bookcover.png" width=300></td>
<td>
<h1>Machine Learning for Business Analytics<br>
<small>Concepts, Techniques, and Applications in R</small></h1>

by Galit Shmueli, Peter C. Bruce, Peter Gedeck, Inbal Yahav, Nitin R. Patel

Publisher: Wiley; 2nd edition (February, 2023)
ISBN: 978-1-118-83517-2
Buy at
<a href="https://www.amazon.com/Machine-Learning-Business-Analytics-Applications/dp/1119835178/?&_encoding=UTF8&tag=petergedeck-20&linkCode=ur2&linkId=681b98b51c3d935ffec95c8284e2e0bc&camp=1789&creative=9325">Amazon</a>
or
<a href="https://www.wiley.com/en-us/Machine+Learning+for+Business+Analytics%3A+Concepts%2C+Techniques%2C+and+Applications+in+R%2C+2nd+Edition-p-9781119835172">Wiley</a>

<!-- Errata: http://oreilly.com/catalog/errata.csp?isbn=9781492072942 -->
</td>
</tr>
</table>

# Description
Machine learning —also known as data mining or data analytics— is a fundamental part of data science. It is used by organizations in a wide variety of arenas to turn raw data into actionable information.

Machine Learning for Business Analytics: Concepts, Techniques, and Applications in R provides a comprehensive introduction and an overview of this methodology. This best-selling textbook covers both statistical and machine learning algorithms for prediction, classification, visualization, dimension reduction, rule mining, recommendations, clustering, text mining, experimentation and network analytics. Along with hands-on exercises and real-life case studies, it also discusses managerial and ethical issues for responsible use of machine learning techniques.

# Source code, datasets, and instructors material
This repository contains:

- `Rmd`: R code of individual chapters as 
  [R markdown files](https://github.com/gedeck/mlba-R-code/tree/main/Rmd) - 
  download all as [mlba-Rmd.zip](mlba-Rmd.zip)
- `R`: R code of individual chapters as plain R
  [R files](https://github.com/gedeck/mlba-R-code/tree/main/R) - 
  download all as [mlba-R.zip](mlba-R.zip)

The datasets are distributed using the [mlba](https://github.com/gedeck/mlba) package; see below for installation instructions. 
To find instructors material go to [www.dataminingbook.com](https://www.dataminingbook.com/book/r-2nd-edition-2022). 


# Installation of R packages used in the book
R and most packages can be installed directly from [CRAN](https://cran.r-project.org/). Go there for instructions on how to install R and individual packages. 
The [RStudio IDE](https://posit.co/downloads/) is a 

## MLBA
The `mlba` package is available from [](https://github.com/gedeck/mlba). You can install this package using the following commands:
```
if (!require(mlba)) {
  library(devtools)
  install_github("gedeck/mlba/mlba", force=TRUE)
}
```
Note that this requires the installation of the `devtools` package

## DiscriMiner
The `DiscriMiner` package is currently not available from CRAN. You can install it directly from Github as described in https://github.com/gastonstat/DiscriMiner
```
if (!require(DiscriMiner)) {
    library(devtools)
    install_github('DiscriMiner’, username='gastonstat')
}
```

# Setting up an environment for deep learning applications
In order to run the code for the deep learning applications, you will need to create a Python environment with the required packages. A convenient way to do this is to use [Anaconda](https://www.anaconda.com/products/individual). See [installPython.md](installPython.md) for instructions on how to install Anaconda and create the required environment.