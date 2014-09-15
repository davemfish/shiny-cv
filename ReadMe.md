
### Setup Instructions
To run the Coastal Vulnerability Output Viewer, first you must install the R software environment on your computer, followed by a few add-on packages for R. 



#### Install R:

http://www.cran.r-project.org/mirrors.html

Use this link to choose a location from which to download, then select the installer for your particular operating system. 


#### Install R Packages: 


After R is installed, open it, copy & paste the following lines into the R console, and press enter.

```r
install.packages(c("shiny", "reshape2", "ggplot2", "devtools", "rgdal", "hwriter", "RColorBrewer", "raster"))
library(devtools)
install_github('rCharts', 'ramnathv')
```




### Run the Coastal Vulnerability app:


Paste this line of code into your R console.

```r
shiny::runGitHub(repo='shiny-cv', username='davemfish', subdir='app-cv')
```
