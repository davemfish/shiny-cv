
### Setup Instructions
To run the Coastal Vulnerability Output Viewer, first you must install the R software environment on your computer, followed by a few add-on packages for R. 



#### Install R:

R installs much like any other software you have installed.

http://www.cran.r-project.org/mirrors.html




#### Install R Packages: 


After R is installed, open the application, copy & paste the following line into the R console, and press enter.

```r
install.packages(c("shiny", "rCharts", "leaflet", "reshape2", "ggplot2")
```




### Run the Coastal Vulnerability app:


Paste this line of code into your R console.

```r
shiny::runGitHub(repo='shiny-cv', username='davemfish', subdir='app-cv')
```
