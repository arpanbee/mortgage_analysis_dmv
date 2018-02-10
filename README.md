# Mortgage Analysis DMV
### Arpan Bhattacharya
### 05/17/2016

## CONTENTS ## 

Shiny Interactive App and Code for Methods
	- App.R

Configuration and Deployment Instructions
	- README.md

The app is self contained within App.R file.  Data needs to 
to be downloaded and added to the working directory.  Data
can be downloaded here: 
http://data.arpan.info/2012_to_2014_loans_data.csv
http://data.arpan.info/2012_to_2014_loans_data.csv
Please ensure that the following data files are available in 
the working directory:

	- '2012_to_2014_loans_data.csv'
	- '2012_to_2014_institutions_data.csv' 

Please ensure the following packages are installed:
	- ggplo2
	- rjson
	- shiny
	- dplyr

## RUNNING THE APP ##

The best way to run this app, instead of running it all at once
which takes a while, is to first run lines 1 to 142.  This will 
build all the derivative datasets using the hdma_init and 
hdma_to_json functions.  These derivative datasets will be used
to create the interactive Shiny charts.






