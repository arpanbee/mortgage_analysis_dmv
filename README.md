# Capital One Data Challenge
# Arpan Bhattacharya
# 05/17/2016

####################  CONTENTS ###############################

Screenshots of Shiny App in "Screenshots" dir
	- Agg_Analysis.png
	- Data_Quality.png
	- DC_Regional.png
	- DE_Regional.png
	- MD_Regional.png
	- VA_Regional.png
	- WV_Regional.png

Shiny Interactive App and Code for Methods
	- App.R

Configuration and Deployment Instructions
	- README.md

##############################################################

I have created an interactive Shiny application to help a 
business analyst explore the dataset and gain information.  
The app is self contained within App.R file.

Please ensure that the following data files are available in 
the workfing directory (/Users/arpan/Mortgage_Analysis_R)

(The working directory can be changed on line 11 of App.R)

	- '2012_to_2014_loans_data.csv 
	- '2012_to_2014_institutions_data.csv 

Please ensure the following packages are installed:
	- ggplo2
	- rjson
	- shiny
	- dplyr

##################  RUNNING THE APP ###########################

The best way to run this app, instead of running it all at once
which takes a while, is to first run lines 1 to 142.  This will 
build all the derivative datasets using the hdma_init and 
hdma_to_json functions.  These derivative datasets will be used
to create the interactive Shiny charts.

All derivative data is created within App.R and shares the same
metadata as the source information.

I have written hmda_init and hmda_to_json as instructed.  I have
made an optional argument to decide to write to disk or not.  
Writing the JSON output file takes a long time and results in 
really large file (~1GB) so I have it turned off within the app 
by default.  

################ AFTER RUNNING LINES 1-142 #####################

Once lines 1-142 have been run, you should see the following 
variables created in your environment:
	- data
	- data_cc
	- data_cc_DC
	- data_cc_DE
	- data_cc_MD
	- data_cc_VA
	- data_cc_WV
	- total_loans
	- total_loans_dc
	- total_loans_de
	- total_loans_md
	- total_loans_va
	- total_loans_wv

############## RUN LINES 143 - 448 ##############################

Run the rest of the code.  The Shiny app will automatically open 
and generate the App.  I think it looks best if you open it in a 
Browser instead of viewing it in a Shiny window.  I am attaching
some screenshots in case there are issues running the Shiny App.
I tried deploying it to Shiny.io and I was planning on hiding it 
behind a password but I ran out of time. 





