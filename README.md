# NSSP_SS_File_Monitoring
For public health jurisdictions utilizing the NSSP BioSense Platform for syndromic surveillance.

This code allows BioSense users with access to RStudio and Adminer data tables to visualize incoming syndromic files from participating facilties on a daily basis. 

The data cleaning portion extracts the data from BioSense, fills in data gaps, and merges with the historical data. The historical data set archives prior months into a single data set so the user doesn't need to extract hsitorical data every time. This is updated on the 15th of every month. The Shiny script launches an app with three visualizations: (1) a dot plot showing the date and time of file arrivals (2) overlapping bar chart with cumulative number of ED registrations per visit date up to the file arrival in gray and the number of ED registrations per visit date in a given file in green (3) another overlapping bar chart for messages per event date with blue bars for the current file. Next to the overlapping bar charts are tables listing the dates with counts lower than the threshold (1 SD below the historical mean based on 1 year of data).

User interactivity is a key aspect of the app:
* Select which hospital group they are viewing
* Select the dates the files arrived
* Select the event/visit dates for the x-axis of the overlapping bar charts
* Add or remove the threshold line 
* Download individual plots as .png files
  + The overlapping bar charts include a list of dates below the threshold underneath the image when downloaded
* Download the full report as a Word, PDF, or HTML file

See a static example of the app here:

![Example of app output](https://github.com/sarachronister/NSSP_SS_File_Monitoring/blob/master/Daily%20File%20Monitoring%20App.png)
      
