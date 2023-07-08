# chelle_data_vis
Visualising beads experiments

This can currently be run remotely using shinyapps.io (private link) or downloaded and run manually. 

Installation:

Will not currently work due to missing Google Drive secret keys!

`git clone git@github.com:TJN25/chelle_data_vis.git`

You will need to install R, the R packages listed in global.R, server.R, ui.R, and plot.R (and possibly others).

This can be run from within RStudio using the Run App button.

It can be run from terminal with 

`R -e "shiny::runApp('/PATH/TO/chelle_data_vis',port = 3838)"`

Open a web browser and go to http://127.0.0.1:3838
