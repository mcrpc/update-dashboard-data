# MCRPC Dashboard Data Update Tool

## Basic Setup

- Download & install the latest version of [R](https://cran.r-project.org/) and [RStudio](https://rstudio.com/products/rstudio/download/). If on an in-office workstation, these can be installed without administrator privileges by installing for your user only.
- **If running the script from the shared drive project folder:** Simply open 'update-dashboard-data' R project in RStudio, then open 'run-script.R' within the 'files' menu of RStudio. Uncomment & Run line #24 to make sure the proper packages are installed if this is the first time using the tool. You may also need to uncomment & Run line #16 to authorize the script to read & write to Google Sheets. When ready, Source the script file in RStudio to run it in its entirety, or Run individual block of code to understand how they work. Running the entire script may take several minutes to execute, depending on your processor, internet connection, and available RAM.
- **If running the script by cloning the repository outside of the shared drive project folder:** You will need to add a Census API key to a plain text file and name it api_key.txt. This file needs to be in the same directory as the R project. MCRPC has a Census API key, it is in the same location as our shared login credentials. Now you can follow the directions on the bullet point above.
