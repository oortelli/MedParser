## Overview

The elevated plus maze is a common preclinical approach to investigate approach/avoidance or anxiety-related behaviors in rodents. 

Our lab currently uses the elevated plus maze package from Med Associates, using IR sensors to detect exploration/entrance activity into the four different arms of the maze. 

## Script functions
### Parsing/extracting:
- **Session Information:** Date, SubjectID, Group Number, Box Number 
- **Time Spent:** Total time of session, time spent in closed arms (combined), time spent in open arms (combined), time spent in junction, time spent in junction + open arms, time spent in junction + closed arms
- **Latencies:** To each arm, to first open arm, to first closed arm
- **Number of explorations and entries** for open arms (combined) and closed arms (combined)
  
### Simple calculations
- Percent of time spent in open arms, relative to total session time
- Percent of time spent in closed arms, relative to total session time 
  
## Example Data

This repository includes example data in the `example_data/` folder. These files can be used to test the script and to see how our arrays are generated. We have shared our Med-PC file in the `medpc_code/` folder, as well.

Use: Run the script and a pop-up window will ask you to select the files (geneated from Med-PC session, in our case) you'd like to analyze. After selecting the file/s, an Excel file will be automatically generated with all output information per subject (i.e., file) selected. 

## Version
1.0.0

## Change Log
- 1.0.0: alpha build

## Contributing
You're welcome to use, adapt, and build on this script as needed. This script was specifically developed to save time for our lab, skipping the need to manually type in data. Your lab/project might have its own needs. If you find bugs or have suggestions, feel free to open an issue or submit a pull request. 
