
### **Data Attribution**
Route, schedule, and arrival data used in this project is provided by permission of **TransLink**.  
TransLink assumes no responsibility for the accuracy or currency of the data.

### **Static GTFS Data Version**
- **Last updated:** April 27, 2026  
- If running this analysis on current service, update the GTFS feed weekly to ensure accuracy.

### **Project Overview**
This project analyzes bus detours across Metro Vancouver using TransLink’s GTFS static data and detour alerts. The workflow includes:

- Cleaning and organizing GTFS datasets  
- Mapping detour hotspots  
- Visualizing detour frequency by route  
- Exploring spatial patterns in disruptions  

The goal is to identify where and when detours occur most frequently, and how they vary across routes, corridors, and neighbourhoods.

## **How to Run the Analysis**

1. **Clone the repository**
   ```
   git clone https://github.com/dchen0515/translink_detour_analysis.git
   ```

2. **Open the project in RStudio**
   - Double‑click `translink_detour_analysis.Rproj`.

3. **Install required R packages**
   ```
   install.packages(c("tidyverse", "sf", "ggplot2", "lubridate"))
   ```
   
4. **Ensure GTFS data is present**
   - All GTFS text files must be in the `data/` folder, including:
     - `stop_times.txt`  
     - `trips.txt`  
     - `stops.txt`  
     - `routes.txt`  
     - and all other GTFS components used by the script.

5. **Run the main analysis script**
   ```
   source("src/translink_detour_data.R")
   ```

6. **View outputs**
   - Maps are saved in `results/maps/`
   - Plots are saved in `results/plots/`

### **Next Steps**
1. **Map detour hotspots for additional major routes**  
   Expand the hotspot analysis beyond the initial set of routes to build a region-wide detour profile.

2. **Compare detour frequency across corridors**  
   Identify which transit corridors experience the highest levels of disruption and explore potential causes.

3. **Analyze detours over time**  
   Examine daily, monthly, and seasonal trends to understand temporal patterns in detour activity.

4. **Assess neighbourhood-level impacts**  
   Determine whether certain neighbourhoods experience disproportionately high detour rates and explore contributing factors.
