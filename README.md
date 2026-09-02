
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

### **1. Clone the repository**
```bash
git clone https://github.com/dchen0515/translink_detour_analysis.git
```

### **2. Open the project in RStudio**
Double‑click:

```
translink_detour_analysis.Rproj
```

### **3. Install required R packages**
Your script uses the following libraries:

```
install.packages(c(
  "RProtoBuf", "ggplot2", "stringr", "dplyr", "purrr", "tidyr",
  "sf", "ggspatial", "ggrepel", "magrittr", "cancensus", "rmapshaper"
))
```

### **4. Set up cancensus API access**
This is required for downloading Metro Vancouver boundaries.

```
cancensus::set_cancensus_api_key("YOUR_API_KEY")
```

You can obtain a key from:  
(https://censusmapper.ca/api)

### **5. Ensure GTFS data is present in the `data/` folder**

Required files:

```
data/routes.txt
data/trips.txt
data/stops.txt
data/shapes.txt
data/stop_times.txt
data/gtfs-realtime.proto
data/translink_gtfsalerts.pb
```

These must match the GTFS version noted in the README.

### **6. Run the analysis script**
```
source("src/translink_detour_data.R")
```

This script will:

- load GTFS static + realtime data  
- classify routes  
- build spatial layers  
- generate detour hotspot maps  
- generate detour frequency plots  

### **7. View outputs**
Maps are saved to:
```
results/maps/
```

Plots are saved to:
```
results/plots/
```

### **Next Steps**
1. **Map detour hotspots for additional major routes**  
   Expand the hotspot analysis beyond the initial set of routes to build a region-wide detour profile.

2. **Compare detour frequency across corridors**  
   Identify which transit corridors experience the highest levels of disruption and explore potential causes.

3. **Analyze detours over time**  
   Examine daily, monthly, and seasonal trends to understand temporal patterns in detour activity.

4. **Assess neighbourhood-level impacts**  
   Determine whether certain neighbourhoods experience disproportionately high detour rates and explore contributing factors.
