
# Summary Report

This report provides a summary of the work completed by Thompson Aquatic Consulting (TAC) for the Bow River Basin Council (BRBC) Water Quality Technical Committee (WQTC).

The information provided here is intended as a high-level overview of the project from a technical perspective.
Any researcher or analyst who wishes to delve deeper into the project should first read through this report and then refer to the [project repository](https://github.com/thompson-aquatic/brbc_sow).
It should be fairly straightforward to update the project repository with new data files and re-run the R scripts to generate new images as time goes on.

## Data Files

Source data files were collected by the BRBC WQTC data sub-group.
The data provided in the data files includes water quality and flow measurements from various sites throughout the Bow River Basin.
Data sources include the [City of Calgary](https://www.calgary.ca/home.html), [Alberta Environment and Protected Areas](https://www.alberta.ca/environment-and-protected-areas), the [Water Survey of Canada](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey.html), [Environment and Climate Change Canada](https://www.canada.ca/en/environment-climate-change.html) and [Parks Canada](https://parks.canada.ca/).
Data files were provided in tabular formats (CSV, Excel).

All source data files have been added to the [project respository](https://github.com/thompson-aquatic/brbc_sow) on [GitHub](https://github.com/).
This repository has been shared with the BRBC WQTC and BRBC staff.
The flow data from the Water Survey of Canada ([HYDAT](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)) is not included in the repository, but is downloaded and accessed at runtime by the R scripts.

## R Code

R code was started by BRBC WQTC data sub-group and shared with Thompson Aquatic Consulting (TAC).
The completed by TAC with assistance and review from the data sub-group.
The R code is contained within the [R subdirectory](https://github.com/thompson-aquatic/brbc_sow/tree/main/R) of the project repository.

Instructions for reproducing the images are contained with the [README.md](https://github.com/thompson-aquatic/brbc_sow/blob/main/README.md) file.
At a high level, the R scripts that are used to generate the images are as follows:

- **R/compile_and_consolidate_data.R**:  Reads the data files and compiles them into several consolidated CSV files.
- **R/populate_database.R**: Populates a database in order to simplify data querying.
- **R/generate_plots.R**: Plots the data and saves the data as PNG files.
- **R/constants.R**: Lookup file for things like colours, strings, footnotes, etc.
- **R/functions.R**: Implementation for data querying and plot creation is contained within this file.

### R Libraries

A number of R packages are used in this project.
This list includes only those R packages that were explicitly installed, and not the dependencies of those packages:
- [doFuture](https://cran.r-project.org/web/packages/doFuture/index.html)
- [doParallel](https://cran.r-project.org/web/packages/doParallel/index.html)
- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
- [duckdb](https://cran.r-project.org/web/packages/duckdb/index.html)
- [foreach](https://cran.r-project.org/web/packages/foreach/index.html)
- [furrr](https://cran.r-project.org/web/packages/furrr/index.html)
- [here](https://cran.r-project.org/web/packages/here/index.html)
- [janitor](https://cran.r-project.org/web/packages/janitor/index.html)
- [languageserver](https://cran.r-project.org/web/packages/languageserver/index.html)
- [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
- [openxlsx](https://cran.r-project.org/web/packages/openxlsx/index.html)
- [pacman](https://cran.r-project.org/web/packages/pacman/index.html)
- [plotly](https://cran.r-project.org/web/packages/plotly/index.html)
- [plumber](https://cran.r-project.org/web/packages/plumber/index.html)
- [tidyhydat](https://cran.r-project.org/web/packages/tidyhydat/index.html)
- [zoo](https://cran.r-project.org/web/packages/zoo/index.html)

## Data Compilation

> Water quality is monitored along the entire length of the Bow River and many of its tributaries. This monitoring is carried out by federal, provincial and municipal government agencies, as well as by watershed groups and university researchers. From each of these sources, available datasets were identified, and a subset of representative monitoring locations were selected for the assessment of water quality.

> For the purposes of this report, the Bow River basin has been split into five reaches: Upper Bow, Upper Foothills, Lower Foothills, Middle Bow, and Lower Bow. These reaches were established with the locations of sub-watersheds, monitoring stations, natural regions and jurisdictions in mind. The reaches were primarily mapped using the hydrologic unit code 8 (HUC 8) sub-watershed boundaries, with two exceptions. The division between the Upper Foothills and Lower Foothills reaches divides the Bow River and Bighill Creek HUC 8 using the Town of Cochrane boundary, so that all monitoring stations within the Town of Cochrane would be included in the Upper Foothills reach. The division between the Lower Foothills and the Middle Bow reaches divides the Middle Bow River HUC 8 using the boundaries of the Middle Bow River above Dalemead HUC 10 and the Dalemead Lake - Middle Bow River HUC 10, so that the mainstem sites within Calgary and upstream of the Highwood River would fall within the Lower Foothills reach.

> Water quality data from Parks Canada, Alberta Environment and Protected Areas (AEPA), and the City of Calgary were selected for data compilation at sites on the Bow River mainstem (Table 2). In the tributaries, water quality datasets from Alberta Environment and Protected Areas and the City of Calgary were selected for the compilation. Most of the water quality data is collected once a month, year-round, although some of these have various intermittent gaps in coverage, presumably due to access or safety issues, equipment malfunctions, and other issues. 

> In order to calculate the flux of substances in the water at the various monitoring locations, water flow data were obtained from the Water Survey of Canada (WSC), AEPA and the City of Calgary (Table 2). For certain stations, flow data were only available for the open water season (seasonal, spanning from March, April or May to October, depending on the agency and location), so that the annual summaries of constituent flux are for the open water season only. For the seasonal flow site on the Bow River at Cochrane, flows were estimated by AEPA through hydrological assessments using nearby hydrometric stations. For the Bow River above the Highwood River, flows were estimated by subtracting Highwood River flows from the Bow River flows below Carseland Dam.  For the seasonal flow site on the Bow River below Carseland Dam, AEPA estimates winter flows based on a flow monitoring station elsewhere on the Bow River, summed with flow in the Elbow and Highwood Rivers, and estimable tributary flows. 

> Most sites where flow data were available had flow measurements for every date for which there were water quality concentrations measured. In the cases where sites were missing flow measurements, this was due to the flow measurement schedule not aligning with the water quality measurement schedule. For example, Bow River above Lake Louise measures flow seasonally from May to October, while water quality is measured in every month of the year.
 
> No flow data or flow estimates were available for Coal Creek, Bow River at Cushing Bridge, or East Arrowwood Creek. Flux estimates were not available for Big Hill Creek because flow records end before water quality records begin at that location.

## Plots

Preliminary images of the plots were provided by the BRBC WQTC to TAC as an example of the desired plot format and content.
Several cycles of feedback between the data sub-group and TAC were completed before presenting the images to the rest of BRBC WQTC for review.
Several cycles of feedback between the BRBC WQTC, BRBC staff and TAC were completed before the final version of the images was delivered.
The images were uploaded to a [Google Drive folder](https://drive.google.com/drive/u/1/folders/12jJVdAqmx9kcRDNNZ28KyuhbIv2U6nEn) for BRBC to access.

The plots are provided both as **individual** images and as **faceted** images.
In the case of individual images, each plot shows the values over time for a single parameter at a single site.
In the case of the faceted image, each plot shows the values over time for a single parameter at all sites where that parameter was measured.

Most parameters are plotted in several ways.
Where flow is present, and the parameters are concentrations, the parameters are plotted as both a **concentration** and as a **flux**.
Although flow is present for most sites, there are some sites for which there is no flow data.
For these sites, there are no flux plots.
In addition, some parameters are not flux parameters, and are only plotted as concentrations (pH, Sodium Adsorption Ratio, Specific Conductance and Turbidity).

For parameters where measurements are concentrations, plots are provided in both **linear** and **log10** scales for individual plots, and as in log10 scale for faceted plots.
Similarly, all flux plots are plotted on a log10 scale.
There are no linear scale flux plots.
Note that pH is already a log scale measurement, so its scale remains unchanged on all plots.

Finally, **flow** measurements are provided as individual plots and as the top-left subplot on each faceted plot.
Flow values are also plotted on both linear and log10 scales.

The naming convention for the plots is as follows:
- Individual plots
	- All individual plots are under the `output/figures/individual` folder
	- The first part of the filename is the site name
	- The second part of the filename is the parameter name
	- The third part of is a combination of the scale and the units of the measurements
		- log10 scale plots end with `_log_conc` or `_log_flux`
		- linear scale plots end with `_conc` or `_flux`
	- Flow plots end with just the parameter name (`_FLOW`)
	- Examples:
	 - `New West Coulee_SULPHATE DISSOLVED_log_conc.jpeg`
	 - `New West Coulee_SULPHATE DISSOLVED_conc.jpeg`
	 - `New West Coulee_SULPHATE DISSOLVED_scatter.jpeg`
	 - `New West Coulee_SULPHATE DISSOLVED_log_flux.jpeg`
	 - `New West Coulee_FLOW.jpeg`
- Faceted plots
	- All facet plots are under the `output/figures/faceted` folder
site
	- The first part of the filename is the site name
	- The second part of is a combination of the scale and the units of the measurements
		- All plots end with `_log_conc` or `_log_flux`
	- Examples:
		- `New West Coulee_log_conc.jpeg`
		- `New West Coulee_log_flux.jpeg`

The images themselves are not present in the project repository.
In order to find the images in the `output/figures` folder, they must first be generated by building and running the project.

## Analysis

> Data for each of the key parameters from each of the three water quality data sources and the flow datasets were standardized for the compilation (same unit, speciation, etc.). In some cases, a given parameter was measured using several different analytical methods, within and between each data sources’ monitoring program. Comparisons between monitoring sites are made in this report, but the reader should note that different sampling and analytical methods may have been used. For water quality data points that were below the analytical detection limit, the most commonly occurring detection limit value in each agency's entire dataset was substituted in order to calculate flux for each parameter. This means that, for water quality parameter data points that were below the detection limit, the calculated flux is likely an overestimate of the actual flux in most cases.

> The flux of constituents in the rivers and creeks was calculated by multiplying the volume of water flowing past a monitoring location by the measured concentration of that parameter at that time. This provides the mass, or flux, of each parameter carried by the river or creek at that time. This type of estimate is considered instantaneous, and is not the same as a substance load, which is usually calculated as the mass of a substance in the river integrated over some longer time period (for example, a day, month or year). Where flow data for a given site are available seasonally and not year-round, the calculated flux is also seasonal, even for sites with year-round water quality data.

> For each site, the water quality concentrations and flux data were separated into historical data and the most recent year of compiled data (2022). The historical data were divided into percentile categories, or buckets, as follows: <10th percentile, between 10th and 25th percentile, between 25th and 75th percentile, between 75th and 90th percentile, and >90th percentile. The data for each year were then plotted as boxplots. 

> The colour of each boxplot reflects the position of each annual median (dark middle line in each box) in terms of the percentile buckets. A year with a median that falls in the >90th percentile bucket indicates an unusually high value, while a year with a median in the 25th-75th percentile bucket indicates a historically normal value, and a year with a median that falls in the <10th percentile bucket indicates an unusually low value. The compiled boxplot graphs use a log scale on the vertical axis to make visualization easier and more interpretable, and readers should keep this display method in mind.

> A boxplot is a type of plot that illustrates the distribution of a group of data around a median, or middle value, in this case for each year of monitoring data (see example Figure 1). Above and below the median, a box extends upwards to the 75th percentile of the data and downward to the 25th percentile of that year’s data. The range of data values within this box is called the interquartile range. The lines extending above and below the box, often called whiskers, extend the largest or smallest value that falls within 1.5 times the interquartile range. Data that are plotted individually beyond these whiskers are called outliers.

> The final graphs, showing colour-coded box plots over time, were visually assessed for notable patterns over time. These trends included upwards or downwards over time, as well as ‘variable’, which usually included both increases and decreases during the period of data coverage. Water quality data with applicable provincial environmental quality guidelines (Government of Alberta 2018) were also evaluated for guideline exceedances. The water quality guidelines applied in this report are listed in Table 3.

> The results of the water quality assessment up to 2022 are discussed and organized in this report by river reach. The findings of the previous comprehensive State of the Watershed Report for the Bow Basin are also summarized for each river reach, for reference. The 2005 report discussed data for many more water quality parameters than are included as key indicator parameters in this report.

> It is important to note that monitoring data are used in this report to explore and discuss potential change in water quality over time, and to identify potential causes of change. However, the analysis does not investigate or identify any specific causes of change in the key indicators identified above.

