
# Bow River Basin Council State of the Watershed Report: Water Quality Chapter
# Data Analsyis Technical Summary Report

This report provides a description of the work completed by Thompson Aquatic Consulting (TAC) for the Bow River Basin Council (BRBC), in coorporation with the Water Quality Technical Committee (WQTC), to prepare the water quality chapter of the [State of the Watershed Report](https://experience.arcgis.com/experience/d47bc1ea9c724991afae0e6d6d92fb71/).

The information provided here is intended as an overview of the data analysis project from a technical perspective.
Any researcher or analyst who wishes to delve deeper into the project should first read through this report and then refer to the [project repository](https://github.com/thompson-aquatic/brbc_sow).
It should be straightforward to update the project repository with new data files and re-run the R scripts to generate new images as time goes on, and new data becomes available.

## Data Files

Source data files for a previously-selected subset of monitoring locations were compiled, formatted and shared by the BRBC WQTC data sub-group in tabular formats (CSV, Excel).
The data in the data files include water quality and flow measurements or flow estimations for sites throughout the Bow River Basin. The details of this compilation are described in the Sate of the Watershed Report Water Quality Chapter.
Data source organizations include the [City of Calgary](https://www.calgary.ca/home.html), [Alberta Environment and Protected Areas](https://www.alberta.ca/environment-and-protected-areas), the [Water Survey of Canada](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey.html), [Environment and Climate Change Canada](https://www.canada.ca/en/environment-climate-change.html) and [Parks Canada](https://parks.canada.ca/).

All source data files have been added to the [project respository](https://github.com/thompson-aquatic/brbc_sow) on [GitHub](https://github.com/).
This repository has been shared with the BRBC WQTC and BRBC staff.
The flow data from the Water Survey of Canada ([HYDAT](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html)) is not included in the repository, but is downloaded and accessed at runtime by the R scripts.

## R Code

The R code used in this project was drafted by the BRBC WQTC data sub-group and shared with Thompson Aquatic Consulting (TAC). The R code was subsequently completed by TAC with assistance and review from the data sub-group.
The R code is contained within the [R subdirectory](https://github.com/thompson-aquatic/brbc_sow/tree/main/R) of the project repository.

Instructions for reproducing the images are contained with the [README.md](https://github.com/thompson-aquatic/brbc_sow/blob/main/README.md) file.
At a high level, the R scripts that are used to generate the images are as follows:

- **R/compile_and_consolidate_data.R**:  Reads the data files and compiles them into several consolidated CSV files.
- **R/populate_database.R**: Populates a database in order to simplify data querying.
- **R/generate_plots.R**: Plots the data and saves the data as JPEG files.
- **R/constants.R**: Lookup file for things like colours, strings, footnotes, etc. used in the plots.
- **R/functions.R**: A library of functions used in the generate_plots.R code.

### R Libraries

A number of R packages were used in this project.
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

## Plots

Draft plots were provided by the BRBC WQTC to TAC as an example of the desired plot format and content.
Several cycles of feedback between the data sub-group, WQTC, BRBC staff and TAC were completed before presenting the images to others for review.
The images were uploaded to a [Google Drive folder](https://drive.google.com/drive/u/1/folders/12jJVdAqmx9kcRDNNZ28KyuhbIv2U6nEn) for BRBC to access.

The plots are provided both as **individual** images and as **faceted** images.
In the case of individual images, each plot shows the values over time for a single parameter at a single site.
In the case of the faceted image, each plot shows the values over time for a single parameter at all sites where that parameter was measured.

Most parameters are plotted in several ways.

Where flow is present, and the parameters are concentrations, the parameters are plotted as both a **concentration** and as a **flux**. Although flow is present for most sites, there are some sites for which there is no flow data. For these sites, there are no flux plots.
In addition, some parameters are not flux parameters, and are only plotted as concentrations (pH, Sodium Adsorption Ratio, Specific Conductance and Turbidity).

For parameters where measurements are concentrations, plots are provided in both **linear** and **log10** scales for individual plots, and in log10 scale for faceted plots. Similarly, all flux plots are plotted on a log10 scale. There are no linear scale flux plots.
Note that pH is already a log scale measurement, so its scale remains unchanged on all plots.

Finally, **flow** measurements are provided as individual plots and as the top-left subplot on each faceted plot. Flow values are also plotted on both linear and log10 scales.

The naming convention for the plots is as follows:
- Individual plots
	- All individual plots are under the `output/figures/individual` folder
	- The first part of the filename is the site name
	- The second part of the filename is the parameter name
	- The third part of the filename is a combination of the scale and the units of the measurements
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
	- All faceted plots are under the `output/figures/faceted` folder
site
	- The first part of the filename is the site name
	- The second part of the filename is a combination of the scale and the units of measurement
		- All plots end with `_log_conc` or `_log_flux`
	- Examples:
		- `New West Coulee_log_conc.jpeg`
		- `New West Coulee_log_flux.jpeg`

The images themselves are not present in the project repository. In order to find the images in the `output/figures` folder, they must first be generated by building and running the project.

