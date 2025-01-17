
## Data Files

- sources (CoC, AEPA, etc)
- types (CSV, Excel, etc)
- 
## R Code

R code was started by BRBC WQTC data sub-group and shared with Thompson Aquatic Consulting (TAC).
The completed by TAC with assistance and review from the data sub-group.
The R code was uploaded to [github.com](https://github.com/thompson-aquatic/brbc_sow) and shared with the data sub-group.

### R Scripts

1. `R/compile_and_consolidate_data.R`
	-  Reads the data files and saves
2. `R/populate_database.R`
	- Populates a database in order to simplify data querying.
3. `R/generate_plots.R`
	- Plots the data and saves the data as PNG files.
### Other R Files

1. `R/constants.R`
	- Lookup file for things like colours, strings, footnotes, etc.
2. `R/functions.R`
	- Implementation for data querying and plot creation is contained within this file.

### R Libraries

A number of R packages are used in this project.
These packages in turn have their own dependencies.
This list includes only those R packages that were explicitly installed, and not their dependencies:
- doFuture
- doparallel
- doParallel
- dplyr
- duckdb
- foreach
- furrr
- here
- hydat
- janitor
- languageserver
- magrittr
- openxlsx
- pacman
- plotly
- plumber
- tidyhydat
- zoo

## Plots

Preliminary images of the plots were provided by the BRBC WQTC to TAC as an example of the desired plot format and content.
Several cycles of feedback between the data sub-group and TAC were completed before presenting the images to the rest of BRBC WQTC for review.
Several cycles of feedback between the larger BRBC WQTC, BRBC staff and TAC were completed before the final version of the images was delivered.
The images were uploaded to a [Google Drive folder](https://drive.google.com/drive/u/1/folders/12jJVdAqmx9kcRDNNZ28KyuhbIv2U6nEn) for BRBC to access.

(describe plots)
- faceted
- individual
- flow
- concentration
- concentration (log10)
- flux (log10)

## Analysis

(start with copy/paste from the report and adapt)
 
- matching flow to WQ
- colour coding according to percentiles
	- ecdf
	- comparison to latest year
- log scale y-axis
	- not for pH
 - non-flux parameters
