# peatProp
Updated: 2016-01-29

A simple web scrape function to get peat properties data from the [**Holocene Perspective on Peatland Biogeochemistry database**](https://peatlands.lehigh.edu/). It is a bit slow and clunky but should do the job.

The data is published in:
- [**Loisel et al. 2014 The Holocene**](http://hol.sagepub.com/content/24/9/1028)

However, of the 215 sites, only 70 sites are available online.

### Usage
The function is `get_peat(type = "all", only_meta = TRUE, metadata = NULL)`

- `type` makes it possible to subset the data on peatland type.
  All data - `"all"`, Bogs - `"bog"`, Rich fens - `"rich"`, Poor fens - `"poor"`, Permafrost - `"perma"`, Kettle holes - `"kettle"` (no available sites though), Other - `"other"`
 
- `only_meta` If TRUE only meta data is extracted, FALSE will download the data. 
 
- `metadata` If you have extracted only metadata and want to use this object to also get the data.
 
```R
#load function
source("get_peat_prop_data.R")

# Get data (slow - a simple counter shows the progress)
dat <- get_peat(type = "all", only_meta = FALSE, metadata = NULL)

# Plot bulk density versus peat depth
library(ggplot2)
library(grid)
#number cores within sites
dat$id <- with(dat, ave(as.numeric(factor(core_name)), factor(site_name), FUN=function(x) as.numeric(factor(x))))

ggplot(dat[order(dat$core_name, dat$depth),], aes(x = (bulk_density_g_cm3*1000), y = depth ,  colour = factor(id))) + 
  geom_path() + 
  geom_point() +
  scale_y_reverse(limits=c(500,0)) +
  xlim(0,300) +
  ylab("Depth (cm)") +
  xlab(expression(paste("Bulk Density (kg ",m^-3,")" ))) +
  facet_wrap(~site_name) +
  guides(colour=FALSE)
```