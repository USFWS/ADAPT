We created ADAPT to help partners make science-informed decisions related to species and habitat prioritization across the Southwest United States. The Shiny app that resulted from the work hosts the output of species distribution models (SDMs) based on climate change (relative humidity), land use change, elevation, and year. We have SDMs for the current time period as well as projections for 2040-2060 and 2080-2100 for SSP2 and SSP5 climate change scenarios. The app also has a “Data Summary” tab includes a summary of the survey data used to generate the SDMs.

For our models, we used Integrated Nested Laplace Approximation (INLA) to analyze the data at a 1-km resolution. We used climate (data source link) and land use change data (data source link) generated from CMIP 6 climate models. Count data were from eBird surveys and converted to presence/absence data for analysis. Our model consisted of fixed effects of elevation, land cover type, and effort with random effects for year and site as well as a spatial random effect. We implemented the effect of relative humidity as a spatially-varying coefficient such that the effect of relative humidity on occupancy can vary at each grid cell. We evaluated models for fit and predictive ability using the Conditional Predictive Ordinate, a form of Leave-One-Out Cross Validation.

Please feel free to incorporate rasters or high-resolution graphics of the SDMs (found in the "images" folder) into technical documents with proper credit. The Shiny app can be found at https://das.ecosphere.fws.gov/content/03f753d8-6793-4394-8e7b-25be5704334d/