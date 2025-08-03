# meteoGermany – climate data interpolation pipeline

This project implements a robust, reproducible pipeline in R for spatial interpolation of daily climate variables using Kriging with external drift (elevation). It is designed for high-resolution gridding of climate parameters from station data across Germany or its subregions.

## Purpose

The pipeline enables:

- Daily spatial interpolation of climate variables using **Kriging with external drift**
- Integration of a digital elevation model (DEM) as covariate
- Quality-controlled output and masking of invalid values
- Aggregated descriptive statistics per **municipality (Gemeinde)** or **state (Bundesland)**
- Scalable, parallelized processing via `pbmcapply`

## Supported Climate Variables

- `TMK`, `TXK`, `TNK`, `TGK`: Mean, max, min, and ground temperature  
- `RSK`: Precipitation  
- `SDK`: Sunshine duration  
- `UPM`, `VPM`: Vapor pressure variables  
- `NM`: Cloudiness  
- `PM`: Mean sea-level pressure

## Core Methods

- **Kriging with external drift** using station elevation (`Stationshoehe`)
- Automatic variogram modeling with `automap`
- Raster output with optional masking and classification
- Statistical summarization using `exactextractr`

## Kriging with External Drift: Strengths and Limitations

### Advantages

- Robust to **temporal variability** in station coverage over long periods (e.g., 30+ years)
- Integrates **DEM-based elevation** as an auxiliary predictor without requiring dense spatial coverage
- Offers **physically interpretable spatial gradients**, e.g. lapse rates in temperature
- Supports **parallelized interpolation** of daily fields across large domains
- Handles **sparse or irregular networks** better than simple interpolation methods

### Limitations

- May **underperform in areas with local microclimatic effects** (e.g., urban heat islands, complex canopy or land use structure)
- Sensitive to **non-stationarity** or **heteroskedasticity** in input data
- External drift relies on **static covariates** — local weather phenomena might need dynamic or multi-scale inputs
- Assumes a **linear relationship** between covariate (elevation) and target variable — not always valid (e.g., inversions)

### Relevance to This Pipeline

The current pipeline implements a **generalized, robust KED setup** that works well across most of Germany. While not optimized for all microclimates, its simplicity and reproducibility make it highly effective for:

- **National-scale climatology**
- **Retrospective gridding** across decades
- **Administrative-scale aggregation** (e.g., Gemeinde, Bundesland)
- Fast, daily **reanalysis-like generation** with minimal tuning

## References

- Dolinar, M. (2006).  
  *Spatial interpolation of sunshine duration in Slovenia.* Meteorological Applications, 13: 375–384.  
  [https://doi.org/10.1017/S1350482706002362](https://doi.org/10.1017/S1350482706002362)

- Hofstra, N., Haylock, M., New, M., & Jones, P.D. (2009).  
  *Testing E-OBS European high-resolution gridded data set of daily precipitation and surface temperature.*  
  J. Geophys. Res., 114, D21101.  
  [https://doi.org/10.1029/2009JD011799](https://doi.org/10.1029/2009JD011799)

- Hengl, T., Heuvelink, G.B.M., & Stein, A. (2003).  
  *Comparison of kriging with external drift and regression-kriging.*  
  ITC Technical Note. Available: [https://ris.utwente.nl/ws/portalfiles/portal/448469781/hengl_comparison.pdf](https://ris.utwente.nl/ws/portalfiles/portal/448469781/hengl_comparison.pdf)

- Pebesma, E.J. (2006).  
  *The gstat package.* Computers & Geosciences, 30(7), 683–691.  
  [https://doi.org/10.1016/j.cageo.2004.03.012](https://doi.org/10.1016/j.cageo.2004.03.012)

## Credits

This work integrates and extends ideas from:

- Documentation of the `rdwd` package  
  [https://bookdown.org/brry/rdwd/](https://bookdown.org/brry/rdwd/)

- Hartmann, K., Krois, J., Rudolph, A. (2023): Statistics and Geodata Analysis using R (SOGA-R). Department of Earth Sciences, Freie Universität Berlin  
  [https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Time-series-analysis/index.html](https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Time-series-analysis/index.html)

---

**Author**: Chris Reudenbach  
**License**: [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](http://creativecommons.org/licenses/by-sa/4.0/)  
**Contact**: creuden@gmail.com
