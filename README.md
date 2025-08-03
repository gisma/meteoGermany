# meteoGermany

# Climate Interpolation Pipeline

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

## References

- Hengl, T., Heuvelink, G.B.M., & Stein, A. (2003).  
  *Comparison of kriging with external drift and regression-kriging.*  
  ITC Technical Note. Available: [http://www.itc.nl/library/Academic_output/](http://www.itc.nl/library/Academic_output/)

- Pebesma, E.J. (2006).  
  *The gstat package.* Computers & Geosciences, 30(7), 683–691.  
  [https://doi.org/10.1016/j.cageo.2004.03.012](https://doi.org/10.1016/j.cageo.2004.03.012)

- Hofstra, N., Haylock, M., New, M., & Jones, P.D. (2009).  
  *Testing E-OBS European high-resolution gridded data set of daily precipitation and surface temperature.*  
  J. Geophys. Res., 114, D21101.  
  [https://doi.org/10.1029/2009JD011799](https://doi.org/10.1029/2009JD011799)

- Dolinar, M. (2006).  
  *Spatial interpolation of sunshine duration in Slovenia.* Meteorological Applications, 13: 375–384.  
  [https://doi.org/10.1017/S1350482706002362](https://doi.org/10.1017/S1350482706002362)

## Credits

This work integrates and extends ideas from:

- Hartmann, K., Krois, J., Rudolph, A. (2023): Statistics and Geodata Analysis using R (SOGA-R). Department of Earth Sciences, Freie Universitaet Berlin.  
  [https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Time-series-analysis/index.html](https://www.geo.fu-berlin.de/en/v/soga-r/Advances-statistics/Time-series-analysis/index.html)

- Documentation of the `rdwd` package  
  [https://bookdown.org/brry/rdwd/](https://bookdown.org/brry/rdwd/)

---

**Author**: Chris Reudenbach  
**License**: MIT  
**Contact**: creuden@gmail.com
