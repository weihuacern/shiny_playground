# Shiny Playground

## Data Source

[COVID-19, Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19)

[World, GeoMap](https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/)

```bash
cd data
wget https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_sovereignty.zip
unzip ne_50m_admin_0_sovereignty.zip -d ne_50m_admin_0_sovereignty
Rscript WorldMapShapeDataGen.R
```

A file named WorldMapShape.RData will be generated.

## Instruction

### Dependencies

```bash
install.packages("RCurl")
install.packages("leaflet")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyverse")
install.packages("viridis")
```

### Running

```bash
R -e "options(shiny.port = 1234);shiny::runApp('./R/app.R')"
R -e "shiny::runGitHub('shiny_playground', 'weihuacern', subdir = 'R/')"
```

## TODO

- Optimize World Map Code
- Add Time Series Code
- Dockerize
- Publish
- Blog
