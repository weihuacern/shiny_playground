# Shiny Playground

## Data Source

### COVID-19

- [COVID-19, Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19)
- [World GeoMap](https://www.naturalearthdata.com/)
- [Country GeoMap](https://www.diva-gis.org/)

- To Generate world map shape data:

```bash
cd data
wget https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_sovereignty.zip
unzip ne_50m_admin_0_sovereignty.zip -d ne_50m_admin_0_sovereignty
Rscript WorldMapShapeDataGen.R
```

A file named WorldMapShape.RData will be generated.

- To generate country map shape data:

```bash
cd data
wget http://biogeo.ucdavis.edu/data/diva/adm/CHN_adm.zip
wget http://biogeo.ucdavis.edu/data/diva/adm/HKG_adm.zip
wget http://biogeo.ucdavis.edu/data/diva/adm/MAC_adm.zip
wget http://biogeo.ucdavis.edu/data/diva/adm/TWN_adm.zip
unzip CHN_adm.zip -d CHN_adm
unzip HKG_adm.zip -d HKG_adm
unzip MAC_adm.zip -d MAC_adm
unzip TWN_adm.zip -d TWN_adm
Rscript CHNMapShapeDataGen.R
wget http://biogeo.ucdavis.edu/data/diva/adm/USA_adm.zip
unzip USA_adm.zip -d USA_adm
Rscript USAMapShapeDataGen.R
```

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
