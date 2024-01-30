## Replication files for the paper: [García, G.](https://gusgarciacruz.github.io/cv), [Badillo, E](https://ebadilloe.github.io/), Aristizábal, J. (2024). "Housing informality and labor informality in space: in search of the missing links". *Applied Spatial Analysis and Policy*, forthcoming

In this repository you will find the codes and dataset to replicate the main findings of the paper.

There are two subfolders:
- [Codes](https://gusgarciacruz.github.io/InformalHousingLabor/Codes) 
- [Files](https://gusgarciacruz.github.io/InformalHousingLabor/Files)

### Codes
The "Codes" folder contains information about the codes in the R language:

- Descriptive Statistics.R: code for the descriptive statistics
- Estimates.R: code for the econometrics models
- LISA - Bivariate_2017.R: code for the LISA bivariate (Fig 5) 

Both the Descriptive Statistics.R and Estimates.R 
use the csv file called "García-Badillo-Aristizábal.csv"

LISA - Bivariate_2017.R use the file Medellin_Map.shp 

### Files
The "Files" folder contains the subfolders: Data and Shapes.

Data contains the csv file and Shapes the file with the .shp 
extension that allow you to built the W matrix and replicate Figure 5 
(the other figures were calculated using QGIS and we do not have code).

The Medellin_Map.shp file refers to a cartography of the analytical regions of the urban area of Medellín.

### Slides
We have a presentation of the paper made at the 20th IEA World Congress in 2023. [Slides](https://gusgarciacruz.github.io/Presentations/IEA2023/SlidesIEA2023.html)  