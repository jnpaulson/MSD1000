Visualization of microbiome data
=============
Diarrhea is a major cause of mortality and morbidity in young children from developing countries, leading to as many as 15% of all deaths in children under 5 years of age. While many causes of this disease are already known, conventional diagnostic approaches fail to detect a pathogen in up to 60% of diarrheal cases. Gut samples were collected from ~1000 children from Bangladesh, Kenya, Mali and The Gambia. OTU features were filtered (for server speed) to those present in at least 20 samples.

The following is a visualization of microbiome data of young children from low-income countries.

To install the shiny app load up R:
```S
# install.packages("shiny")
library("shiny")
runGitHub("MSD1000", "nosson")
```
For more information please visit: http://cbcb.umd.edu/research/projects/GEMS-pathogen-discovery

Graciously hosted on [Epiviz]("http://epiviz.cbcb.umd.edu/shiny/MSD1000/") at http://epiviz.cbcb.umd.edu/shiny/MSD1000/.
