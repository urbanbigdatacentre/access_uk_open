
# Generate R package citations

# Packages
packages <- 
  c(
    'base', 'r5r', 'osmdata', 'UK2GTFS'
  )

# Write bibliography
dir.create('bib')
knitr::write_bib(packages, "bib/packages.bib")

