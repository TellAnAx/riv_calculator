
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A Shiny app to calculate RIV points

This RShiny app calculates RIV points according to [Dean’s Measure
No. 18/2024](https://www.frov.jcu.cz/images/FROV/fakulta/uredni-deska/opatreni-dekana/2024/Measure_of_Dean_18-2024.pdf).
The app uses the *Clarivate Journal Citation Report* data to calculate
the RIV points for scientific outputs such as journal articles. The app
is designed to be used by researchers and research institutions at the
Faculty of Fisheries and Protection of Waters, University of South
Bohemia in České Budějovice, Czech Republic.

## Repository structure

- The **raw data** can be found in the `data/` directory.
- The `www/` directory holds all logo files in `.png` format.
- `calc.xlsx` is just a test calculation and not part of the app.

## Usage

- to add new *Clarivate Journal Citation Report* data, add the `.xlsx`
  file to the `data/` directory. Also, the dropdown menu
  (`input$dataset`) must be updated by adding the dataset name as
  additional `selectInput` option in the `ui.R` file.
- version updates shall be described in `NEWS.md` and the updated
  version number added to `DESCRIPTION` from where it is queried in the
  `app.R` file and displayed in the UI automatically.

``` r
sessionInfo()
#> R version 4.6.0 (2026-04-24 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26200)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: Europe/Prague
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.6.0    fastmap_1.2.0     cli_3.6.6         tools_4.6.0      
#>  [5] htmltools_0.5.9   otel_0.2.0        rstudioapi_0.18.0 yaml_2.3.12      
#>  [9] rmarkdown_2.31    knitr_1.51        xfun_0.57         digest_0.6.39    
#> [13] rlang_1.2.0       evaluate_1.0.5
```
