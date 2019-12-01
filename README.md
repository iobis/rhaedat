rhaedat
-------

### Example code

A number of code examples can be found in the [demo](/demo) folder.

### Fetching data

This package includes the `events()` function which fetches all event
data from HAEDAT:

``` r
df <- events()
head(df)
```

    ##   gridCode regionName longitude  latitude countryCode countryName
    ## 1    AL-01       <NA>  19.73333  41.36667          91     ALBANIA
    ## 2    AM-01       <NA>  44.46667  40.15000         119     ARMENIA
    ## 3    AR-00      FANSA -58.27440 -34.41500           2   ARGENTINA
    ## 4    AR-00      FANSA -58.27440 -34.41500           2   ARGENTINA
    ## 5    AR-00      FANSA -58.27440 -34.41500           2   ARGENTINA
    ## 6    AR-01      FANSA -56.62000 -35.76000           2   ARGENTINA
    ##   eventYear eventName                  syndromeName massMortal foamMucil
    ## 1      2013 AL-13-001 Cyanobacterial toxins effects      FALSE     FALSE
    ## 2      2012 AM-12-001 Cyanobacterial toxins effects      FALSE     FALSE
    ## 3      2000 AR-00-003 Cyanobacterial toxins effects      FALSE     FALSE
    ## 4      2003 AR-03-002 Cyanobacterial toxins effects      FALSE     FALSE
    ## 5      2004 AR-04-002 Cyanobacterial toxins effects      FALSE     FALSE
    ## 6      2000 AR-00-005    Aerosolized toxins effects      FALSE     FALSE
    ##   aquacultureFishAffected causativeSpecies  eventDate initialDate
    ## 1                   FALSE Microcystis spp.       <NA>        <NA>
    ## 2                   FALSE Microcystis spp.       <NA>        <NA>
    ## 3                   FALSE Microcystis spp.       <NA>        <NA>
    ## 4                   FALSE Microcystis spp.       <NA>        <NA>
    ## 5                   FALSE             <NA>       <NA>        <NA>
    ## 6                   FALSE             <NA> 2000-01-02        <NA>
    ##   finalDate days months      period
    ## 1      <NA>    0      0 2013 - 2017
    ## 2      <NA>    0      0 2008 - 2012
    ## 3      <NA>    0      0 1998 - 2002
    ## 4      <NA>    0      0 2003 - 2007
    ## 5      <NA>    0      0 2003 - 2007
    ## 6      <NA>    0      0 1998 - 2002

Note that there may be multiple rows per event, for example when an
event spans multiple grids or when multiple causative species are
involved. This needs to be taken into account when analyzing event data.
