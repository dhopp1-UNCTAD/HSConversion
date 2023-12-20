# HSConversion
Code to convert comtrade data from one HS year to another.
## Installation
```R
devtools::install_github("dhopp1-UNCTAD/HSConversion")
```
## Background
The purpose of this library is to enabling the conversion of Comtrade data from one HS version to another, both forwards and backwards in time. For example, not only converting HS 2012 data into HS 2017, but also converting HS 2022 data into HS 2017. This is achieved differently for each of the 4 types of relationships between HS versions:
- _1:1_- Product codes have a 1 to 1 correspondence between HS versions, no transformation required
-  _1:n_- Several codes in the source HS version correspond to a single code in the destination HS; the source values are then simply summed
- _n:1_- A single code in the source HS version corresponds to several codes in the destination HS. There is no 100% accurate way of determining the true distribution in the destination HS. In this case, a "mapping" dataframe is used, which contains trade data for the country in the destination HS (e.g., of later years). The distribution of the destination HS codes in this mapping data is then used to allocate the trade value from the single code in the source data. The idea is that countries' data in the destination HS, even if from other years, gives some indication of the true allocation. If a country has never published in the destination HS, or if the country did not trade a particular source product code in other years, equal allocation is used.
- _n:n_- Same principle as _n:1_, but several codes in the source HS version correspond to several codes in the destination HS. This is dealt with in a similar way to _n:1_ transformations. The only difference is that instead of a single product code being distributed according to the mapping data, multiple source product codes are first summed up, then distributed.

## Usage
The library consists of five functions. Run `?function_name` to obtain more information on any of them:

- `get_correspondence_tables`: this function obtains the mapping between HS versions fron [UN Stats](https://unstats.un.org/unsd/classifications/Econ). The output of this function is a dataframe used as an input to the `convert_hs` function, which can be saved to CSV to avoid pinging te UN Stats website unnecessarily.
- `convert_hs_name`: this function converts HS names (e.g., "HS2", "HS4", etc.) into their corresponding years and vice versa.
- `aggregate_country_data`: this function is used to produce a more robust mapping dataframe. A single country-year dataframe can be used for the mapping, but aggregating all of a country's data in the destination HS is a more robust method of determining allocations for _n:1_ and _n:n_ mappings. As such, the function takes as input multiple years of Comtrade data in the destination HS and aggregates the data to produce a new mapping dataframe for use in the `convert_hs` function.
- `aggregate_digit_level`: this function converts data from a more detailed HS level to a less detailed one. E.g., it can convert 6-digit HS data to the 4 or 2-digit level, or 4-digit data to the 2-digit level.
- `convert_hs`: the main function of the library, where actual conversion happens. It takes as input raw Comtrade data with the columns. Run `?convert_hs` for more details on parameters and outputs. The function may take a while to run (usually still less than a 45 seconds or so), as the calculation for _n:n_ conversion is complex and time-consuming. 

### Three most important parameters
Three important parameters you will see are the following:
- **_agg\_columns_**: a list of the columns you want the data aggregated for. Generally, these are the quantity/numeric columns. For instance, if I only care about value, I would pass `c("Value")`, but if I cared about CIF value, FOB value, and net weight, I might pass `c("CIFValue", "FOBValue", "NetWeight")`.
- **_group\_columns_**: a list of the columns you want the data grouped by. For instance, if I only care about preserving partner country and method of transport, I may pass something like `c("PartnerCode", "MoT")`.
- **_commodity\_column_**: string of the name of the commodity code column, often `'CommodityCode'`

