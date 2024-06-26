# My goal here is to come up with some questions for Rachel about getting
# ceds stuff into Hector, I think the idea would be to compare old ceds vs
# new release.


# 0 . Set Up -------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(hector)

# 1. Load Data ----------------------------------------------------------------
list.files(here::here("CEDS_v2021-04-21_emissions"),
           pattern = "global_CEDS_emissions_by_sector",
           full.names = TRUE) %>%
    lapply(function(f){

        d <- read.csv(f)

        vals_df <-  select( d, -em, - sector, -units)

        em <-  unique(d[["em"]])
        units <- unique(d[["units"]])
        total_values <- apply(X = vals_df, MARGIN = 2, FUN = sum)

        Xyrs <- names(total_values)
        yrs <- as.integer(gsub(pattern = "X", replacement = "", x = Xyrs))

        out <- data.frame(value = total_values,
                          year = yrs,
                          em = em,
                          units = units,
                          row.names = NULL)
        return(out)

    }) %>%
    do.call(what = "rbind") ->
    global_values



# Units -----------------------------------------------------------------------
global_values %>%
    select(em, units) %>%
    distinct()


ini <- system.file(package = "hector", "input/hector_ssp245.ini")
hc <- newcore(ini)
run(hc)
vars <- c(EMISSIONS_BC(), EMISSIONS_CH4(), EMISSIONS_CO(), FFI_EMISSIONS(),
          EMISSIONS_N2O(), EMISSIONS_NH3(), EMISSIONS_NMVOC(), EMISSIONS_NOX(),
          EMISSIONS_OC(), EMISSIONS_SO2())
fetchvars(hc, 1990, vars = vars) %>%
    select(variable, units) %>%
    distinct() %>%
    mutate(em = gsub(pattern = "_emissions", x = variable, replacement = ""),
           hector_units = units) %>%
    select(em, hector_units) ->
    hector_units_df

global_values %>%
    filter(em %in% c("BC", "CH4", "CO", "N2O", "NH3", "NMVOC", "OC")) %>%
    mutate(value = value * 0.001) ->
    converted_ceds

global_values %>%
    select(em, ced_units = units) %>%
    distinct ->
    ceds_units

hector_units_df %>%
    inner_join(ceds_units)



vars <- c(EMISSIONS_BC(), EMISSIONS_CH4(), EMISSIONS_CO(), EMISSIONS_N2O(),
          EMISSIONS_NH3(), EMISSIONS_NMVOC(), EMISSIONS_OC())
fetchvars(hc, 1750:2025, vars = vars) %>%
    mutate(em = gsub(pattern = "_emissions", x = variable, replacement = "")) ->
    hector_emiss

converted_ceds %>%
    mutate(value = if_else(em == "CO", value /28.01, value)) ->
    converted_ceds

ggplot() +
    geom_line(data = converted_ceds, aes(year, value, color = "ceds")) +
    geom_line(data = hector_emiss, aes(year, value, color = "hector")) +
    facet_wrap("em", scales = "free")




