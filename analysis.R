## ------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------


#| label: setup


#| echo: true


#| results: hide


#| message: false


#| warning: false


#| include: false


# Setup Chunk
knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  results = 'hold',
  fig.align = "center",
  attr.source = '.numberLines',
  cache = FALSE
)
library(tidyverse)
library(psych)
library(psychTools)
library(dplyr)
library(knitr)
library(epitools)
library(broom)
library(modelsummary)
library(janitor)
library(readxl)
library(sf)
library(tigris)
library(ggrepel)
library(ggfortify)
set.seed(24)



# Custom Model Summary function
ms <- function(models, ...) {
  g <- broom::glance(models)
  modelsummary(models, stars = FALSE, output = "markdown",
               shape = model ~ term, fmt = 2,
               statistic = c("({std.error})", "p = {p.value}"),
               notes = paste0("R2 = ", round(g$r.squared, 3),
                              ", Adj. R2 = ", round(g$adj.r.squared, 3),
                              ", F = ", round(g$statistic, 2),
                              ", p = ", format.pval(g$p.value, digits = 3),
                              ", N = ", g$nobs), ...)
}



## ----------------------------------------------------------------------------------------

# Script was scraped from the QMD I used to render the pdf so there may be some odd formatting but the code runs without error

#| label: data-loading


#| include: false

# Place the data files listed below in this folder before running.

# Pollution Data
airport <- read_excel("airport_runway_2020_mapapp.xlsx", skip = 2)

# Enplanement Data
enplanements <- read_excel("cy19-all-enplanements.xlsx")

# County-level Cancer Data
county <- read_excel("national_cancerrisk_by_county_srcgrp.xlsx")


# Renaming Variables for legibility
enplanements <- enplanements |>
  rename(
    faa_id = Locid,
    cy19_enplanements = `CY 19 Enplanements`,
    cy18_enplanements = `CY 18 Enplanements`,
    pct_change = `% Change`
  )

airport <- airport |>
  rename(faa_id = `FAA Locator ID`)



# Join Datasets
combined <- enplanements |>
  inner_join(airport, by = "faa_id")



# Filtering to counties in New England
county_ne <- county |>
  filter(`EPA Region` == "EPA Region 1") |>
  select(
    FIPS,
    State,
    County,
    Population,
    total_risk     = `Total Cancer Risk (per million)`,
    airport_risk   = `NR-Point-Airports Cancer Risk (per million)`,
    onroad_gas     = `OR-LightDuty-OnNetwork-Gas Cancer Risk (per million)`,
    onroad_diesel  = `OR-HeavyDuty-OnNetwork-Diesel Cancer Risk (per million)`,
    background     = `BACKGROUND Cancer Risk (per million)`
  ) |>
  mutate(pct_from_airport = (airport_risk / total_risk) * 100)



# County-level Airport Data (Whole Country)
airport_by_county <- combined |>
  group_by(FIPS) |>
  summarise(
    n_airports = n(),
    total_enplanements = sum(cy19_enplanements, na.rm = TRUE),
    total_lead = sum(Lead, na.rm = TRUE),
    total_benzene = sum(Benzene, na.rm = TRUE),
    total_formaldehyde = sum(Formaldehyde, na.rm = TRUE),
    total_1_3_butadiene = sum(`1,3-Butadiene`, na.rm = TRUE),
    .groups = "drop"
  )



# Combining New England counties with their airport data
county_combined <- county_ne |>
  left_join(airport_by_county, by = "FIPS") |>
  mutate(
    n_airports = coalesce(n_airports, 0L),
    total_enplanements = coalesce(total_enplanements, 0)
  )



## ----------------------------------------------------------------------------------------


#| label: summary-table-prep


#| include: false


# Creating Summary Table
summary_table <- county_combined |>
  group_by(State) |>
  summarise(
    Population      = sum(Population) / 1e6,
    mean_risk       = mean(airport_risk),
    sd_risk         = sd(airport_risk),
    mean_pct        = mean(pct_from_airport),
    sd_pct          = sd(pct_from_airport),
    Enplanements    = sum(total_enplanements) / 1e6,
    .groups = "drop"
  ) |>
  arrange(desc(mean_risk))



## ----------------------------------------------------------------------------------------


#| label: plots-prep


#| include: false


# Distribution Plot
cancer_distribution <-
  ggplot(county_combined, aes(x = airport_risk)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Aviation-Attributable Cancer Risk",
    subtitle = "New England counties, 2020",
    x = "Airport cancer risk (per million people)",
    y = "Number of counties",
    caption = "Source: EPA AirToxScreen 2020"
  ) +
  theme_minimal()



# Activity vs. Risk Plot
activity_vs_risk <-
  ggplot(county_combined |> filter(total_enplanements > 0),
         aes(x = total_enplanements, y = airport_risk)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Airport Activity vs. Modeled Cancer Risk",
    subtitle = "New England counties with nonzero enplanement, 2020",
    x = "Total enplanements (log scale)",
    y = "Airport cancer risk (per million, log scale)",
    caption = "Source: EPA AirToxScreen 2020; FAA Passenger Boarding Data 2019"
  ) +
  theme_minimal()



# New England map data and chart creation
options(tigris_use_cache = TRUE)

ne_counties <- counties(
  state = c("ME", "NH", "VT", "MA", "CT", "RI"),
  cb = TRUE,
  year = 2020
)

county_combined2 <- county_combined |>
  mutate(GEOID = sprintf("%05d", as.integer(FIPS)))

ne_map_data <- ne_counties |>
  left_join(county_combined2, by = "GEOID")

attributable_cancer <-
  ggplot(ne_map_data) +
  geom_sf(aes(fill = pct_from_airport), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    name = "Aviation's share\nof cancer risk (%)",
    option = "plasma",
    limits = c(0, 1),
    oob = scales::squish,
    breaks = c(0.1, 0.25, 0.5, 1),
    labels = c("0.1", "0.25", "0.5", "1+"),
    na.value = "grey90"
  ) +
  coord_sf(crs = 5070) +
  labs(
    title = "Aviation-Attributable Cancer Risk by County",
    caption = "Color scale capped at 1%; Nantucket County, MA exceeds this value."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(hjust = 0),
    plot.margin = margin(10, 10, 10, 20)
  )



## ----------------------------------------------------------------------------------------


#| label: regression-prep


#| include: false


# --- Regression Models ---

county_reg <- county_combined |>
  filter(total_enplanements > 0) |>
  mutate(
    log_enplanements = log(total_enplanements),
    log_airport_risk = log(airport_risk),
    log_population = log(Population)
  )



# Model 1: Bivariate
m1 <- lm(log_airport_risk ~ log_enplanements, data = county_reg)



# Model 2: Adds number of airports
m2 <- lm(log_airport_risk ~ log_enplanements + n_airports, data = county_reg)



# Model 3: Adds population control
m3 <- lm(log_airport_risk ~ log_enplanements + n_airports + log_population,
         data = county_reg)



## ----------------------------------------------------------------------------------------


#| label: new-plots-prep


#| include: false


# --- Risk Source Comparison by State ---

risk_comparison <- county_combined |>
  group_by(State) |>
  summarise(
    Airport = mean(airport_risk),
    `On-Road Gas` = mean(onroad_gas),
    `On-Road Diesel` = mean(onroad_diesel),
    .groups = "drop"
  ) |>
  pivot_longer(-State, names_to = "Source", values_to = "Risk") |>
  mutate(Source = fct_reorder(Source, Risk))

risk_comparison_plot <-
  ggplot(risk_comparison, aes(x = reorder(State, -Risk), y = Risk, fill = Source)) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean Cancer Risk by Source and State",
    subtitle = "New England counties, 2020",
    x = NULL,
    y = "Cancer risk (per million)",
    caption = "Source: EPA AirToxScreen 2020"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



# --- Top Counties by % from Airport ---

top_counties <- county_combined |>
  slice_max(pct_from_airport, n = 15) |>
  mutate(label = paste0(County, ", ", State))

top_counties_plot <-
  ggplot(top_counties, aes(x = pct_from_airport,
                           y = reorder(label, pct_from_airport))) +
  geom_point(color = "steelblue", size = 3) +
  geom_segment(aes(xend = 0, yend = label), color = "grey70") +
  labs(
    title = "Counties With Highest Aviation Share of Cancer Risk",
    subtitle = "Top 15 New England counties, 2020",
    x = "Aviation's share of total cancer risk (%)",
    y = NULL,
    caption = "Source: EPA AirToxScreen 2020"
  ) +
  theme_minimal()



# --- Lead vs. Airport Risk ---

lead_plot <-
  county_combined |>
  filter(total_lead > 0) |>
  ggplot(aes(x = total_lead, y = airport_risk)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Lead Emissions vs. Modeled Airport Cancer Risk",
    subtitle = "New England counties with nonzero lead emissions, 2020",
    x = "Total lead emissions (log scale)",
    y = "Airport cancer risk (per million, log scale)",
    caption = "Source: EPA AirToxScreen 2020"
  ) +
  theme_minimal()



# --- Residual Diagnostics ---

county_reg <- county_reg |>
  mutate(
    fitted = fitted(m1),
    resid = resid(m1),
    label = paste0(County, ", ", State),
    big_resid = abs(resid) > quantile(abs(resid), 0.85)
  )

residual_plot <-
  ggplot(county_reg, aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(color = "steelblue", size = 2) +
  ggrepel::geom_text_repel(
    data = county_reg |> filter(big_resid),
    aes(label = label), size = 3, max.overlaps = 12
  ) +
  labs(
    title = "Residuals vs. Fitted Values",
    subtitle = "Bivariate log-log model",
    x = "Fitted values",
    y = "Residuals",
    caption = "Labeled: counties with largest absolute residuals"
  ) +
  theme_minimal()




## ----------------------------------------------------------------------------------------


#| label: regression-table
regression_table = modelsummary(
  list("Bivariate" = m1, "With Airports" = m2, "With Controls" = m3),
  stars = TRUE,
  fmt = 3,
  output = "markdown",
  statistic = "({std.error})",
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  title = "OLS: Log Airport Cancer Risk on Log Enplanements"
)




## ------------------------------------------------------------------------------------

#| label: tbl-summary

#| tbl-cap: "Summary Statistics by State, New England Counties (2020)"
summary_table |>
  knitr::kable(
    digits = 2,
    col.names = c("State", "Pop (M)",
                  "Mean Risk", "SD Risk",
                  "Mean % Airport", "SD % Airport",
                  "Enpl. (M)")
  )


## ------------------------------------------------------------------------------------

#| label: fig-risk-comparison

#| fig-cap: "Mean Cancer Risk by Source and State"
risk_comparison_plot


## ------------------------------------------------------------------------------------

#| label: fig-activity-vs-risk

#| fig-cap: Airport Activity vs. Modeled Aviation-Attributable Cancer Risk
activity_vs_risk


## ------------------------------------------------------------------------------------

#| fig-cap: Map of Modeled Aviation-Attributable Cancer Risk by County
attributable_cancer


## ------------------------------------------------------------------------------------

#| label: tbl-regression

#| tbl-cap: "OLS Regression Results: Log Airport Cancer Risk on Log Enplanements"
modelsummary(
  list("Bivariate" = m1, "With Airports" = m2, "With Controls" = m3),
  stars = TRUE,
  fmt = 3,
  output = "markdown",
  statistic = "({std.error})",
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  coef_rename = c(
    "(Intercept)" = "Intercept",
    "log_enplanements" = "Log Enplanements",
    "n_airports" = "Number of Airports",
    "log_population" = "Log Population"
  )
)


## ------------------------------------------------------------------------------------

#| label: fig-top-counties

#| fig-cap: "Counties With Highest Aviation Share of Cancer Risk"

#| fig-height: 6
top_counties_plot


## ------------------------------------------------------------------------------------

#| label: fig-lead-risk

#| fig-cap: "Lead Emissions vs. Modeled Airport Cancer Risk"
lead_plot


## ------------------------------------------------------------------------------------

#| fig-cap: Distribution of Modeled Aviation-Attributable Cancer Risk in New England Counties
cancer_distribution


## ------------------------------------------------------------------------------------

#| label: fig-diagnostic

#| fig-cap: "Regression Diagnostic Plots, Bivariate Log-Log Model"

#| fig-width: 11

#| fig-height: 7
county_reg_named <- county_reg |>
  mutate(row_label = paste0(County, ", ", State))
rownames(county_reg_named) <- county_reg_named$row_label
m1 <- lm(log_airport_risk ~ log_enplanements, data = county_reg_named)
residual_plots <- autoplot(m1, which = 1:4, colour = "steelblue", size = 2,
                           label.size = 3, ncol = 2) +
  theme_minimal() + 
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(20, 30, 20, 30))
residual_plots

