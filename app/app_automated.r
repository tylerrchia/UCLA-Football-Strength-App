# app.R
# -----
# Shiny UI for VALD roster explorer + player card + correlations
# Uses objects produced by source("metrics_automated.R"):
#   roster_view, roster_percentiles_wide, roster_percentiles_long,
#   vald_tests_long_ui, keep_roster_metrics, fill_summary, as_of_date
#
# NOTE: Your test data probably doesn't include Position/Class/Headshot.
# This app will gracefully fall back (filters hidden, radar = NordBord vs ForceDecks).

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(plotly)

source("metrics_automated.r")
