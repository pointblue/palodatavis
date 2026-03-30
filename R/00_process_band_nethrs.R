# PACKAGES
library(tidyverse)
library(foreign)


# EFFORT-------------

nethrspath <- "rawdat/allpalonthrs.dbf" # local copy of entire database
effort_raw <- foreign::read.dbf(nethrspath) |> select(PROJECT:DUPE) # (drop extra columns labeled "X")

# summarize total net hours per year:
effort = sum_pn_nethrs(effort_raw) 
write_csv(effort, 'output/nethrs_effort.csv')

# BAND---------
bandpath <- "rawdat/allnumb.dbf" # local copy of allnumb

band_raw = foreign::read.dbf(bandpath) # slow because this is a large database!
str(band_raw)

# filter to PN after 1979 and no later than last net hours
# > NOTE: not limited to new captures
unique(band_raw$LOC)
summary(band_raw$DATE) # 1966-01-14 thruogh 2026-03-26 (and 1 NA)

band = band_raw |> select(INITIALS:COM) |> 
  filter(DATE <= max(effort_raw$DATE)) |> 
  filter(DATE >= min(effort_raw$DATE)) |> 
  count_captures()
  
write_csv(band, 'output/band_captures.csv')

testthat::test_that('test that net hours and captures match years', {
  testthat::expect_true(all(unique(effort$year) %in% band$year))
  testthat::expect_true(all(unique(band$year) %in% effort$year))
})
