sum_pn_nethrs = function(dat) {
  dat |> 
    # filter to PN after 1979
    filter(LOCATION == 'PN') |> 
    filter(DATE >= '1979-01-01') |> 
    mutate(year = format(DATE, '%Y'),
           month = format(DATE, '%m')) |> 
    # summarize by year and month
    group_by(year, month) |> 
    summarize(nethours = sum(NETHOURS),
              .groups = 'drop')
}

count_captures = function(dat) {
  dat_subset = dat |> 
    # filter to PN after 1979
    filter(LOC == 'PN') |> 
    filter(DATE >= '1979-01-01') |> 
    mutate(year = format(DATE, '%Y'),
           month = format(DATE, '%m')) |> 
    # summarize number of captures by species, year, and month
    group_by(SPEC, year, month) |> 
    count() |> 
    ungroup()
}
