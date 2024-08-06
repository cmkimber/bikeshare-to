io_daily <- rides_2022_sf %>%
  count(Start.Station.Id, Trip.Date = date(Start.Time)) %>%
  rename(Start.Count = n) %>%
  left_join(rides_2022_sf %>%
              count(End.Station.Id, Trip.Date = date(End.Time)) %>%
              rename(End.Count = n),
            by = join_by(Start.Station.Id == End.Station.Id,
                         Trip.Date)) %>%
  mutate(IO.Ratio = Start.Count/End.Count)

io_monthly <- rides_2022_sf %>%
  count(Start.Station.Id, Trip.Month = month(Start.Time)) %>%
  rename(Start.Count = n) %>%
  left_join(rides_2022_sf %>%
              count(End.Station.Id, Trip.Month = date(End.Time)) %>%
              rename(End.Count = n),
            by = join_by(Start.Station.Id == End.Station.Id,
                         Trip.Month)) %>%
  mutate(IO.Ratio = Start.Count/End.Count)

io_yearly <- rides_2022_sf %>%
  count(Start.Station.Id) %>%
  rename(Start.Count = n) %>%
  left_join(rides_2022_sf %>%
              count(End.Station.Id) %>%
              rename(End.Count = n),
            by = join_by(Start.Station.Id == End.Station.Id)) %>%
  mutate(IO.Ratio = Start.Count/End.Count)
  