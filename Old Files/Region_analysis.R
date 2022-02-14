rm(list = ls())

load('Data/cleaned_data.RData')

region = data %>%
  mutate(Region = case_when(State == 'FL' ~ 'Southeast',
                            State == 'PA' ~ 'Northeast',
                            State == 'WI' ~ 'Midwest',
                            State == 'KS' ~ 'Midwest',
                            State == 'NM' ~ 'Southwest',
                            State == 'CA' ~ 'West',
                            State == 'AR' ~ 'Southeast',
                            State == 'TX' ~ 'Southwest',
                            State == 'MI' ~ 'Midwest',
                            State == 'IN' ~ 'Midwest',
                            State == 'CT' ~ 'Northeast',
                            State == 'VT' ~ 'Southeast',
                            State == 'NC' ~ 'Southeast',
                            State == 'NY' ~ 'Northeast',
                            State == 'AK' ~ 'West',
                            State == 'MT' ~ 'West',
                            State == 'TN' ~ 'Southeast',
                            State == 'AL' ~ 'Southeast',
                            State == 'IL' ~ 'Midwest',
                            State == 'ND' ~ 'Midwest',
                            State == 'NE' ~ 'Midwest',
                            State == 'MO' ~ 'Midwest',
                            State == 'WA' ~ 'West',
                            State == 'MA' ~ 'Northeast',
                            State == 'OR' ~ 'West',
                            State == 'WV' ~ 'Southeast',
                            State == 'LA' ~ 'Southeast')) %>%
  distinct(College, Region) %>%
  group_by(Region)

NE = length(which(region$Region == 'Northeast'))
SE = length(which(region$Region == 'Southeast'))
MW = length(which(region$Region == 'Midwest'))
SW = length(which(region$Region == 'Southwest'))
W = length(which(region$Region == 'West'))
