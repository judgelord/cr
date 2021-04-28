
# Load packages and functions
source("code/setup.R")

# Load member data from voteview
load(here('data', 'members.Rdata'))

# years in data 
years <- tibble(year = list.files(here('data', 'txt')) %>% as.numeric()) %>% 
  mutate(congress = year_congress(year))

# join in member data for years
d <- years %<>% full_join(members %>% distinct(icpsr, congress, state_abbrev))

# count speeches per icpsr id
n_speeches <- function(year, i) {
  list.files(here('data', 'txt', year, i) ) %>% 
    length()}

## Test
# map2_int(.x = d$year,
#          .y = d$icpsr,
#         .f = n_speeches)

d_count <- d %>% mutate(n_speeches = map2_int(year, icpsr, n_speeches))

save(d_count, file = here("data", "d_count.Rdata"))