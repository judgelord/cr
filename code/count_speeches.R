load(here('data', 'members.Rdata'))

years <- tibble(year = list.files(here('data', 'txt')) %>% as.numeric()) %>% 
  mutate(congress = year_congress(year))

d <- years %<>% full_join(members %>% distinct(icpsr, congress, state_abbrev))

d <- years

# count speeches per icpsr id
n_speeches <- function(year, i) {
  list.files(here('data', 'txt', year, i) ) %>% 
    length()}

## Test
# map2_int(.x = d$year,
#          .y = d$icpsr,
#         .f = n_speeches)

d %<>% mutate(n_speeches = map2_int(year, icpsr, n_speeches))
d$year %>% unique()
d_count <- d
save(d_count, file = here("data", "d_count.Rdata"))