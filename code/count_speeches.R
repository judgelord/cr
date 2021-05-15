
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

d %<>% mutate(n_speeches = map2_int(year, icpsr, n_speeches))

d_count <- d
save(d_count, file = here("data", "d_count.Rdata"))

dim(d)
d %<>% filter(n_speeches > 0)
dim(d)

# file names
file_names <- function(year, i) {
  list.files(here('data', 'txt', year, i) )
}

d %<>% mutate(file_name = map2(year, icpsr, file_names))

# unlist 
d %<>% unnest(file_name)
head(d)

test_file <- here('data', 'txt', d$year[1], d$icpsr[1], d$file_name[1])
test_file
read_lines(test_file)

read_lines(test_file) %>% 
  #str_c(sep = " ") %>% 
  nchar()

# file size
d %<>% mutate(file_size = here('data', 'txt', year, icpsr, file_name) %>% 
                file.size()
              )

head(d)

# filter out small files
d %<>% filter(file_size > 1000)
dim(d)

# nchar
d %<>% 
  group_by(file_name) %>% 
  mutate(nchar = here('data', 'txt', year, icpsr, file_name) %>% 
                read_lines() %>% 
                str_c(sep = " ") %>% 
                nchar()
)%>% 
  ungroup()

# variation
d$nchar %>% min() 
d$nchar %>% max() 

# save
d_files <- d
save(d_files, file = here("data", "d_files.Rdata"))


# Merge in metadata 
load(here("data", "cr_metadata.Rdata"))
head(cr_metadata)

legislation <- "ACT|BILL|RESOLUTION|AMMENDMENT|EARMARK|SPONSORS"

business <- "PRAYER|PLEDGE OF ALLEGIANCE|MORNING BUSINESS|MESSAGE|CONDEMING|APPOINTMENT|NOMINATION|CONFIRMATION|REPORT|PETITION|MEMORIAL|COMMUNICATION"
  
process <- "RECOGNIZED FOR [0-9] MINUTES|UNANIMOUS CONSENT|ADJOURNMENT|PETITION| ORDER|^ORDER|MOTION|RECESS|CALENDAR|RECOGNITION|SPEAKER PRO TEMPORE"
  

# clean up headers 
cr_metadata %<>% 
  mutate(header = header %>% toupper(),
         legislation = str_extract(header, legislation),
         business = str_extract(header, business),
         proess = str_extract(header, process),
         subtype = coalesce(legislation, business, process, header) %>% 
           str_remove(";.*") %>% 
           str_remove_all(" BY .*| UNTIL.*| \\(EXECUTIVE.*")
  )


to_exclude <- str_c(business, process, sep = "|") %>% str_split("") 

cr_metadata %<>% filter(!subtype %in% to_exclude )

count(cr_metadata, subtype, sort = T) %>% head(100) %>% kable(format = "pipe")

# Merge 
cr_metadata %<>% rename(file_htm = file)

cr_metadata$file_htm %>% head()

# from file name to cr htm file name
d %<>% mutate(file_htm = file_name %>% 
  str_replace("-[0-9]+-[0-9]+.txt", ".htm") )

d %<>% left_join(cr_metadata)

d %>% select(header)

