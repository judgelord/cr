> #  UPDATE: The functions in this repository have migrated to the [`congressionalrecord`](https://judgelord.github.io/congressionalrecord/)  R package. Active development is now in the [package repository](https://github.com/judgelord/congressionalrecord/).


# Scrape, parse, and analyze the Congressional Record 

This repo (and the package) contains functions to do three things:

### 1. Scrape the text of the congressional record and metadata from congress.gov

- `scraper.R` contains functions to scrape sections of the record and save them as htm files in a directory. `get_cr_df()` first scrapes metadata for all subsections for each day of the record, including headers and links to the raw text. `get_cr_htm()` then downloads the raw text of each subsection as a .htm file to a "data/htm" directory. 
- scraper methods are described [here](https://judgelord.github.io/cr/scraper.html)

![](docs/figs/cr_subtypes-1.png)

### 2. Parse the record into .txt files by speaker

- `parser.R` contains functions to parse htm sections of the record by speaker and tag members with their ICPSR ID numbers. By default, `parse_cr()` will parse all htm files in the "data/htm" directory for dates that do not already appear in a "data/txt" directory. You may specify a custom vector of `dates` or `skip_parsed = FALSE` if you don't want to skipped files already parsed.
- the parser methods are explained [here](https://judgelord.github.io/cr/speakers)
- `count_speeches.R` contains methods to count speeches per member
- summary tables and figures of speech counts are [here](https://judgelord.github.io/cr/summary.html)
- the parsed .txt directory looks like this:

![](figs/winstat.png)

### 3. Count and extract sentences that contain user-specified phrases

- Preliminary work on feature extraction is [here](https://judgelord.github.io/cr/features)  
- There is also a brief tutorial on feature extraction at the end of the scraper docs [here](https://judgelord.github.io/cr/scraper.html#Text_features)

![](docs/figs/covid-1.png)


### Notes about these data and methods

Notes about the parser

- The parser inserts ":::" after the speaker's name in the parsed text to make them easy to extract.
- Parsed speeches include where members introduce legislation into the record. These are actually read by the clerk (beginning with the string "The Clerk read the title of the bill."), but I decided to keep them associated with the member who introduced them. 
- The introduction of speakers to the floor appear as separate "speeches." Most all analyses will delete these and other super-short texts that are procedural things like "I yield back." Introductions in the parsed text look like this: "HON. JOHN D. DINGELL :::  of Michigan in the house of representatives"--notably these intros contain state names.
- Occasionally, when a speaker makes a long speech, GPO divides it into subsections that don't have the speaker's name on the page (either in html or pdf). (For example, [this speech](https://www.congress.gov/congressional-record/2020/12/17/senate-section/article/S7563-8) by Senator Durbin). These get parsed as "headers" but are much longer than the typical header and are thus easy to identify. In the next version of the parser or some post hoc correction, I hope to re-attach these to the earlier pages of the speech.


### Other notes 

Hein-bound data   

- Before deciding to scrape and parse the record from scratch, I tried to use existing speech data from Hein bound. Unfortunately, these data are incomplete and have a few errors. Most notably, they do not retain the date of the speech. My notes on these data and code to merge them with voteview data [here](https://judgelord.github.io/cr/member_data). A few plots comparing Hein-bound speech counts to NOMINATE scores and vote share are [here](https://judgelord.github.io/cr/speeches)
