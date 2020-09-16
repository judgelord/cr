##########################################################################################################
# This script defines functions for cleaning and extracting names of members of congress
#' cleanFROMcolumn() preprocesses text to make matching more likely
#' formatFirstName() and formatLastName()formats names to look like those provided by voteview. These are used by other functions
#' extractMemberNames looks for names in many formats
#' addFirst() adds first names given last names, but only to last names that are unique in congress. This should be used with caution.
##########################################################################################################

## FOR TESTING, use names that are failing to match
if(F){
  # data frame of names that were recently failing to match for testing
  data <- gs_title("worst.names") %>% gs_read() 
  data %<>% 
    mutate(congress = str_split(congress,";")) %>%  # unnest congresses
    unnest(congress) %>%
    mutate(congress = as.numeric(congress)) %>%
    filter(!problem %in% c("other", "not unique"), !solution %in% c("don't fix")) # drop cases we know we don't need to fix
  
  data$last_name <- formatLastName(data, "FROM")
  # agencies to use add_first on
  data %>% 
    add_first() %>% 
    filter(!is.na(first_name) ) %>%
    mutate(agency = str_split(agency,";")) %>%  # unnest congresses
    unnest(agency) %>%
    count(agency, sort = T)
  
  # names where add_first fails 
  data %>% 
    #mutate(last_name = str_remove(last_name, " .*")) %>% 
    add_first() %>% 
    filter(is.na(first_name)&solution == "addFirst") %>%
    mutate(agency = str_split(agency,";")) %>%  # unnest agencies
    unnest(agency) %>%
    count(FROM, congress, last_name, sort = T) %>% .$last_name
  
  # vectors for testing 
  FROM <- data$FROM
  col_name <- FROM
  congresses <- unique(data$congress)
  
  # Test
  data %<>% extractMemberName(col_name = "FROM", members = members)
  look <- data %>% 
    group_by(agency, FROM, string, problem, solution, last_name) %>% 
    summarise(congress = str_c(unique(congress), collapse = ";")) %>%
    filter(is.na(last_name)) %>%
    arrange(solution, problem)
}

# This function cleans up text from which member names will be extracted.
# SUCH CODE SHOULD BE CONSOLIDATED HERE
# It is used in extractMemberNames etc. to preprocess text.
cleanFROMcolumn <- function(FROM){

  # remove +
  FROM %<>% str_remove('\\+') %>% str_remove("—")
  
  # remove common names in quotes 
  FROM <- gsub('\\"(Bill|Bobby|Buddy|GT|Buck|Chuck|Hank|Rick|Duke|Randy|Andy)\\"', "", FROM, ignore.case = TRUE)
  
  # remove common names in parentheses
  FROM <- gsub('\\((Bill|Bobby|Buddy|GT|Buck|Chuck|Hank|Rick|Duke|Randy|Andy)\\)', "", FROM, ignore.case = TRUE)
  
  
  # remove paragraph breaks
  FROM %<>% str_replace_all("\n", " ") 
  
  # remove extra white space inside strings
  FROM %<>% str_squish()
  
  # fix misplaced commas
  #FROM <- gsub("(\\w+) ,(\\w+)|(\\w+) , (\\w+)", "\\1, \\2", FROM)
  FROM  %<>% str_replace_all(" , | ,|,", ", ")
  
  # remove extra white space inside strings again
  FROM %<>% str_squish()
  
  # remove 
  FROM %<>% str_remove(" Jr\\.| Jr| III| II| Ii| IV| ll| \\(Il\\)|, JR\\.")
  
  # replace with comma
  FROM %<>% str_replace(pattern = " Jr,| CPA,| M\\.D\\.,| MD,| M\\.C\\.,| P\\.E\\.,| Ii,",
                        replacement = ",")

  # remove paragraph breaks
  FROM %<>% str_replace_all("\n", " ") 
  
  # remove extra white space inside strings again
  FROM %<>% str_squish()
  
  # Delete titles that appear after a commma
  # FROM %<>% str_replace(", (SEN|Sen)(-|\\b)", ", ")
  #FROM <- gsub(", (REP|Rep)(-|\\b)", ", ", FROM)
  
  # Replace titles at the beginning of a string or not after a comma 
  FROM %<>% str_replace("\\b(SEN|Sen)( |- | - |\\. |\\.)|^S( |- | - |\\. |\\.)", 
                        "Senator ")
  
  FROM %<>% str_replace("\\b(REP|Rep)( |- | - |\\. |\\.)|^R( |- | - |\\. |\\.)|Congressman|Congresswoman", 
                        "Representative ")
  
  # trim down extra spaces
  FROM %<>% str_squish()

  # remove periods
  FROM %<>% str_replace_all("\\.", " ") %>% str_squish()
  
  #removing double commas
  FROM %<>% str_replace(",+ |, ,", ", ")
  FROM %<>% str_replace(",+ |, ,", ", ")
  FROM %<>% str_replace_all(",,", ",")
  
  #removing spaces before commas
  FROM %<>% str_replace(" ,", ", ") 
  
  # replace spaces with a single space
  FROM %<>% str_squish()
  
  return(FROM)
}

###################################################################################################################

# Formats col_name (usually last_name) to similiar format as members$last_name
# Capitalizes letters and fixes common errors 
formatLastName <- function(data, col_name){
  
  data$last_name <- data[[col_name]]
  
  # trim white space and paragraph breaks
  data$last_name  %<>% 
    str_squish() %>% 
    # correct capitalization to match last names in voteview data 
    # Last names in voteview are upper case
    str_to_upper() %>%
    str_replace_all(" NA ", " ") %>% 
    str_replace_all(" NA ", " ") %>%
    str_replace_all(" NA ", " ") %>%
    str_remove_all("^NA | NA$") %>% 
    str_remove_all("^NA | NA$") %>% 
    # remove anything in parentheses
    str_remove_all("\\(.*\\)") %>% 
    str_squish()
  
  # THIS WILL STAY IN THE FUNCTION formatLastName
  data %<>%
    #case corrections, not touching at the moment
    mutate(last_name = gsub("^MC", replacement = "Mc", last_name)) %>% 
    mutate(last_name = gsub("McEACHIN", replacement = "MCEACHIN", last_name, ignore.case = TRUE)) %>% 
    mutate(last_name = gsub("DEFAZIO", replacement = "DeFAZIO", last_name, ignore.case = TRUE)) %>% 
    mutate(last_name = gsub("DELAURO", replacement = "DeLAURO", last_name)) %>% 
    mutate(last_name = gsub("DEMINT", replacement = "DeMINT", last_name)) %>% 
    mutate(last_name = gsub("LOBIONDO", replacement = "LoBIONDO", last_name)) %>% 
    mutate(last_name = gsub("LATOURETTE", replacement = "LaTOURETTE", last_name)) %>% 
    mutate(last_name = gsub("LAHOOD", replacement = "LaHOOD", last_name)) %>% 
    mutate(last_name = gsub("DEGETTE", replacement = "DeGETTE", last_name)) %>% 
    mutate(last_name = gsub("DELBENE", replacement = "DelBENE", last_name)) %>% 
    mutate(last_name = gsub("DESANTIS", replacement = "DeSANTIS", last_name)) %>% 
    mutate(last_name = gsub("MACARTHUR", replacement = "MacARTHUR", last_name)) %>% 
    mutate(last_name = gsub("LAMALFA", replacement = "LaMALFA", last_name)) %>% 

    # Commented this out because we modified the members file instead, but maybe it would better to modify just the search pattern to be Luj.n as a typo
    # FIXED
    # mutate(last_name = ifelse(grepl("Lujan", FROM,ignore.case=TRUE)&grepl("Ben", FROM,ignore.case=TRUE), "LUJÁN", last_name)) %>% 
    # mutate(last_name = ifelse( grepl("Lujan",FROM,ignore.case=TRUE)&grepl("Ben",FROM,ignore.case=TRUE), "LUJÁN", last_name)) %>%
    

    ##############################################################################################################################
    # FIXED
    # All of the below should be corrected with the typos tables (if a typo) or in nameCongress.R (if a name that needs expanding)
    # Spelling and specific corrections
  
    # FIXED and added names to typo tables
    mutate(last_name = gsub("DENIS", replacement = "DENNIS", last_name)) %>% #fixed
    mutate(last_name = gsub("DUNCAN JOHN.*", replacement = "DUNCAN", last_name)) %>% #fixed
    mutate(last_name = gsub("JOHNSON HENRY.*", replacement = "JOHNSON", last_name)) %>% #fixed
    mutate(last_name = gsub("BONO MACK.*", replacement = "BONO", last_name)) %>% #this should be Mary not Mack #fixed 
    mutate(last_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "ROCKEFELLER", last_name)) %>% #fixed
    mutate(last_name = gsub(".*SANDLIN.*", replacement = "HERSETH SANDLIN", last_name)) %>%  #fixed


    mutate(last_name = gsub("MOORE CAPITO.*", replacement = "CAPITO", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Milkulski, Barbara", FROM), "MIKULSKI", last_name)) %>% #fixed 
    mutate(last_name = ifelse(grepl("GRESHAM BARRETT", last_name,ignore.case=TRUE), "BARRETT", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Shelley Moore", FROM,ignore.case=TRUE), "CAPITO", last_name)) %>% #fixed
    mutate(last_name = gsub(".*SCHULTZ.*", replacement = "WASSERMAN SCHULTZ", last_name)) %>% #will be fixed below
    mutate(last_name = ifelse( grepl("Jackson",FROM,ignore.case=TRUE)&grepl("She",FROM)&grepl("Lee",FROM), "JACKSON LEE", last_name)) %>% #fixed
    mutate(last_name = ifelse( (grepl("McMorris|Rodgers",FROM,ignore.case=TRUE))&grepl("Cathy|McMorris",FROM), "McMORRIS RODGERS", last_name)) %>% #fixed below
    mutate(last_name = ifelse( grepl("Michael|(^| )K",FROM,ignore.case=TRUE)&grepl("Conaway",FROM,ignore.case=TRUE), "CONAWAY", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Ben",FROM)&grepl("Nelson",FROM), "NELSON", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Beutler",FROM,ignore.case=TRUE)&grepl("Herrera",FROM,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Gillbrand",FROM,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Hillary|Hilary",FROM,ignore.case=TRUE)&grepl("Rodham",FROM), "CLINTON", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Sandlin", FROM,ignore.case=TRUE), "HERSETH SANDLIN", last_name)) %>%  #fixed
    mutate(last_name = ifelse(grepl("Murhpy", FROM,ignore.case=TRUE), "MURPHY", last_name)) %>% #fixed
    #mutate(last_name = ifelse( grepl("Linda",FROM,ignore.case=TRUE)&grepl("Sanchez",FROM,ignore.case=TRUE), "SÁNCHEZ", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Wasserman", FROM,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("McMorris", FROM,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("ROS-LEHTINEN", FROM ,ignore.case=TRUE), "ROS-LEHTINEN", last_name)) %>% #not an error
    mutate(last_name = ifelse(grepl(".ISCLOSKY", FROM,ignore.case=TRUE), "VISCLOSKY", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Guitierrez", FROM,ignore.case=TRUE), "GUTIERREZ", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Harmon", FROM,ignore.case=TRUE), "HARMAN", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Hollen", FROM,ignore.case=TRUE), "VAN HOLLEN", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Masto", FROM,ignore.case=TRUE), "CORTEZ MASTO", last_name)) %>%  #fixed
    
    mutate(last_name = ifelse(grepl("Roybal", last_name,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("RAHALL", last_name,ignore.case=TRUE), "RAHALL", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("BEUTLER", last_name,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Inholfe|Imhofe|Imholfe|Inhoffe", last_name,ignore.case=TRUE), "INHOFE", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Barrat|Barret", last_name,ignore.case=TRUE), "BARRETT", last_name)) %>%  #fixed
    mutate(last_name = ifelse(grepl("Stebenow", last_name,ignore.case=TRUE), "STABENOW", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("C.rdenas", last_name,ignore.case=TRUE), "CARDENAS", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Vel.zquez", last_name,ignore.case=TRUE), "VELAZQUEZ", last_name)) %>% #fixed
   
    
    mutate(last_name = gsub("GONZALES", replacement = "GONZALEZ", last_name)) #fixed
  
  
  # data %>% filter(str_detect(last_name, "INHOF")) %>% .$last_name 
  
  
  
  data$last_name %<>% str_replace(" ,", ", ")
  data$last_name %<>% str_squish()

  return(data$last_name)
  
}

##################################################################################################################################

# Formats col_name (usually first_name) to similiar format as members$first_name
# Capitalizes letters appropriately and fixes common errors
formatFirstName <- function(data, col_name){
  
  data$first_name <- data[[col_name]]

  data %<>%
    # In voteview, first names are title case
    mutate(first_name = stri_trans_totitle(first_name))
    
  ##############################################################################################################################
  # FIXED
  # All of the below should be corrected with the typos tables (if a typo) or in nameCongress.R (if a name that needs expanding)
  # Spelling and specific corrections
  data %<>% 
    mutate(first_name = ifelse( grepl("Don",FROM,ignore.case=TRUE)&grepl("Young",FROM,ignore.case=TRUE), "Donald", first_name)) %>% #fixed in namecongress
    #mutate(first_name = ifelse( grepl("Andr",FROM,ignore.case=TRUE)&grepl("Carson",FROM,ignore.case=TRUE), "André", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("John",FROM,ignore.case=TRUE)&grepl("Thune",FROM,ignore.case=TRUE), "John", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("John",FROM,ignore.case=TRUE)&grepl("Rockefeller",FROM,ignore.case=TRUE), "John", first_name)) %>%#no error
    mutate(first_name = ifelse( grepl("Harold",FROM,ignore.case=TRUE)&grepl("Rogers",FROM,ignore.case=TRUE), "Harold", first_name)) %>% #no error
    
    mutate(first_name = ifelse( grepl("James",FROM,ignore.case=TRUE)&grepl("Sensenbrenner",FROM,ignore.case=TRUE), "James", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Richard",FROM,ignore.case=TRUE)&grepl("Blumenthal",FROM,ignore.case=TRUE), "Richard", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Bill",FROM,ignore.case=TRUE)&grepl("Nelson",FROM,ignore.case=TRUE), "Clarence", first_name)) %>% #fixed in namecongress
    mutate(first_name = ifelse( grepl("Fred",FROM,ignore.case=TRUE)&grepl("Upton",FROM,ignore.case=TRUE), "Frederick", first_name)) %>% #fixed in namecongress
    mutate(first_name = ifelse( grepl("Thad",FROM,ignore.case=TRUE)&grepl("Cochran",FROM,ignore.case=TRUE), "William", first_name)) %>% #fixed in namecongress
    mutate(first_name = ifelse( grepl("Kristen",FROM,ignore.case=TRUE)&grepl("Gillibrand",FROM,ignore.case=TRUE), "Kirsten", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("C",FROM,ignore.case=TRUE)&grepl("Ruppersberger",FROM,ignore.case=TRUE), "Dutch", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Paul",FROM,ignore.case=TRUE)&grepl("Gosar",FROM,ignore.case=TRUE), "Paul", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Ros-Lehtinen",FROM,ignore.case=TRUE), "Ileana", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Beutler",FROM,ignore.case=TRUE)&grepl("Herrera",FROM,ignore.case=TRUE), "Jaime", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Will|Bill",FROM,ignore.case=TRUE)&grepl("Owens",FROM,ignore.case=TRUE), "William", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Butterfield",FROM,ignore.case=TRUE)&grepl("G",FROM,ignore.case=TRUE), "George", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("G. K.",FROM,ignore.case=TRUE), "G.K.", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Nelson",FROM,ignore.case=TRUE)&grepl("Ben",FROM,ignore.case=TRUE), "Earl", first_name)) %>% #fixed in namecongress
  #mutate(first_name = ifelse( grepl("Carson",FROM,ignore.case=TRUE)&grepl("Andr",FROM,ignore.case=TRUE), "André", first_name)) %>% #fixed
   #mutate(first_name = ifelse( grepl("Griv",FROM,ignore.case=TRUE)&grepl("Raul",FROM,ignore.case=TRUE), "Raúl", first_name)) %>%
    mutate(first_name = ifelse( grepl("Scott",FROM,ignore.case=TRUE)&grepl("Bobby",FROM,ignore.case=TRUE), "Bob", first_name)) %>% #fixed
    
    
    mutate(first_name = ifelse( grepl("Young",FROM,ignore.case=TRUE)&grepl("C.W|C. W|CW",FROM,ignore.case=TRUE), "Charles", first_name)) %>%  #fixed
    mutate(first_name = ifelse( grepl("Jackson",FROM,ignore.case=TRUE)&grepl("She",FROM)&grepl("Lee",FROM,ignore.case=TRUE), "Sheila", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Gresham",FROM,ignore.case=TRUE)&grepl("Barrett",FROM,ignore.case=TRUE), "James", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Putnam",FROM,ignore.case=TRUE)&grepl("Ad",FROM,ignore.case=TRUE), "Adam", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Lind",FROM,ignore.case=TRUE)&grepl("Graham",FROM,ignore.case=TRUE), "Lindsey", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("SERRANO",FROM,ignore.case=TRUE)&grepl("Jos",FROM,ignore.case=TRUE), "Jose", first_name)) %>% #fixed
    
    mutate(first_name = gsub(pattern = "Christoher", replacement = "Christopher", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Hilllary|Hilary|Fillary", replacement = "Hillary", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Babara", replacement = "Barbara", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Colin", replacement = "Collin", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Melisssa", replacement = "Melissa", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Denis", replacement = "Dennis", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub("Eliott", replacement = "Eliot", first_name)) %>% #fixed
    mutate(first_name = gsub("Brain", replacement = "Brian", first_name)) %>% #fixed
    
    mutate(first_name = gsub("Duncan John.*", replacement = "John", first_name)) %>% #fixed
    mutate(first_name = gsub("Johnson Henry.*", replacement = "Henry", first_name)) #fixed
  
  data$first_name %<>% trimws()
  data$first_name <- gsub("(^ |^  |^   |\n)", "", data$first_name)
  
  return(data$first_name)
  
}


# Fixes common errors when names read in from OCR
# May need adjustments for different agencies/formats
ocr.errors <- function(FROM){
  
  # adds deleted "ll" to last names
  #CAN EDIT HERE
  FROM <- gsub("Hiary", "Hillary", FROM)
  FROM <- gsub("(^| )Dinge($| )", "\\1Dingell\\2", FROM)
  FROM <- gsub("Connoy", "Connolly", FROM)
  FROM <- gsub("(^| )Russe($| )", "\\1Russell\\2", FROM)
  FROM <- gsub("(^| )Swalwe($| )", "\\1Swalwell\\2", FROM)
  FROM <- gsub("Cheie", "Chellie", FROM)
  FROM <- gsub("(^| )Uda($| )", "\\1Udall\\2", FROM)
  FROM <- gsub("(^| )Wooda($| )", "\\1Woodall\\2", FROM)
 # FROM <- gsub("Marsha$", "Marshall", FROM)
  FROM <- gsub("Wiiam", "William", FROM)
  FROM <- gsub("(^| )Cantwe($| )", "\\1Cantwell\\2", FROM)
  FROM <- gsub("Mier$", "Miller", FROM)
  FROM <- gsub("Way$", "Wally", FROM)
  FROM <- gsub("Aen$", "Allen", FROM)
  FROM <- gsub("(^| )Bi($| )", "\\1Bill\\2", FROM)
  FROM <- gsub("Coins", "Collins", FROM)
  FROM <- gsub("Paone|Pal lone", "Pallone", FROM)
  FROM <- gsub("(^| )Campbe($| )", "\\1Campell\\2", FROM)
  FROM <- gsub("Hoen", "Hollen", FROM)
  FROM <- gsub("(^| )Darre($| )", "\\1Darrell\\2", FROM)
  FROM <- gsub("Gaegly", "Gallegly", FROM)
  FROM <- gsub("Giibrand", "Gillibrand", FROM)
  FROM <- gsub("(^| )McConne( |$)", "\\1McConnell\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Ayson", "\\1Allyson", FROM)
  FROM <- gsub("Keer($| |,)", "Keller\\1", FROM)
  FROM <- gsub("(^| )Een($| )", "\\1Ellen\\2", FROM)
  FROM <- gsub("Roybal-Aard", "Roybal-Allard", FROM)
  FROM <- gsub("Cuear($| |,)", "Cuellar\\1", FROM)
  FROM <- gsub("Pascre($| |,)", "Pascrell\\1", FROM)
  FROM <- gsub("Pa one", "Pallone", FROM)
  FROM <- gsub("Gabriee", "Gabrielle", FROM)
  FROM <- gsub("(^| )Aard", "\\1Allard", FROM)
  FROM <- gsub("McCoum", "McCollum", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Eison( |$)", "\\1Ellison\\2", FROM)
  FROM <- gsub("(^| )Weer( |$)", "\\1Weller\\2", FROM)
  FROM <- gsub("Rockefeer", "Rockefeller", FROM)
  FROM <- gsub("(^| )Suivan( |$)", "\\1Sullivan\\2", FROM)
  FROM <- gsub("(^| )Tiis( |$)", "\\1Tillis\\2", FROM)
  FROM <- gsub("(^| )McCaski( |$)", "\\1McCaskill\\2", FROM)
  FROM <- gsub("(^| )Espaiat( |$)", "\\1Espalliat\\2", FROM)
  FROM <- gsub("(^| )Costeo( |$)", "\\1Costello\\2", FROM)
  FROM <- gsub("(^| )Hi( |$)", "\\1Hill\\2", FROM)
  FROM <- gsub("(^| )Gaego( |$)", "\\1Gallego\\2", FROM)
  FROM <- gsub("(^| )McSay( |$)", "\\1McSally\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Muin( |$)", "\\1Mullin\\2", FROM)
  FROM <- gsub("(^| )Ciciin.( |$)", "\\1Cicilline\\2", FROM)
  FROM <- gsub("(^| )Sewe( |$)", "\\1Sewell\\2", FROM)
  FROM <- gsub("(^| )Heer( |$)", "\\1Heller\\2", FROM)
  FROM <- gsub("(^| )Rige( |$)", "\\1Rigell\\2", FROM)
  FROM <- gsub("(^| )Emers( |$)", "\\1Ellmers\\2", FROM)
  FROM <- gsub("(^| )Mier( |$)", "\\1Miller\\2", FROM)
  FROM <- gsub("(^| )Ha( |$)", "\\1Hall\\2", FROM)
  FROM <- gsub("(^| )McAISTER( |$)", "\\1McAllister\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Boswe( |$)", "\\1Boswell\\2", FROM)
  FROM <- gsub("(^| )Manzuo( |$)", "\\1Manzullo\\2", FROM)
  FROM <- gsub("(^| )Schiing( |$)", "\\1Shilling\\2", FROM)
  FROM <- gsub("(^| )Kisse( |$)", "\\1Kissell\\2", FROM)
  FROM <- gsub("(^| )Hoingsworth( |$)", "\\1Hollingsworth\\2", FROM)
  FROM <- gsub("(^| )Gaagher( |$)", "\\1Gallagher\\2", FROM)
  FROM <- gsub("(^| )Joy( |$)", "\\1Jolly\\2", FROM)
  FROM <- gsub("(^| )Raha( |$)", "\\1Rahall\\2", FROM)
  FROM <- gsub("(^| )Boswe( |$)", "\\1Boswell\\2", FROM)
  FROM <- gsub("(^| )Perrieo( |$)", "\\1Perriello\\2", FROM)
  FROM <- gsub("(^| )Fain( |$)", "\\1Fallin\\2", FROM)
  FROM <- gsub("(^| )Esworth( |$)", "\\1Ellsworth\\2", FROM)
  FROM <- gsub("(^| )Moohan( |$)", "\\1Mollohan\\2", FROM)
  FROM <- gsub("(^| )Fossea( |$)", "\\1Fossella\\2", FROM)
  FROM <- gsub("(^| )Miender-McDonald( |$)", "\\1Millender-McDonald\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Knoenberg( |$)", "\\1Knollenberg\\2", FROM)
  FROM <- gsub("(^| )Gimor( |$)", "\\1Gillmor\\2", FROM)
  FROM <- gsub("(^| )Jewe( |$)", "\\1Jewell\\2", FROM)
  # add 'll' to first names
  FROM <- gsub("(^| )oyd( |$)", "\\1Lloyd\\2", FROM)
  FROM <- gsub("(^| )Lucie( |$)", "\\1Lucille\\2", FROM)
  FROM <- gsub("(^| )Michee( |$)", "\\1Michelle\\2", FROM)
  FROM <- gsub("(^| )Bi( |$)", "\\1Bill\\2", FROM)
  FROM <- gsub("(^| )Biy( |$)", "\\1Billy\\2", FROM)
  FROM <- gsub("(^| )Coeen( |$)", "\\1Colleen\\2", FROM)
  FROM <- gsub("(^| )Cheie( |$)", "\\1Chellie\\2", FROM)
  FROM <- gsub("(^| )Sheey( |$)", "\\1Shelley\\2", FROM)
  FROM <- gsub("(^| )Darre( |$)", "\\1Darrell\\2", FROM)
  FROM <- gsub("(^| )Ayson( |$)", "\\1Allyson\\2", FROM)
  FROM <- gsub("(^| )Aen( |$)", "\\1JAllen\\2", FROM)
  FROM <- gsub("(^| )Say( |$)", "\\1Sally\\2", FROM)
  FROM <- gsub("(^| )Wi( |$)", "\\1Will\\2", FROM)
  FROM <- gsub("(^| )Way( |$)", "\\1Wally\\2", FROM)
  FROM <- gsub("(^| )Key( |$)", "\\1Kelly\\2", FROM)
  FROM <- gsub("([A-Z])(55)([A-Z])", "\\1SS\\2",FROM)
  FROM <- gsub("([A-Z])(5)([A-Z])", "\\1S\\2",FROM)
  FROM <- gsub("A1 ", "Al ", FROM)
  FROM <- gsub(" 1. ", " L. ", FROM)
  FROM <- gsub("Hany", "Harry", FROM) 
 
  
  
  # other errors
  #FIXED
  FROM <- gsub(".1.", "", FROM)
  FROM <- ifelse(grepl(" Cha", FROM)&grepl("((^| )Ja)|(J a.son)", FROM)&grepl('etz', FROM), gsub("J.*?n","Jason", FROM), FROM) #not sure if we should make this correction
  FROM <- ifelse(grepl(" Cha", FROM)&grepl("((^| )Ja)|(J a.son)", FROM)&grepl('etz', FROM), gsub("Chaf.*?z","Chaffetz", FROM), FROM) #also not sure if we should make this correction
  FROM <- ifelse(grepl("Tom", FROM)&grepl("Cobum|Co bum", FROM), gsub("Cobum|Co bum", "Coburn", FROM), FROM) #fixed
  FROM <- ifelse(grepl("DarrellIssa", FROM), 'Darrell Issa', FROM) #fixed
  FROM <- ifelse(grepl("Trent|Robin|Mike", FROM)&grepl("Key", FROM), gsub("(Trent|Robin|Mike) Key", "\\1 Kelly",FROM), FROM) #this goes in OCR errors/last name errors
  FROM <- ifelse(grepl("Comyn|Com yn|Cobum|Corvyn", FROM)&grepl("John", FROM), gsub("Comyn|Com yn|Corvyn","Cornyn", FROM), FROM) #fixed
  FROM <- ifelse(grepl("Jon", FROM)&grepl("(^| )Kyi( |$)", FROM), gsub("Kyi","Kyl", FROM), FROM) #fixed
  FROM <- ifelse(grepl("Diane", FROM)&grepl("(^| )Feinstein( |$|,)", FROM), gsub("Diane","Dianne", FROM), FROM) #fixed
  
 # FROM <- ifelse(grepl("Cliff", FROM)&grepl("Steams", FROM), gsub("Steams","Stearns", FROM), FROM) #fixed
  
 #FIXED
  
  FROM <- gsub("Cwnmings", 'Cummings', FROM) # fixed
  FROM <- gsub("Tnhofe", "Inhofe", FROM) # fixed
  FROM <- gsub("Ellrners","Ellmers", FROM) # fixed 
  FROM <- gsub("TONKA", "TONKO", FROM) #fixed
  FROM <- gsub("Mcarthur|Mccarthur", "MacArthur", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Coryn( |$|,)", "\\1Cornyn\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Connelly( |$|,)", "\\1Connolly\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Heitkmap( |$|,)", "\\1Heitkamp\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Micahel( |$)", "\\1Michael\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Farenhold( |$|,)", "\\1Farenthold\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Eschoo( |$|,)", "\\1Eshoo\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Lary( |$)", "\\1Larry\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("Christophers", "Christopher", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("Courntey", "Courtney", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Martrin( |$)", "\\1Martin\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Machin( |$|,)", "\\1Manchin\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Marry( |$|,)", "\\1Mary\\2", FROM, ignore.case = TRUE) # might not need this code because it is Harry not Marry
  FROM <- gsub("(^| )T MOTHY( |$|,)", "\\1Timothy\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )L NCOLN( |$|,)", "\\1Lincoln\\2", FROM, ignore.case = TRUE) #fixed 
  FROM <- gsub("(^| )Wydon( |$|,)", "\\1Wyden\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Klobachur( |$|,)", "\\1Klobuchar\\2", FROM, ignore.case = TRUE) #fixed
  
  
  
  return(FROM)
}

# *** Use this function with caution. Will add first name information based on ONLY last names that are unique, has 
#     potential to add a members first name to a non member creating false positives (e.g. If only last name 
#     Grassley is provided, it will assume it's Chuck Grassley, even if it was actually a 
#     random person named Jim Joe Grassley)
#
#
#  Typical use of function looks as follows:
# data$first_name <- addFirst(data$first_name,data$last_name)

#     *** last_name paramter must be in same all caps format of the members file
#     This function call may be necessary:   data$last_name <- formatLastName(data, 'last_name')

# Useful code for creating last_name when only last name provided:
#data %<>%
#  mutate(last_name = ifelse(grepl("^(\\w+)$",FROM), gsub("^(\\w+)$", '\\1',FROM),last_name))
addFirst <- function(first_name, last_name){
  
  twolastnames  <- members %>% group_by(last_name, congress) %>% tally() %>% filter(n>1) %>% select(-congress, -n) %>% distinct()
  membersOneLastName <- members[!(members$last_name %in% twolastnames$last_name),]

  i <- 1
  for(i in 1:length(membersOneLastName$id)){
    first_name = ifelse(last_name == membersOneLastName$last_name[i] & is.na(first_name), membersOneLastName$first_name[i],first_name)
    
  }
  return(first_name)
}

add_first <- function(data){
  
  twolastnames  <- members %>% group_by(last_name, congress) %>% tally() %>% filter(n>1)  %>% distinct()
  membersOneLastName <- members %>% select(last_name, first_name, congress) %>% anti_join(twolastnames)
  
  if(!"first_name" %in% names(data)){
    data %<>% left_join(membersOneLastName)
    return(data)
  } else{
    stop("add_first adds a missing first_name column. Rename the existing first_name column or use addFirst() to complete an existing but incomplete first_name columns")
  }
}

# same as addFirst, but with a better name, keeping the old for posterity, but should replace with this one and make it congress-specific
complete_first <- function(first_name, last_name){
  
  twolastnames  <- members %>% group_by(last_name, congress) %>% tally() %>% filter(n>1) %>% select(-congress, -n) %>% distinct()
  membersOneLastName <- members[!(members$last_name %in% twolastnames$last_name),]
  
  i <- 1
  for(i in 1:length(membersOneLastName$id)){
    first_name = ifelse(last_name == membersOneLastName$last_name[i] & is.na(first_name), membersOneLastName$first_name[i],first_name)
    
  }
  return(first_name)
}

complete_chamber <- function(chamber, last_name){
  
  twolastnames  <- members %>% group_by(last_name, congress) %>% tally() %>% filter(n>1) %>% select(-congress, -n) %>% distinct()
  membersOneLastName <- members[!(members$last_name %in% twolastnames$last_name),]
  
  i <- 1
  for(i in 1:length(membersOneLastName$id)){
    chamber = ifelse(last_name == membersOneLastName$last_name[i] & is.na(chamber), membersOneLastName$chamber[i],chamber)
    
  }
  return(chamber)
}



#########################


# A helper function to return the full regex pattern string (so that we can join on pattern) where it finds a match
str_detect_replace <- function(string_to_search, pattern){
  out <- ifelse(str_detect(string_to_search, pattern), pattern, "404error")
  out %<>% str_squish()
  return(out)
}


findTypos <- function(string){
  purrr::map(.x = typos$typos, 
             .f= str_detect_replace,
             string_to_search = string) %>% 
    unlist() %>%
    unique() %>% 
    # seperate pattrns found with OR 
    #str_c(collapse = "|") %>%
    # remove 404error when it appears along side a found pattern
    str_remove("\\|404error|404error\\|") %>% 
    str_replace(" ,", ", ") %>% 
    str_squish()
    
}






  
  ######################################################################################
# This function will replace extractMemberNames after it has been tested and vetted
# It does not use format first and last name columns. 
# It does correct ocr.errors and then corrects typos using the typos tables
# It then uses the pattern variable in the members data to match names 
# FIXME need to reverse members and col name in ALL SCRIPTS to make this tidy 

## FIRST A FEW HELPER FUNCTIONS: 

# A function to map over members 
# (assumes that memmbers object contains congress and pattern)
# (assumes that data contains congress and string)
extractName <- function(string, data, members){
  pattern <- purrr::map(.x = members %>% select(pattern), 
             .f = str_detect_replace,
             string_to_search = string) %>% 
    unlist() %>%
    unique() %>% 
    str_c(collapse = ";") %>%
    str_remove(";404error|404error;")
  
  return(pattern)
}

# A function to map over congresses 
# one congress at a time
extractNamesPerCongress <- function(congress_i, data, members = members){
  
  # subset to one congress
  data %<>% filter(congress == congress_i)
  
  # search only if that congress exists in data
  # FIXME with purrr error handeling?
  
  # if congress not in members file 
  if(!congress_i %in% members$congress & "DATE" %in% names(data)){
    
    top5 <- nrow(data)
    if(top5>5){top5<-5}
    
    ## Message for when errors are probably non-observations (short strings or NA)
    nonobs <- "but empty string, so probably non-observations"
    
    base::message(red(paste(
      paste0("Bad dates in ", unique(data$agency), ", ", congress_i, "th congress?"),
      paste(data %>% 
              mutate(string = ifelse(
                nchar(string <3) |
                  string %in% c("na", "na na", "(b)(6)", "") |
                  is.na(string) |
                  !is.na(ERROR),
                nonobs,
                string) ) %>% 
              # group_by(LetterID) %>% 
              # mutate(LetterID = ifelse(string == "but probably non-observations",
              #                          LetterID == paste(top_n(1, LetterID), "-", top_n(1, desc(LetterID)) ),
              #                          LetterID) ) %>% 
              # ungroup() %>% 
              group_by(string) %>% 
              mutate(DATE = paste0(unique(DATE), collapse = ", "),
                     row = paste0(unique(LetterID), collapse = ";") %>% str_trunc(4+(10*7)+3) ) %>% # "row [first 10 row numbers]..."
              count(DATE, row, string, wt = NULL) %>% 
              arrange(row) %>% 
              arrange(-n) %>% 
              ungroup() %>% 
              transmute(strings = paste0("row ", row, ", DATE = ", DATE, " \"", string, "\"")) %>%
              .$strings,
            collapse = "\n"), 
      sep = "\n")))
    
    data %<>% 
      mutate(pattern = "Date out of range", 
             first_name = NA,
             last_name = NA)
  }
  
  # if congress in members file
  if(congress_i %in% members$congress){
  members %<>% filter(congress == congress_i)
  
  base::message( green(str_c("Searching ", unique(data$agency), " data for members of the ", congress_i, "th, n = ", 
                             nrow(data), " (", length(unique(data$string)), " distinct strings)."#,
                             # broken by dplyr 1.0.0, reverted in and works with 1.0.1
         #" Most common string: \"", count(data, string) %>% top_n(1, n) %>% .[1,1], "\""
         )
         ))
  #count(data, string) 
  #count(data, string) %>% top_n(1, n)
  # match patterns from the members data and merge with member names
  data %<>%
    ungroup() %>%
    # map function to detect members over lower case version of FROM 
    mutate(pattern = string %>% purrr::map_chr(extractName,
                                               data = data,
                                               members = members) ) %>% # select(from,matches)
    # split out multiple members into separate rows 
    mutate(pattern = str_split(pattern, ";")  ) %>% 
    unnest() 
  
  suppressMessages(
  data %<>% 
    # join in members data by pattern 
    left_join(members %>% select(pattern, first_name, last_name, congress) ) %>% #, by = c("pattern", "congress")) %>% 
    mutate(first_name = as.character(first_name),
           last_name = as.character(last_name))
  )
  }
  return(data)
}



extractMemberName <- function(data, members = members, col_name, congresses = unique(data$congress)){
  
  # provided col name is string to format and extract names from
  data %<>% mutate(string = data[[col_name]])
  # FOR TESTING 
  # col_name <- "FROM"
  
  if("chamber" %in% names(data)){
    # add chamber to string if not NA 
    data %<>% mutate(string = ifelse(!is.na(chamber), 
                                   paste(chamber, string) %>% 
                                     str_replace("House", "Represenative") %>% 
                                     str_replace("Senate", "Senator"), 
                                   string))
    
    # drop chamber 
    data %<>% select(-chamber)
  }
  
  if("state" %in% names(data)){
    # FIXME add state_abbrev to string if not NA 
    data %<>% mutate(string = ifelse(!is.na(state),
                                     paste(string, "-", stateFromFull(state)),
                                     string))
    
    # drop state
    data %<>% select(-state)
  }

  t <- Sys.time()

  # Add Letter ID if missing 
  if(!"LetterID" %in% names(data)){data$LetterID <- 1:nrow(data)}
  
  data$LetterID %<>% 
    str_squish() %>% 
    as.numeric()
  
  data$ID <- 1:nrow(data)
  
  data %<>% 
    mutate(LetterID = coalesce(LetterID, ID) %>% # replace missing with row number 
             as.numeric() + 1) # add one to make letter id the same as sheet id
 
  data$LetterID %<>% 
    formatC(width=6, flag="0", format = "fg")
  
  # Make missing congress explicit 0 so that it will not be dropped 
  data$congress %<>% replace_na(0) %>% as.numeric() %>% replace_na(0) %>% as.numeric()

  
  # joining with members requires these variables are not there
  data %<>% mutate(last_name = NA,
                   first_name = NA, 
                   pattern = NA) %>% 
    select(-first_name, -last_name, -pattern)
    
    # clean up text
    data$string %<>% cleanFROMcolumn() 

     # correct common OCR errors
    data$string %<>% ocr.errors()
    
    # lower case 
    data$string %<>% tolower() %>% 
      str_replace("senator senator", "senator") %>% 
      str_replace("represenative representative", "representative")

    # na's pasted in 
    data$string %<>% str_replace("na politano", "napolitano")
    data$string %<>% str_remove("^na ")
    data$string %<>% str_remove("^na ")
    data$string %<>% str_replace("han na\\b", "hanna")
    data$string %<>% str_remove_all("\\bna\\b")
    
    data$string %<>% str_squish()
    
    # misplaced commas
    data$string %<>% str_replace(" ,", ", ")
    
    data$string %<>% str_squish()
    
    # explicit NA
    data$string %<>% replace_na("")
    
    
    
    # correct typos 
    #FIXME with more purrr
    for (i in 1:dim(typos)[1]){
    r <- typos$correct[i]
    p <- typos$typos[i]
  
    # Fix name typos
    data %<>% 
      # find common typos
      mutate(string = string %>% purrr::map_chr(str_replace, 
                                                   pattern = p, 
                                                   replacement = r %>% paste("")))
    }
    
    #FIXME problem created by correcting typos
    data$string %<>% str_replace(" ,", ", ") %>% str_squish()

    base::message(paste("Typos fixed in", round(Sys.time()-t), "seconds"))
    t <- Sys.time()
    
    # loop over congresses in data 
    data <- map_dfr(congresses, extractNamesPerCongress, data = data, members = members) #FIXME members default provided?

    base::message(paste("Names matched in", round(Sys.time()-t), "seconds"))
    
    data %<>% distinct()
    
    # New ID since function may split out multiple members if found
    data$ID <- 1:nrow(data) %>% formatC(width=6, flag="0")
    
    # trying this out adding chamber and state from member data becasuse scripts use them post extractmembername sometimes, 
    # should not increase n because pattern is already unique to icpsr in a chamber, right?
    data %<>% left_join(members %>% select(icpsr, pattern, bioname, first_name, last_name, congress, chamber, state) %>% distinct() ) %>% 
      distinct()
    
    data$icpsr %<>% as.numeric()
    
    data %<>% select(LetterID, ID, congress, string, pattern, chamber, everything())
    
    return(data)
}



