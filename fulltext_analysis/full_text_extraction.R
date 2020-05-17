# Timing code runtime
start_time <- Sys.time()

# 20191025: code to search just titles and abstracts
# 20191217: updated to include full text search
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(pdftools)  # install.packages("pdftools")
library(tm)        # install.packages("tm")
# library(tabulizer)  # install.packages("tabulizer")
Sys.setenv(JAVA_HOME="")
library(rJava)  # setx PATH "C:\Program Files\Java\jre1.8.0_211\bin\server;%PATH%"; Sys.setenv(JAVA_HOME="") #in r studio
library(NLP)
library(openNLP)
options(java.parameters = "- Xmx1024m")
############################################################################
###
# SETUP THE PROJECT
###
# this will be used for cleaning the data whenever two elements joined, or special characters removed
replacement.char   <- " "
# function to clean string vector
# there are multiple forms of hyphens, so it homogenizes that
# it removes any instances of multiple spaces
# it converts all text to lower caps
f.clean.text <- function(input.vector) {
  cleaned.vector <- input.vector %>%
    gsub(replacement = replacement.char, x = ., pattern = "-|−") %>%
    # following line commented out because I think we should not replace periods and other punctuation
    # gsub(replacement = replacement.char, x = ., pattern = "[[:punct:]]") %>%
    gsub(replacement = replacement.char, x = ., pattern = "\\s+") %>% # one or more spaces replaced
    tolower(.)
  return(cleaned.vector)
}
############################################################################
###
# UPDATE ADDRESSES OF ALL FILES AND KEYWORDS
###
working.dir <- "C:/Users/wb551716/OneDrive - WBG/Projects/Personal_projects/Ceres2030/"
# download the following files and into the working directory
# 1. final look up tables               "look_up_tables_final.xlsx"      downloaded from https://docs.google.com/spreadsheets/d/1EZMyMyMwlKm8Eba7YzYsgdHOLXBMKtP3zwUsAXtCj8A/edit#gid=1174818988
# 2. crop livestock look up tables      "crop_livestock_lookup.xlsx"     downloaded from https://docs.google.com/spreadsheets/d/1Wfgm8_2T7ZNNbBzWmDLRUQKWtSk9Z7tKp9PkW1JSoHw/edit#gid=0
setwd(working.dir)
address.keywords       <- "input/look_up_tables_final.xlsx"    # file containing look up terms
address.crop.livestock <- "input/crop_livestock_lookup.xlsx"   # file containing crop livestock terms
# also, following items are available in the Box folder
# titles and abstracts      "Ceres_team6_full_text_info_tracking_20191212.xlsx"
address.abstracts      <- "input/Ceres2030-Team 6/Ceres_team6_full_text_info_tracking_20191212.xlsx"
# address.pdf is the address of working directory where all full text PDFs are stored
address.pdf            <- "input/Ceres2030-Team 6/PDF/"
############################################################################
###
# CREATE A CLEANED KEYWORD LIST
###
# extract names of sheets in keywords excel file:
names.sheets <- readxl::excel_sheets(address.keywords)
# concatenate all keyword sheets into one dataframe
df.keywords <- data.frame()
for ( i in names.sheets){
  temp <- readxl::read_excel(address.keywords,
                             range = NULL, col_names = TRUE, col_types = NULL, 
                             na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                             guess_max = 25, sheet = i) %>% as.data.frame()
  temp.long <- temp %>% gather(., key = "keyword_subcategory", value = "keyword")
  temp.long <- cbind(keyword_category = i, temp.long)
  df.keywords <- rbind(df.keywords, temp.long)
  rm(temp, temp.long)
}; rm(i, names.sheets)
# add crop livestock list
temp <- readxl::read_excel(address.crop.livestock,
                           range = NULL, col_names = TRUE, col_types = NULL, 
                           na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                           guess_max = 25, sheet = 1) %>% as.data.frame()
names(temp)
temp <- data.frame(keyword_category = "crop_livestock", 
                   keyword_subcategory = temp$type, 
                   keyword = temp$Item)
df.keywords <- rbind(df.keywords, temp)
rm(temp)
df.keywords <- df.keywords %>% dplyr::filter(!is.na(keyword)) %>% unique(.)
df.keywords$keyword <- f.clean.text(df.keywords$keyword) %>% trimws(.)
str(df.keywords)
df.keywords[,] <- apply(df.keywords[,], 2, function(x) as.character(x))
df.keywords <- cbind(keyword_ID = 1:nrow(df.keywords), df.keywords)
# create new column with regex compatible keywords
df.keywords$regex_keyword <- gsub("\\*", "\\\\w*", df.keywords$keyword)
df.keywords$regex_keyword <- gsub("\\$", "\\\\w", df.keywords$regex_keyword)
df.keywords$regex_keyword <- paste0("\\b", df.keywords$regex_keyword, "\\b")
############################################################################
###
# SEARCH ACROSS ONLY TITLES AND ABSTRACTS (full text search performed in a later section)
###
# note: I am reading in all abstracts (irrespective of whether the associated fulltext PDF has been uploaded or not)
#       if you want to search abstracts/titles of only uploaded PDFs, change sheet parameter to 1 in the following command
df.abstracts <- readxl::read_excel(address.abstracts,
                                   range = NULL, col_names = TRUE, col_types = NULL, 
                                   na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                                   guess_max = 25, sheet = 4) %>% as.data.frame()
names(df.abstracts)
# keep only ID, title, abstract:
df.abstracts <- df.abstracts[,c("id", "title", "abstract")]
str(df.abstracts)
df.abstracts$title <- f.clean.text(df.abstracts$title)
df.abstracts$abstract_raw <- df.abstracts$abstract  # keeping a raw version for the location extractor
df.abstracts$abstract <- f.clean.text(df.abstracts$abstract)
names(df.keywords)
# create list, each element of which is a keyword-search dataframe for a particular paper
list.result.abstracts <- vector(mode = "list", length = nrow(df.abstracts))
names(list.result.abstracts) <- df.abstracts$id

##### set up the location extractor #####
# create annotators for words and sentences
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
# creates annotators of kind person, location and organization
person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
# holds annotators in the order to be applied
pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
# Extract entities from an AnnotatedPlainTextDocument
entities <- function(text, kind) {
  s <- text$content
  a <- annotation(text)
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}
# read text
extract_entity <- function(text) {
  #convert the character vectors into one character vector
  text <- paste(text, collapse = " ")
  #converts bio variable into a string
  text<- as.String(text)
  #Identifies where the sentences are and the words
  text_annotations <- NLP::annotate(text, list(sent_ann, word_ann))
  #combines bio and the annotations
  text_doc <- AnnotatedPlainTextDocument(text, text_annotations)
  text_annotations <- NLP::annotate(text, pipeline)
  text_doc <- AnnotatedPlainTextDocument(text, text_annotations)
  return(entities(text_doc, kind = 'location'))
}
##### location extractor set up complete #####

# loop over each paper to search the title and abstract
for (i in 1:length(list.result.abstracts)) {
  temp.results <- df.keywords
  temp.results$abstract_index <- df.abstracts$id[i]
  temp.results$search_results_title <- str_count(pattern = temp.results$regex_keyword, string = df.abstracts$title[i])
  temp.results$search_results_abstract <- str_count(pattern = temp.results$regex_keyword, string = df.abstracts$abstract[i])
  t <- extract_entity(df.abstracts$abstract_raw[i])
  temp.results$location <- paste(t, collapse = '; ')
  list.result.abstracts[[i]] <- temp.results
  rm(temp.results)
}; rm(i)
# bind the list of search results into one dataframe (long format)
df.result.abstracts <- bind_rows(list.result.abstracts, .id = "list_index") %>% as.data.frame()
rm(list.result.abstracts)
str(df.result.abstracts)
df.result.abstracts$list_index <- NULL
# write.csv(df.result.abstracts, file = "/Users/Sidhu/Google Drive/1a/1_research/projects/ceres2030/20191217_abstracts_results.csv",row.names = F)
############################################################################
###
# READ IN ALL FULLTEXT PDFs
###
setwd(address.pdf)
pdf.list <- list.files(pattern = '.pdf')
# this string will hold all texts scraped from the PDFs (each element being one pdf)
pdf.strings <- rep(NA, length(pdf.list))
pdf.strings.raw <- rep(NA, length(pdf.list))
pdf.failed  <- list()
# name the PDFs using their id
names(pdf.strings) <- substr(pdf.list, start = 1, stop = 5)
for (i in 1:length(pdf.strings)) {
  print(names(pdf.strings)[i])
  tryCatch(
  {
      # pdftools::pdf_text is bad with multi-column PDFs, which is the case with most of our PDFs
      # pdftools::pdf_data may work better
      text <- pdftools::pdf_data(pdf.list[i])
      # pdf_data makes a list, with each element corresponding to a dataframe/tibble for each page 
      # bind all dataframes to create one mega dataframe
      # each row of this dataframe corresponds to a word on the page
      df.text <- bind_rows(text, .id = "page") %>% as.data.frame()
      # remove all rows beyond the last instance of "reference/s"
      # what if there are no "reference/s" in the file?
      # Then the search will just pick up the last occurence of "reference/s" , potential for error
      last.reference <- c(grep(pattern = "\\breference", x = df.text$text, ignore.case = T))
      if (length(last.reference) != 0) {
        # pick up the last element of "last.reference"
        last.reference <- last.reference[length(last.reference)]
        # remove everything beyond the last occurence of "reference/s"
        df.text <- df.text[1:(last.reference-1),]
      }
      rm(last.reference)
      # concatenate the last column of the dataframe, and clean it up for analysis
      concat.text <- df.text$text %>%
        paste0(., collapse = replacement.char) %>%
        f.clean.text(.)
      pdf.strings[i] <- concat.text
      
      concat.text <- df.text$text %>%  # keep raw text for location extraction later
        paste0(., collapse = replacement.char)
      pdf.strings.raw[i] <- concat.text
      
      rm(text, df.text, concat.text)
  },
  error = function(cond) {
    message(paste('failed attempt: ', names(pdf.strings)[i]))
    pdf.failed[i] <- names(pdf.strings)[i]
  }
  )
} # end "i" loop: input pdf files

rm(i, pdf.list)
object.size(pdf.strings)
sum(is.na(pdf.strings))
# View(nchar(pdf.strings))
names(pdf.strings[(nchar(pdf.strings) == 0)])
sum(nchar(pdf.strings) < 2000)
plot(sort(nchar(pdf.strings)), ylim = c(0,50000))
pdf.character.count <- nchar(pdf.strings)
write.csv(data.frame(ID = names(pdf.character.count), character_count = pdf.character.count),
          file = "../../../output/20191219_pdf_character_counts.csv",
          row.names = FALSE)
############################################################################
### troubleshooting
# # some files give errors.
# this code was used by Balsher to see what was causing the error
# manual inspection revealed nothing serious.
# so this chunk has been commented out
# # one of these two errors:
# # # 1: "PDF error: Invalid Font Weight"
# # # 2: "PDF error: Invalid shared object hint table offset"
# problematic.pdf <- c(10033, 10250, 10434, 10482, 10610, 10632, 10731,
#                       10820, 10834, 11053, 11144, 11373, 11391)
# problematic.pdf.error <- c(1,2,2,2,1,1,2,
#                            2,2,2,2,2,2)
# problematic.pdf.index <- which(names(pdfs) %in% problematic.pdf)
# # 17 119 194 215 261 271 313 358 367 466 510 601 610
# rm(problematic.pdf)
# problematic.text <- pdftools::pdf_data(pdf.list[problematic.pdf.index[1]])
# problematic.df.text <- bind_rows(problematic.text, .id = "page") %>% as.data.frame()
# problematic.concat.text <- problematic.df.text$text %>%
#   paste0(., collapse = replacement.char) %>%
#   f.clean.text(.)
# rm(list = ls()[grep("problematic", ls())])
############################################################################
###
# SEARCH ACROSS FULL TEXT
###
# this code section follows similar logic as the title-abstract from the code chunk two sections back
names(df.keywords)
list.result.fulltext <- vector(mode = "list", length = length(pdf.strings))
names(list.result.fulltext) <- names(pdf.strings)
for (i in 1:length(list.result.fulltext)) {
  tryCatch(
    {
      temp.results <- df.keywords
      temp.results$pdf_index <- names(pdf.strings)[i]
      temp.results$search_results <- str_count(pattern = temp.results$regex_keyword, string = pdf.strings[i])
      t <- extract_entity(pdf.strings.raw[i])
      temp.results$location <- paste(t, collapse = '; ')
      list.result.fulltext[[i]] <- temp.results
      rm(temp.results)
    },
    error = function(cond) {
      message(paste('failed attempt: ', names(pdf.strings)[i]))
      pdf.failed[i] <- names(pdf.strings)[i]
    }
  )
}; rm(i)
df.result.fulltext <- bind_rows(list.result.fulltext, .id = "list_index") %>% as.data.frame()
str(df.result.fulltext)
rm(list.result.fulltext)
df.result.fulltext$list_index <- NULL
df.result.fulltext %>% na.omit() %>% summarise(n_distinct(pdf_index))
write.csv(df.result.fulltext, file = "../../../output/20191219_fulltext_results.csv",row.names = F)
############################################################################
############################################################################
############################################################################


# Timing code runtime
end_time <- Sys.time()
print(end_time - start_time)





#######################################
# Code check - Vinny
detach("package:reshape2", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
library(reshape2)
library(ggplot2)

# df.result.fulltext <- read.csv('../../../output/20191219_fulltext_results.csv')
df.result.fulltext %>% na.omit() %>% summarise(n_distinct(pdf_index))

pot_fails <- read.csv("../../../output/20191219_pdf_character_counts.csv")
pot_fails %>% filter(character_count < 10000) %>% nrow


# Check how well the look up tables worked (seems like abstracts are better than full text)
df.result.abstracts %>% 
  filter(search_results_abstract > 0) %>%
  group_by(keyword_subcategory) %>% 
  summarise(value = n_distinct(abstract_index)) %>%
  View

df.result.abstracts$pdf_index <- df.result.abstracts$abstract_index
df.result.abstracts$search_results <- df.result.abstracts$search_results_abstract

dat <- df
dat$pdf_index <- dat$studyID
dat$studyID <- NULL

# Matrix with study type
dd <- dat %>% 
  filter(search_results > 0) %>%
  filter(keyword_category %in% c('outcomes', 'interventions', 'type of study')) %>%
  mutate(d = ifelse(search_results > 0, 1, NA),
         keyword_subcategory = as.character(keyword_subcategory),
         keyword_subcategory = ifelse(grepl('yield', keyword_subcategory), 
                                      'yield', 
                                      keyword_subcategory)) %>%
  select(keyword_category, keyword_subcategory, pdf_index, d) %>%
  na.omit()

tmp1 <- dd %>% filter(keyword_category %in% c('outcomes')) %>% select(keyword_subcategory, pdf_index)
tmp2 <- dd %>% filter(keyword_category %in% c('interventions')) %>% select(keyword_subcategory, pdf_index)
tmp3 <- dd %>% filter(keyword_category %in% c('type of study')) %>% select(keyword_subcategory, pdf_index)

colnames(tmp1)[1] <- c('outcomes')
colnames(tmp2)[1] <- c('interventions')
colnames(tmp3)[1] <- c('studytype')

tmp <- unique(merge(tmp1, tmp2, by = 'pdf_index', all = T))
tmp <- unique(merge(tmp, tmp3, by = 'pdf_index', all = T))
tmp <- dcast(tmp, outcomes + studytype ~ interventions)
tmp <- melt(tmp, id.vars = c('outcomes', 'studytype'))
tmp <- tmp %>% filter(variable != 'NA') %>% na.omit()

tmp_aggregate <- tmp %>% 
  group_by(outcomes, variable) %>% 
  summarise(value = sum(value))

plotly::ggplotly(
ggplot(tmp, aes(outcomes, variable)) +
  geom_tile(aes(fill = value),
            data = tmp_aggregate,
            color = 'grey80') +
  geom_point(aes(size = value, color = studytype),
             shape = 'square',
             position = position_dodge(width = .9)) +
  theme_minimal()
)

# Crop versus livestock systems
setwd(working.dir)
metadata <- readxl::read_excel(address.abstracts,
                               range = NULL, col_names = TRUE, col_types = NULL, 
                               na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                               guess_max = 25, sheet = 4) %>% as.data.frame()
metadata <- metadata[, c('id', 'year')]
colnames(metadata)[1] <- 'pdf_index'

dd <- dat %>% 
  filter(search_results > 0) %>%
  filter(keyword_category %in% c('crop_livestock')) %>%
  select(keyword_subcategory, pdf_index) %>%
  na.omit()
dd <- unique(dd)

tmp1 <- dd %>% filter(keyword_subcategory == 'crop')
tmp2 <- dd %>% filter(keyword_subcategory == 'livestock')
colnames(tmp1)[1] <- c('crop')
colnames(tmp2)[1] <- c('livestock')
tmp <- unique(merge(tmp1, tmp2, by = 'pdf_index', all = T))
tmp$system <- paste0(tmp$crop, tmp$livestock)
tmp$system <- ifelse(tmp$system == 'cropNA', 'crop',
              ifelse(tmp$system == 'NAlivestock', 'livestock',
              ifelse(tmp$system == 'croplivestock', 'mixed', NA)))
tmp <- merge(tmp, metadata, by = 'pdf_index', all.x = T, all.y = F)
  
tmp %>%
  filter(year < 2019) %>%
  group_by(year, system) %>%
  summarise(n = n_distinct(pdf_index)) %>% 
  ggplot(aes(year, n, color = system)) +
  geom_line(size = 1) +
  theme_minimal()
  

# Gender and enviro dimensions
setwd(working.dir)
metadata <- readxl::read_excel(address.abstracts,
                               range = NULL, col_names = TRUE, col_types = NULL, 
                               na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
                               guess_max = 25, sheet = 4) %>% as.data.frame()
metadata <- metadata[, c('id', 'year')]
colnames(metadata)[1] <- 'pdf_index'

dd <- dat %>% 
  filter(search_results > 0) %>%
  filter(keyword_category %in% c('gender', 'environmental impacts')) %>%
  select(keyword_category, keyword_subcategory, pdf_index) %>%
  na.omit()
dd <- unique(dd)

tmp <- merge(dd, metadata, by = 'pdf_index', all.x = T, all.y = F)

tmp %>%
  filter(year < 2019) %>%
  group_by(year) %>%
  mutate(n_tot = n_distinct(pdf_index)) %>%
  group_by(year, keyword_category) %>% 
  summarise(n = n_distinct(pdf_index),
            n_tot = max(n_tot)) %>%
  ggplot(aes(year, n)) +
  geom_line(size = 1) +
  theme_minimal() +
  facet_wrap(~ keyword_category)


tmp %>%
  filter(year < 2019 &
         keyword_category == 'environmental impacts') %>%
  group_by(year) %>%
  mutate(n_tot = n_distinct(pdf_index)) %>%
  group_by(year, keyword_category, keyword_subcategory) %>%
  summarise(n = n_distinct(pdf_index),
            n_tot = max(n_tot)) %>%
  ggplot() +
  geom_line(aes(year, n), size = 1, color = 'red') +
  geom_line(aes(year, n_tot), size = 1, coor = 'black') +
  theme_minimal() +
  facet_wrap(~ keyword_subcategory)



