library(tidyverse)
library(reshape2)
library(data.table)

######################################
# Read and prep validation dataset
######################################


# Read in data; filter out test entries
df <- read.csv('input/Validation Survey_January 21, 2020_09.10.csv', header = T, skip = 1)
df <- df[c(2:nrow(df)),]
df <- df %>% filter(Response.Type != 'Survey Preview')

# Clean up dataframe
df <- df[,18:ncol(df)]
df <- df[, colnames(df)[!grepl('text|sentiment|topic', colnames(df), ignore.case = T)]]
cnames <- c('enumerator', 'studyID', 'title', 'inclusion', 
            'scale', 'region', 'country', 'crop_livestock', 'Type of study', 
            'Interventions', 'Outcomes', 'fullDesign', 
            'fullDesign_text', 'Environmental impacts', 'Gender')

colnames(df) <- cnames

df$`Type of study` <- str_replace(df$`Type of study`, 'al, prim', 'al prim')
df$studyID <- as.integer(as.character(df$studyID))

# Filter studies that met inclusion criteria
df <- df %>% filter(inclusion == 'Yes')

# Split variables with multiple entries and combine with dataset
# TODO: Potentially split country and region if multiple entered
intervention <- str_split(df$Interventions, ',', simplify = T)
colnames(intervention) <- paste0('Interventions_', 1:ncol(intervention))

outcome <- str_split(df$Outcomes, ',', simplify = T)
colnames(outcome) <- paste0('Outcomes_', 1:ncol(outcome))

environmental <- str_split(df$`Environmental impacts`, ',', simplify = T)
colnames(environmental) <- paste0('Environmental impacts_', 1:ncol(environmental))

studyType <- str_split(df$`Type of study`, ',', simplify = T)
colnames(studyType) <- paste0('Type of study_', 1:ncol(studyType))

crop_livestock <- str_split(df$crop_livestock, ',', simplify = T)
colnames(crop_livestock) <- paste0('crop livestock_', 1:ncol(crop_livestock))

gender <- str_split(df$Gender, ',', simplify = T)
colnames(gender) <- paste0('Gender_', 1:ncol(gender))


df <- cbind(df, intervention, outcome, environmental, studyType, crop_livestock, gender)
df <- melt(df, id.vars = cnames, 
           variable.name = 'keyword_category',
           value.name = 'keyword_subcategory')

df <- df %>% filter(keyword_subcategory != '')
df$keyword_category <- str_extract(df$keyword_category, '^[a-zA-Z ]*')
df$keyword_category <- ifelse(df$keyword_category == 'crop livestock', 
                              'crop_livestock',  df$keyword_category)

df$keyword_category <- str_to_lower(df$keyword_category)
df$keyword_subcategory <- str_to_lower(df$keyword_subcategory)

df <- df %>%
  select(studyID, keyword_category, keyword_subcategory)

######################################
# Match validation to automated dataset
######################################

# Read in automation data
df_auto <- fread('output/20191217_abstracts_results.csv')
df_auto$search_results <- df_auto$search_results_abstract + df_auto$search_results_title
df_auto$studyID <- df_auto$abstract_index

# df_auto <- fread('output/20191217_fulltext_results.csv')
# df_auto$studyID <- df_auto$pdf_index

# Clean up data
df_auto$keyword_category <- ifelse(df_auto$keyword_category %in% c(
  'crop yield', 'yield (livestock)', 'yield (livestock products)'),
  'yield', df_auto$keyword_category)

df_auto$keyword_category <- str_to_lower(df_auto$keyword_category)
df_auto$keyword_subcategory <- str_to_lower(df_auto$keyword_subcategory)

# Subset to only validation data
df_auto <- df_auto %>% 
  filter(studyID %in% unique(df$studyID))

# create a crop_livestock variable from crop and livestock lists
cl <- df_auto %>% 
  filter(keyword_category == 'crop_livestock') %>% 
  group_by(studyID, keyword_subcategory) %>% 
  summarise(tot = sum(search_results, na.rm = T))
cl <- dcast(cl, studyID ~ keyword_subcategory)
cl$keyword_subcategory <- ifelse(cl$crop > 0 & cl$livestock > 0, 'mixed',
                            ifelse(cl$crop > 0 & cl$livestock == 0, 'crop',
                                   ifelse(cl$crop == 0 & cl$livestock > 0, 'livestock', NA)))
cl <- cl[, c('studyID', 'keyword_subcategory')]
cl$keyword_category <- 'crop_livestock'
cl$search_results <- 10
cl <- na.omit(cl)

# Merge crop_livestock variable into df_auto, 
# first filter out original crop/livestock keywords
df_auto <- df_auto %>% 
  filter(keyword_category != 'crop_livestock') %>%
  select(studyID, keyword_category, keyword_subcategory, search_results)

df_auto <- rbind(df_auto, cl)

# Ensure that all subterm serach results are summed
df_auto <- df_auto %>%
  group_by(studyID, keyword_category, keyword_subcategory) %>%
  summarise(search_results = sum(search_results, na.rm = T))


df <- df %>% filter(keyword_category != 'gender' | keyword_subcategory != 'no')
df$validation_results <- 1
df <- merge(df, df_auto, by = c('studyID', 'keyword_category', 'keyword_subcategory'), all = T)
df$validation_results <- ifelse(!is.na(df$validation_results), 
                                df$validation_results, 0)


threshold = 1
df$matches <- ifelse(df$validation_results > 0 & df$search_results > threshold, 'Correct', 
                     ifelse(df$validation_results > 0 & df$search_results == 0, 'False Negatives', 
                            ifelse(df$validation_results == 0 & df$search_results > threshold, 'False Positives', NA)))

round(100 * xtabs(~ matches, df) / sum(xtabs(~ matches, df)), 2)

mt <- xtabs( ~ keyword_category + matches, df, )
100 * round(prop.table(mt, 1), 2)



