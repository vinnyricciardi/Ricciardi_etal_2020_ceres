library(tidyverse)
library(reshape2)
library(data.table)
library(zoo)
library(rgdal)
library(ggthemes)
library(gridExtra)
library(broom)
# library(ggthemr)  #devtools::install_github('cttobin/ggthemr')
library(grid)
library(cowplot)
# library(ggpubr)
library(scales)
library(readxl)
library(showtext)
library(waffle)



######################################
# Define ggplot theme
######################################
base_size = 14
theme_set(theme_tufte(base_family = 'Arial', base_size = base_size))
tg <- theme_get()

ggplot <- function(...) ggplot2::ggplot(...) + 
  scale_color_tableau() +
  scale_fill_tableau()


######################################
# Tmp helper function for plot viewing 
# in actual dimensions
######################################

viewplot <- function(width, height, dpi = 72) {
  ggsave('C:/Users/wb551716/OneDrive - WBG/Documents/R_tmpPlotviewer/tmp.png', 
         width = width,
         height = height, 
         dpi = dpi)
}


######################################
# Read and prep dataset
######################################


# Read in data; filter out test entries
df <- read.csv('input/Validation Survey_February 17, 2020_12.13.csv', header = T, skip = 1)
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

df$`Type of study` <- str_replace(df$`Type of study`, 'Observational, primary data', 'Observational primary data')
df$`Type of study` <- str_replace(df$`Type of study`, 'Experimental, primary data', 'Experimental primary data')
df$studyID <- as.integer(as.character(df$studyID))

# Filter studies that met inclusion criteria
df <- df %>% filter(inclusion == 'Yes')

# Set aside a distinct dataframe for all studies included
df_include <- df %>% select(studyID)

# Split variables with multiple entries and combine with dataset
intervention <- str_split(df$Interventions, ',', simplify = T)
colnames(intervention) <- paste0('Interventions_', 1:ncol(intervention))

outcome <- str_split(df$Outcomes, ',', simplify = T)
colnames(outcome) <- paste0('Outcomes_', 1:ncol(outcome))

environmental <- str_split(df$`Environmental impacts`, ',', simplify = T)
environmental[,1][environmental[,1] == ''] <- 'No'
colnames(environmental) <- paste0('Environmental impacts_', 1:ncol(environmental))

studyType <- str_split(df$`Type of study`, ',', simplify = T)
colnames(studyType) <- paste0('Type of study_', 1:ncol(studyType))

crop_livestock <- str_split(df$crop_livestock, ',', simplify = T)
colnames(crop_livestock) <- paste0('crop livestock_', 1:ncol(crop_livestock))

gender <- str_split(df$Gender, ',', simplify = T)
colnames(gender) <- paste0('Gender_', 1:ncol(gender))

country <- str_split(df$country, ',', simplify = T)
colnames(country) <- paste0('country_', 1:ncol(country))


df <- cbind(df, intervention, outcome, environmental, studyType, crop_livestock, gender, country)
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
  select(studyID, keyword_category, keyword_subcategory) %>%
  filter(keyword_subcategory != 'no')



######################################
# Plot results
######################################
dat <- df

######################################
# Location of studies
######################################

wb_class <- read.csv('input/worldbank_countryclassification.csv', 
                     stringsAsFactors = F)
wb_class <- unique(wb_class[,c('CountryCode', 'CountryName')])
# wb_class$CountryName <- as.character(wb_class$CountryName)
# wb_class$CountryCode <- as.character(wb_class$CountryCode)

URL <- 'https://github.com/nvkelso/natural-earth-vector/raw/master/geojson/ne_50m_admin_0_countries.geojson'
fil <- paste0('input/', basename(URL))

if (!file.exists(fil)) download.file(URL, fil)
world <- readOGR(dsn = 'input/ne_50m_admin_0_countries.geojson')
world <- world[!world$ISO_A3 %in% c('ATA'),]
world <- spTransform(world, CRS('+proj=wintri'))

map <- tidy(world, region = 'ISO_A3')

tmp <- dat %>%
  filter(keyword_category == 'country') %>%
  mutate(keyword_subcategory = str_trim(keyword_subcategory),
         keyword_subcategory = str_to_title(keyword_subcategory)) %>%
  group_by(keyword_subcategory) %>%
  summarise(n = length(keyword_subcategory))

tmp <- merge(tmp, wb_class, by.x = 'keyword_subcategory', by.y = 'CountryName', all.x = T, all.y = F)
tmp$CountryCode <- ifelse(tmp$keyword_subcategory == 'United States Of America', 'USA',
                   ifelse(tmp$keyword_subcategory == 'Laos', 'LAO', 
                   ifelse(tmp$keyword_subcategory == 'Iran', 'IRN',
                   ifelse(tmp$keyword_subcategory == 'Saint Lucia', 'LCA',
                          tmp$CountryCode))))

ggplot() +
  geom_map(data = map, 
           map = map,
           aes(x = long, y = lat, map_id = id, group = group, color = '0'),
           size = .05,
           fill = '#d6d7dc',
           alpha = .6) +
  geom_map(data = tmp, 
           map = map, 
           color = 'white', 
           size = 0.0,
           aes(fill = n, 
               group = CountryCode, 
               map_id = CountryCode)) +
  scale_fill_continuous_tableau(palette = 'Blue', 
                                trans = 'sqrt',
                                breaks = c(5, 20, 40, 75),
                                labels = c(5, 20, 40, 80)) +
  scale_color_manual(values = '#d6d7dc') +
  labs(fill = element_blank()) +
  coord_equal(ratio = 1) + 
  theme_map(font_family = 'Arial', font_size = base_size) +
  xlim(c(-1.15e+07, 1.3e+07)) +
  guides(color = guide_legend(label.position = 'bottom',
                              title = '', 
                              label.hjust = 0,
                              keywidth = 1.5)) +
  theme(plot.subtitle = element_text(hjust = .49, vjust = -140),
        legend.position = 'bottom',
        legend.justification = c(.5,0),
        legend.key  =  element_blank(),
        legend.spacing.x = unit(-.26, 'cm'),
        legend.text = element_text(size = 8)) +
  ggtitle('', subtitle = 'Number of Studies') 

ggsave('output/fig_map_n.png', width = 7, height = 7)


######################################
# Bar plot of funding needs
######################################
df_needs <- read.csv('input/water_stressed_fs_country.csv')
df_needs$ISO3 <- df_needs$ISO3_usable
df_needs <- df_needs %>% 
  filter(waterScarce == 'Water Scarce' & 
         size == 'Small-scale') %>%
  mutate(waterScarce = paste(str_to_title(waterScarce), 'Regions'),
         size = paste(str_to_title(size), 'Farms'))

df_needs <- merge(df_needs, tmp, 
                  by.x = 'ISO3', by.y = 'CountryCode', 
                  all.x = T, all.y = F) %>%
  mutate(Income.group = str_to_title(Income.group))

tmp1 <- df_needs %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  group_by(Income.group) %>%
  arrange(desc(tot_fs), .by_group = T) %>%
  top_n(15, tot_fs) %>%
  ungroup() %>%
  mutate(ix = rev(1:n()),
         tot_fs = tot_fs / 1000000,
         country = ifelse(country == 'United Republic of Tanzania', 
                          'Tanzania', 
                          as.character(country)),
         country = ifelse(country == 'Democratic Republic of the Congo', 
                          'Dem. Rep. Congo', 
                          as.character(country)),
         country = factor(country, rev(country))) %>%
  arrange(Income.group, desc(ix)) %>%
  head(35)

x_scale <- c(1, 5, 20, 80)

p <- ggplot(tmp1, aes(country, log(tot_fs+1))) +
  geom_linerange(aes(ymin = -1, ymax = log(tot_fs+1)), 
                 size = .5,
                 color = 'grey70') +
  geom_point(aes(color = Income.group, size = n)) +
  coord_flip(ylim = c(.2, log(85))) +
  scale_y_continuous(breaks = log(x_scale+1),
  labels = x_scale) +
  xlab('') +
  ylab('\nNumber of Small-Scale Farms\nin Water-Scarce Regions (in millions)') +
  labs(color = element_blank()) +
  guides(size = F, 
         colour = guide_legend(
           override.aes = list(size = 5))) +
           # label.hjust = -5)) +
  theme_minimal(base_size = base_size) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        # plot.title = element_text(hjust = .5),
        # plot.subtitle = element_text(hjust = .5),
        panel.grid = element_blank(),
        strip.text = element_blank(),
        # legend.position = 'right',
        # legend.spacing.x = unit(.05, 'cm'),
        legend.spacing.x = unit(.55, 'cm'),
        # legend.text = element_text(size = base_size - 4),
        panel.spacing.y = unit(-.6, 'lines'))

library(ggforce)
circles <- data.frame(
  x0 = rep(1, 3),
  y0 = c(-.2,0,.2),
  r = seq(0.5, 1, length.out = 3)
)

# Use coord_fixed to ensure true circularity
l <- ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles) +
  coord_fixed() +
  theme_void() +
  labs(caption = 'Number of Studies') +
  theme(plot.caption = element_text(hjust = -.84, vjust = 13, size = 11))
 
p + draw_plot(l, 10, 2.8, 5, 5, .9)

ggsave('output/fig_bar_needs.png', width = 7, height = 7, dpi = 72)


######################################
# Matrix with study type
######################################

tmp <- dat %>% 
  filter(keyword_category %in% c('outcomes', 'interventions', 'type of study') &
         keyword_subcategory != 'other' &
         keyword_subcategory != 'labor') %>%
  mutate(keyword_subcategory = as.character(keyword_subcategory),
         keyword_subcategory = ifelse(grepl('yield|productivity', keyword_subcategory, ignore.case = T), 
                                      'yield', 
                                      keyword_subcategory)) %>%
  select(keyword_category, keyword_subcategory, studyID) %>%
  na.omit()


tmp1 <- tmp %>% filter(keyword_category %in% c('outcomes')) %>% select(keyword_subcategory, studyID)
tmp2 <- tmp %>% filter(keyword_category %in% c('interventions')) %>% select(keyword_subcategory, studyID)
tmp3 <- tmp %>% filter(keyword_category %in% c('type of study')) %>% select(keyword_subcategory, studyID)

colnames(tmp1)[1] <- c('outcomes')
colnames(tmp2)[1] <- c('interventions')
colnames(tmp3)[1] <- c('studytype')

tmp <- unique(merge(tmp1, tmp2, by = 'studyID', all = T))
tmp <- unique(merge(tmp, tmp3, by = 'studyID', all = T))
tmp <- dcast(tmp, outcomes + studytype ~ interventions)
tmp <- melt(tmp, id.vars = c('outcomes', 'studytype'))
tmp <- tmp %>% 
  mutate(outcomes = str_to_title(outcomes),
         variable = str_to_title(variable),
         studytype = str_to_title(studytype)) %>%
  filter(variable != 'Na') %>% 
  na.omit()

tmp_aggregate <- tmp %>% 
  group_by(outcomes, variable) %>% 
  summarise(value = sum(value))

tmp <- tmp %>% filter(value > 0)


tmp$variable <- ifelse(tmp$variable == 'Conservation',
                       str_wrap('Natural Resource Protection', 16),
                       tmp$variable)

tmp$studytype <- str_trim(str_remove_all(
  tmp$studytype, 'Primary|Secondary|Data|With'))
  
tmp <- tmp %>% 
  group_by(studytype) %>%
  mutate(order = sum(value)) %>%
  ungroup() %>%
  arrange(desc(order)) %>% 
  mutate(studytype = factor(studytype, unique(studytype))) %>%
  group_by(variable) %>%
  mutate(order = sum(value)) %>%
  ungroup() %>%
  arrange(desc(order)) %>% 
  mutate(variable = factor(variable, unique(variable)),
         outcomes = factor(outcomes, rev(c('Yield', 'Income/Expenditure'))))

ggplot(tmp, aes(fill = studytype, values = value)) +
  geom_waffle(color = 'white', n_rows = 5, flip = F) +
  facet_grid(rows = vars(variable),
             cols = vars(outcomes), 
             switch = 'y') +
  scale_y_discrete() +
  scale_x_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  coord_equal() +
  labs(
    title = 'Outcomes by Interventions',
    x = '\nNumber of Studies',
    y = '',
    fill = 'Methodology Study Used'
  ) +
  theme(panel.grid = element_blank(),
        # legend.position = 'bottom',
        legend.title = element_text(size = base_size - 2),
        # axis.text.y = element_text(size = base_size),
        # axis.text.x = element_text(size = base_size),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        strip.text.y.left = element_text(angle = 0, hjust = 0)) 
  # guides(fill = guide_legend(reverse = F, nrow = 2, byrow = T, 
                             # title.position = 'top'))



ggsave('output/fig_matrix.png', width = 8, height = 6)


######################################
# Crop versus livestock systems
######################################

# metadata <- read_excel('input/Ceres2030-Team 6/Ceres_team6_full_text_info_tracking_20191212.xlsx',
#                        range = NULL, col_names = TRUE, col_types = NULL, 
#                        na = '', trim_ws = TRUE, skip = 0, n_max = Inf,
#                        guess_max = 25, sheet = 4) %>% as.data.frame()
metadata <- read.csv('input/Ceres2030-Team 6/metadata.csv')
metadata <- metadata[, c('id', 'year')]
colnames(metadata)[1] <- 'studyID'

tmp <- dat %>% 
  filter(keyword_category %in% c('crop_livestock')) %>%
  select(keyword_subcategory, studyID) %>%
  na.omit()
tmp <- unique(tmp)

tmp <- merge(tmp, metadata, by = 'studyID', all.x = T, all.y = F)

tmp <- tmp %>%
  mutate(keyword_subcategory = str_to_title(keyword_subcategory)) %>%
  group_by(year, keyword_subcategory) %>%
  summarise(n = n_distinct(studyID)) %>% 
  ungroup()

tmp %>%
  filter(keyword_subcategory %in% c('Crop', 'Livestock', 'Mixed')) %>%
  group_by(keyword_subcategory) %>% 
  expand(year = seq(min(tmp$year), max(tmp$year))) %>% 
  unnest() %>%
  left_join(tmp) %>%
  arrange(year, .by_group = T) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         nc = cumsum(n)) %>%
  ungroup() %>%
  mutate(keyword_subcategory = factor(keyword_subcategory, 
                                      c('Crop', 'Mixed', 'Livestock'))) %>%
  ggplot(aes(year, nc, fill = keyword_subcategory)) +
  geom_area(stat = 'identity', position = position_dodge()) +
  labs(fill = '') +
  xlab('') +
  ylab('Number of Studies\n(Cumulative)\n') +
  theme_minimal(base_family = 'Arial', base_size = base_size) +
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave('output/fig_system.png', width = 9, height = 4)


######################################
# Gender and enviro dimensions
######################################

tmp <- dat %>% 
  filter(keyword_category %in% c('gender', 'environmental impacts')) %>%
  select(keyword_category, studyID) %>%
  na.omit()
tmp <- unique(tmp)

df_include <- na.omit(df_include)
tmp <- merge(tmp, df_include, by = 'studyID', all.x = T, all.y = T)
tmp <- merge(tmp, metadata, by = 'studyID', all.x = T, all.y = F)

tmp <- tmp %>%
  mutate(keyword_category = ifelse(is.na(keyword_category), 
                                   'no cross-cutting theme',
                                   keyword_category),
         keyword_category = str_to_title(keyword_category)) %>%
  group_by(year, keyword_category) %>%
  summarise(n = n_distinct(studyID)) %>% 
  ungroup() %>%
  mutate(keyword_category = factor(keyword_category, 
                                   c('Environmental Impacts',
                                     'No Cross-Cutting Theme',
                                     'Gender')))

p1 <- tmp %>%
  group_by(keyword_category) %>% 
  expand(year = seq(min(tmp$year), max(tmp$year))) %>% 
  unnest() %>%
  left_join(tmp) %>%
  arrange(year, .by_group = T) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         nc = cumsum(n)) %>% 
  ggplot(aes(year, nc, fill = keyword_category)) +
  geom_area(stat = 'identity') +
  scale_fill_manual(values = c('#7C73C3', '#FFD186', '#6A8EBC')) +
  labs(fill = '') +
  xlab('') +
  ylab('Number of Studies\n(Cumulative)\n') +
  xlim(1975, 2020) +
  theme_minimal(base_family = 'Arial', base_size = base_size) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


dev.off()
g1 <- ggplotGrob(p1)
g1$layout$clip[g1$layout$name == 'panel'] = 'off'


# ggsave('output/fig_gender_enviro.png', width = 9, height = 4)


######################################
# Enviro impacts dissaggregated
######################################
tmp <- dat %>% 
  filter(keyword_category %in% c('environmental impacts')) %>%
  select(keyword_subcategory, studyID) %>%
  na.omit()
tmp <- unique(tmp)

tmp <- merge(tmp, metadata, by = 'studyID', all.x = T, all.y = F)

tmp <- tmp %>%
  filter(keyword_subcategory != 'environmental') %>%
  mutate(keyword_subcategory = str_to_title(keyword_subcategory)) %>%
  mutate(keyword_subcategory = ifelse(
    keyword_subcategory == 'Climate Change', 
    'GHG Emissions',
    keyword_subcategory)) %>%
  group_by(year, keyword_subcategory) %>%
  summarise(n = n_distinct(studyID)) %>% 
  ungroup() %>%
  mutate(keyword_subcategory = factor(keyword_subcategory, 
                                   unique(keyword_subcategory)))

tmp <- tmp %>%
  filter(keyword_subcategory != 'Other') %>%
  group_by(keyword_subcategory) %>% 
  expand(year = seq(min(tmp$year), max(tmp$year))) %>% 
  unnest() %>%
  left_join(tmp) %>%
  arrange(year, .by_group = T) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         nc = cumsum(n),
         tot = max(nc)) %>%
  ungroup() %>%
  arrange(desc(tot)) %>%
  mutate(keyword_subcategory = factor(keyword_subcategory, 
                                      unique(keyword_subcategory)))

lbrl_bio <- tmp %>%
  filter(year == 2019) %>%
  arrange(desc(keyword_subcategory)) %>%        
  mutate(y = cumsum(nc)) %>%
  filter(keyword_subcategory == 'Biodiversity')


lbrl_h20 <- tmp %>%
  filter(year == 2019) %>%
  arrange(desc(keyword_subcategory)) %>%        
  mutate(y = cumsum(nc)) %>%
  filter(keyword_subcategory == 'Water Quantity')

 
p2 <- ggplot(tmp, aes(year, nc, fill = keyword_subcategory)) +
  geom_area(stat = 'identity') +
  labs(fill = '') +
  xlab('') +
  ylab('Number of Studies\n(Cumulative)\n') +
  theme_minimal(base_family = 'Arial', base_size = base_size) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

g <- ggplotGrob(p2)
g$layout$clip[g$layout$name == 'panel'] = 'off'

pb <- ggplot() + 
  geom_blank() + 
  ggtitle('Cross-Cutting Themes') + 
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        panel.background = element_blank(),
        axis.line = element_blank())

plot_grid(pb, p1, g, ncol = 1, labels = c('', '', ''), greedy = F,
          rel_heights = c(.2,1,1),
          vjust = -1)


ggsave('output/fig_cross_cutting.png', width = 9, height = 7)






######################################
# Irrigation gaps
######################################

wb_class <- read.csv('input/worldbank_countryclassification.csv', 
                     stringsAsFactors = F)

regions <- c(
  "East Asia & Pacific",
  "Europe & Central Asia",
  "Latin America & Caribbean",
  "Middle East & North Africa",
  "North America",
  "South Asia",
  "Sub-Saharan Africa"
  )

incomes <- c(
  "Low income",
  "Lower middle income",
  "Upper middle income",
  "High income" 
)
  
wb_class <- wb_class %>% filter(GroupName %in% regions)

df_irr <- fread('input/water_stressed_fs_country.csv')
df_irr_all <- fread('input/water_stressed_fs_all.csv')

df_irr <- merge(df_irr, wb_class, 
                by.x = 'ISO3_usable', 
                by.y = 'CountryCode', 
                all.x = T, all.y = F)

df_irr %>%
  filter(is.finite(p_irr_fs) & 
           !is.na(p_irr_fs)) %>%
  na.omit() %>%
  mutate(GroupName = 'All LMIC Countries') %>%
  group_by(GroupName, waterScarce, size) %>%
  summarise(tot = sum(tot_fs, na.rm = T),
            tot_irr_fs = sum(t_irr_fs, na.rm = T),
            p_irr_fs = 100 * tot_irr_fs / tot) %>%
  select(GroupName, waterScarce, size, p_irr_fs)

tmp1 <- df_irr %>%
  filter(is.finite(p_irr_fs) & 
         !is.na(p_irr_fs)) %>%
  na.omit() %>%
  group_by(GroupName, waterScarce, size) %>%
  summarise(tot = sum(tot_fs, na.rm = T),
            tot_irr_fs = sum(t_irr_fs, na.rm = T),
            p_irr_fs = 100 * tot_irr_fs / tot) %>% 
  ungroup() %>%
  arrange(desc(waterScarce), desc(size), p_irr_fs) %>%
  select(GroupName, waterScarce, size, p_irr_fs)


tmp2 <- df_irr %>%
  filter(is.finite(p_irr_fs) & 
        !is.na(p_irr_fs)) %>%
  na.omit() %>%
  mutate(GroupName = 'All LMIC Countries') %>%
  group_by(GroupName, waterScarce, size) %>%
  summarise(tot = sum(tot_fs, na.rm = T),
            tot_irr_fs = sum(t_irr_fs, na.rm = T),
            p_irr_fs = 100 * tot_irr_fs / tot) %>%
  select(GroupName, waterScarce, size, p_irr_fs)

tmp <- rbind(as.data.frame(tmp2), as.data.frame(tmp1))
tmp <- tmp %>%
  mutate(GroupName = factor(GroupName, unique(GroupName)),
         waterScarce = factor(waterScarce, rev(unique(waterScarce))))
library(ggthemes)

ggplot(tmp, aes(GroupName, p_irr_fs, fill = size)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  facet_wrap(vars(waterScarce)) +
  xlab('') +
  ylab('\nPercent of Farms with Irrigation\nin Low and Middle Income Countries') +
  theme(axis.text.y = element_text(hjust = 0),
        legend.position = 'bottom') +
  facet_wrap(vars(waterScarce)) +
  theme_minimal(base_size = base_size) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        axis.text.y = element_text(hjust = 0)) +
  labs(fill = element_blank()) 

ggsave('output/fig_irrigation_gap_country.png', width = 8, height = 6)




# Irrigation by country
tmp <- df_irr %>%
  filter(is.finite(p_irr_fs) & 
           !is.na(p_irr_fs)) %>%
  na.omit()
  
ggplot() +
  geom_map(data = map, 
           map = map,
           aes(x = long, y = lat, map_id = id, group = group, color = 'No farms\nmatched criteria'),
           size = .05,
           fill = '#d6d7dc',
           alpha = .6) +
  geom_map(data = tmp, 
           map = map, 
           color = 'white', 
           size = 0.0,
           aes(fill = p_irr_fs, 
               group = ISO3_usable, 
               map_id = ISO3_usable)) +
  scale_fill_continuous_tableau(palette = 'Blue',
                                trans = 'sqrt',
                                breaks = c(5, 20, 40, 100),
                                labels = c(5, 20, 40, 100)) +
  scale_color_manual(values = '#d6d7dc') +
  labs(fill = element_blank()) +
  coord_equal(ratio = 1) + 
  theme_map(font_family = 'Arial', font_size = base_size) +
  xlim(c(-1.15e+07, 1.3e+07)) +
  guides(color = guide_legend(label.position = 'bottom',
                              title = '', 
                              label.hjust = 0,
                              keywidth = .5)) +
  theme(plot.subtitle = element_text(hjust = .49, vjust = -165),
        legend.position = 'bottom',
        legend.justification = c(.5,0),
        legend.key  =  element_blank(),
        legend.spacing.x = unit(-.3, 'cm'),
        legend.text = element_text(size = 8)) +
  ggtitle('', subtitle = 'Percent Irrigated') +
  facet_grid(rows = vars(size),
             cols = vars(waterScarce))

ggsave('output/fig_irrigation_gap_country_SI.png', width = 8, height = 8)


# Global stat on irrigation
df_irr %>%
  filter(is.finite(p_irr_fs) & 
           !is.na(p_irr_fs)) %>%
  na.omit() %>%
  group_by(size) %>%
  summarise(tot = sum(tot_fs, na.rm = T),
            tot_irr_fs = sum(t_irr_fs, na.rm = T),
            p_irr_fs = 100 * tot_irr_fs / tot) %>%
  select(size, p_irr_fs)

# Global stats on percent of farms (and area) in water scarce regions
df_irr %>% 
  group_by(size) %>%
  mutate(f = sum(tot_fs, na.rm = T),
         a = sum(tot_ag, na.rm = T)) %>%
  group_by(size, waterScarce) %>%
  summarise(p_f = 100 * sum(tot_fs) / max(f), 
            p_a = 100 * sum(tot_ag) / max(a)) %>% 
  na.omit()

######################################
# Themes over time
######################################


tmp <- dat %>% na.omit()
tmp <- unique(tmp)
tmp <- merge(tmp, metadata, by = 'studyID', all.x = T, all.y = F)
tmp <- tmp %>%
  filter(keyword_category == 'interventions') %>%
  mutate(keyword_subcategory = ifelse(keyword_subcategory == 'conservation', 
                                      'natural-resource protection', 
                                      keyword_subcategory),
         keyword_subcategory = str_to_title(keyword_subcategory)) %>%
  group_by(year, keyword_subcategory) %>%
  summarise(n = n_distinct(studyID)) %>% 
  ungroup()


tmp %>%
  group_by(keyword_subcategory) %>% 
  expand(year = seq(min(tmp$year), max(tmp$year))) %>% 
  unnest(cols = year) %>%
  left_join(tmp) %>% 
  group_by(keyword_subcategory) %>% 
  arrange(year, .by_group = T) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         nc = cumsum(n),
         order = max(n)) %>% 
  group_by(year) %>%
  mutate(pc = 100 * nc / sum(nc)) %>%
  ungroup() %>%
  arrange(desc(year), desc(order)) %>%
  mutate(keyword_subcategory = factor(keyword_subcategory, 
                                      unique(keyword_subcategory))) %>%
  ggplot(aes(year, pc, fill = keyword_subcategory)) +
  geom_area(stat = 'identity', color = 'white') +
  labs(fill = '') +
  xlab('') +
  ylab('Percent of Studies per Year\n') +
  theme_minimal(base_family = 'Arial', base_size = base_size) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave('output/fig_themes_time_percent.png', width = 10, height = 6)



######################################
# Yield vs livelihood over time
######################################

tmp <- dat %>% 
  filter(keyword_category == 'outcomes') %>%
  merge(., metadata, by = 'studyID', all.x = T, all.y = F) %>%
  mutate(keyword_subcategory = ifelse(grepl('yield|productivity', 
                                            keyword_subcategory, ignore.case = T), 
                                      'Yield', 
                                      str_to_title(keyword_subcategory))) %>% 
  filter(keyword_subcategory != 'Other') %>%
  group_by(year, keyword_subcategory) %>%
  summarise(n = length(unique(studyID)))


ggplot(tmp, aes(year, n)) +
  geom_col() +
  facet_wrap(vars(keyword_subcategory)) +
  labs(fill = '') +
  xlab('') +
  ylab('Number of Studies\n') +
  theme_minimal(base_family = 'Arial', base_size = base_size) +
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggsave('output/fig_outcomes_time.png', width = 9, height = 4)

