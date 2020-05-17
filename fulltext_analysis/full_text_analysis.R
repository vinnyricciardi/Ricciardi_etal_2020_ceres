library(tidyverse)
library(reshape2)
library(data.table)
library(zoo)
library(rgdal)
library(ggthemes)
library(gridExtra)
library(broom)
library(ggthemr)
library(grid)
library(cowplot)
library(ggpubr)

######################################
# Define ggplot theme
######################################
ggthemr('light')
tg <- theme_get()

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

wb_class <- read.csv('input/worldbank_countryclassification.csv')
wb_class <- unique(wb_class[,c('CountryCode', 'CountryName')])
wb_class$CountryName <- as.character(wb_class$CountryName)
wb_class$CountryCode <- as.character(wb_class$CountryCode)

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
           aes(x = long, y = lat, map_id = id, group = group),
           fill = '#d6d7dc', 
           size = .05,
           alpha = .6,
           color = NA) +
  geom_map(data = tmp, 
           map = map, 
           color = 'white', 
           size = 0.15,
           aes(fill = n, 
               group = CountryCode, 
               map_id = CountryCode)) +
  # scale_fill_gradient(low = '#b1c5d4', 
  #                     high = '#686573',
  #                     name = 'Number of Studies') +
  scale_fill_gradient(trans = 'sqrt') +
  labs(fill = 'Number of Studies') +
  coord_equal(ratio = 1) + 
  theme_map() +
  xlim(c(-1.15e+07, 1.3e+07)) +
  theme(legend.position = 'right') +
  theme(legend.key  =  element_blank()) +
  theme(plot.title = element_text(size = 14,
                                  face = 'bold', 
                                  color = tg$text$colour,
                                  hjust = .5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  ggtitle('Research Coverage') +
  annotate('text',
           x = -2.5e6,
           y = -1e6,
           label = str_wrap(
             'Limited studies in central Africa', 17),
           hjust = 0,
           size = 3) +
  annotate('text',
           x = 5e6,
           y = -6e6,
           label = str_wrap(
             'Research stations in higher income countries 
             tested interventions for small-scale farms', 30),
           hjust = 0,
           size = 3) 

ggsave('output/fig_map_n.png')


######################################
# Map of funding needs
######################################

df_needs_low <- read.csv('input/water_stressed_fs_countryLevel_hdiLow.csv')
df_needs_med <- read.csv('input/water_stressed_fs_countryLevel_hdiMed.csv')

prep_df_needs <- function(dat) {
  
  dat <- dat %>%
    filter(waterScarce == 'Water Scarce' &
           size == 'Small-scale') %>%
    mutate(tot = tot / 1000000)
  
  return(dat)
}
  
plot_needs_map <- function(dat, lab) {
  
  dat <- prep_df_needs(dat)
  
  p <- ggplot() +
    geom_map(data = map, 
             map = map,
             aes(x = long, y = lat, map_id = id, group = group),
             fill = '#d6d7dc', 
             size = .05,
             alpha = .6,
             color = NA) +
    geom_map(data = dat, 
             map = map, 
             color = 'white', 
             size = 0.15,
             aes(fill = tot, 
                 group = ISO3, 
                 map_id = ISO3)) +
    scale_fill_gradient(low = '#FFB7B7', 
                        high = '#B13D3D',
                        name = "Water-Scarce Small-Scale Farms'\nAgricultural Area (Million ha)") +
    coord_equal(ratio = 1) + 
    theme_map() +
    theme(legend.position = 'bottom') +
    theme(legend.key  =  element_blank()) +
    theme(plot.title = element_text(size = 14),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) +
    ggtitle(lab)

  return(p)
}

# p2a <- plot_needs_map(df_needs_low, 'Funding Needs for Smallholders')
# ggsave('output/fig_map_needs_low.png')
# 
# p2b <- plot_needs_map(df_needs_med, 'Funding Needs for Smallholders')
# ggsave('output/fig_map_needs_med.png')


######################################
# Bar plot of funding needs
######################################
tmp_bar <- tmp
dat_low <- prep_df_needs(df_needs_low)
dat_med <- prep_df_needs(df_needs_med)
dat_low$variable <- 'Most Vulnerable\nSmall-Scale Farms\n(Low HDI)'
dat_med$variable <- 'Most Vulnerable and Moderately Vulnerable\nSmall-Scale Farms\n(Low and medium HDI)'
dat_tmp <- rbind(dat_low, dat_med)

tmp1 <- merge(dat_tmp, tmp_bar, 
             by.x = 'ISO3', by.y = 'CountryCode', 
             all.x = T, all.y = F)

tmp1 <- merge(tmp1, wb_class, 
             by.x = 'ISO3', by.y = 'CountryCode', 
             all.x = T, all.y = F)

tmp1$CountryName <- ifelse(tmp1$CountryName == "Korea, Dem. People's Rep.",
                           'Korea (DPRK)', tmp1$CountryName)
tmp1$CountryName <- ifelse(tmp1$CountryName == 'Syrian Arab Republic',
                           'Syria', tmp1$CountryName)

tmp1 <- tmp1 %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  group_by(variable) %>%
  arrange(desc(tot), .by_group = T) %>%
  top_n(20, tot) %>%
  ungroup() %>%
  mutate(ix = rev(1:n()),
         variable = factor(variable, (unique(variable))))


p <- ggplot(tmp1, aes(ix, tot, fill = n),
       alpha = .3) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_fill_gradient(trans = 'sqrt') +
  labs(fill = 'Number of Studies') +
  xlab('') +
  ylab("\nWater-Scarce Small-Scale Farms'\nAgricultural Area (Million ha)") +
  theme(axis.text.y = element_text(hjust = 0),
        strip.text = element_text(size = tg$strip.text$size - .0),
        plot.title = element_text(hjust = .5),
        panel.grid = element_blank()) +
  ggtitle('Funding Needs & Research Coverage') +
  facet_wrap(~ variable, scales = 'free') +
  scale_x_continuous(
    breaks = tmp1$ix,
    labels = tmp1$CountryName,
    expand = c(0, 0)
  )

ann_text1 <- data.frame(ix = 40, tot = 10.8036407, n = 1, lab = 'Text',
                        variable = factor(levels(tmp1$variable)[1],
                                          levels = levels(tmp1$variable)))

ann_text2 <- data.frame(ix = 20, tot = 50, n = 1, lab = 'Text',
                        variable = factor(levels(tmp1$variable)[2],
                                          levels = levels(tmp1$variable)))

p + 
  geom_text(data = ann_text1,
            label = 'The smallest\n40% of farms in\nNigeria in areas\nwith "low" HDI',
            nudge_y = -4, 
            nudge_x = -2,
            size = 3.25,
            hjust = 0) +
  geom_text(data = ann_text2,
            label = 'The smallest 40% of farms in\nIndia in areas with\n"low" or "medium" HDI\n',
            nudge_y = -4, 
            nudge_x = -2,
            size = 3.25,
            hjust = 0) 
  

ggsave('output/fig_bar_needs.png', height = 7, width = 9)




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

tmp$x <- as.integer(as.factor(tmp$outcomes))
tmp$y <- as.integer(as.factor(tmp$variable))
tmp$z <- as.integer(as.factor(tmp$studytype))

tmp$xpos <- (tmp$x + tmp$z/10) - .25
tmp$ypos <- tmp$y

tmp$variable <- ifelse(tmp$variable == 'Conservation',
                       str_wrap('Natural Resource Protection', 16),
                       tmp$variable)
tmp_aggregate$variable <- ifelse(tmp_aggregate$variable == 'Conservation',
                                 str_wrap('Natural Resource Protection', 16),
                       tmp_aggregate$variable)


tmp$variable <- factor(tmp$variable, unique(tmp$variable))
tmp_aggregate$variable <- factor(tmp_aggregate$variable, unique(tmp_aggregate$variable))

p <- ggplot(tmp) +
  geom_tile(aes(outcomes, variable),
            fill = 'transparent',
            data = tmp_aggregate,
            color = 'grey80') +
  geom_point(aes(x = xpos, y = ypos, size = value, color = studytype),
             shape = 'square') +
  scale_size(range = c(2, 10)) + 
  guides(color = guide_legend(override.aes = list(size = 5))) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = 'red'),
        plot.title = element_text(hjust = .5),
        axis.text.y = element_text(hjust = 0)) + 
  xlab('') +
  ylab('') +
  ggtitle('Research Coverage and Gaps') +
  labs(size = 'Number of Studies',
       color = 'Study Type') +
  annotate('text',
           x = 2.2,
           y = 4.3,
           label = str_wrap(
             'Limited studies on livestock or digital solutions', 20),
           hjust = 0,
           size = 2.5) +
  annotate('text',
           x = .5,
           y = 1,
           label = str_wrap(
             'Limited studies on trade-offs between outcomes and natural resource protection', 25),
           hjust = 0,
           size = 2.5) 
  # annotate('segment',
  #          x = 2.3,
  #          y = 4.85,
  #          xend = 2.25,
  #          yend = 4.9,
  #          arrow = arrow(type = 'closed', 
  #                        length = unit(0.15, 'cm'))) +
  #   annotate('segment',
  #            x = 2.3,
  #            y = 4.2,
  #            xend = 2.25,
  #            yend = 4,
  #            arrow = arrow(type = 'closed', 
  #                          length = unit(0.15, 'cm')))


g <- ggplotGrob(p)
g$layout$clip[g$layout$name == 'panel'] = 'off'
grid.draw(g)

ggsave('output/fig_matrix.png', height = 6, width = 8.5)



######################################
# Crop versus livestock systems
######################################

metadata <- readxl::read_excel('input/Ceres2030-Team 6/Ceres_team6_full_text_info_tracking_20191212.xlsx',
                               range = NULL, col_names = TRUE, col_types = NULL, 
                               na = '', trim_ws = TRUE, skip = 0, n_max = Inf,
                               guess_max = 25, sheet = 4) %>% as.data.frame()
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
  ylab('Number of Studies\n(Cumulative)\n')

ggsave('output/fig_system.png')


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
  annotate('text',
           x = 1995,
           y = 150,
           label = str_wrap(
             'Few studies looked at gender
             aspects of an intervention', 30),
           # hjust = 0,
           size = 3)
  
g1 <- ggplotGrob(p1)
g1$layout$clip[g1$layout$name == 'panel'] = 'off'
grid.draw(g1)

# ggsave('output/fig_gender_enviro.png')


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
  annotate('text',
           x = 1990,
           y = lbrl_bio$y + 50,
           label = str_wrap(
           'Few studies 
           measured trade-offs between yield 
           or income and biodiversity', 30),
           hjust = 0,
           size = 3) +
  annotate('text',
           x = 2005,
           y = lbrl_h20$y - 50,
           label = str_wrap(
             'Many studies 
             measured trade-offs between yield 
             or income and water quantity', 30),
           hjust = 0,
           size = 3)

g <- ggplotGrob(p2)
g$layout$clip[g$layout$name == 'panel'] = 'off'
grid.draw(g)

pb <- ggplot() + 
  geom_blank() + 
  ggtitle('Cross-Cutting Themes') + 
  theme(plot.title = element_text(hjust = .5),
        panel.background = element_blank(),
        axis.line = element_blank())

plot_grid(pb, p1, g, ncol = 1, labels = c('', 'A', 'B'), greedy = F,
          rel_heights = c(.2,1,1),
          vjust = -1)

ggsave('output/fig_cross_cutting.png')






######################################
# Irrigation gaps - global
######################################

df_irr_gap <- read.csv('input/water_stressed_fs_globalLevel_hdilow.csv')
tmp <- df_irr_gap %>%
  mutate(variable = factor(variable, 
                           rev(c('Irrigated',
                                 'Rainfed'))),
         value = value * 100)

tmp$lab <- ifelse(
  tmp$size == 'Small-scale' &
    tmp$waterScarce == 'Water Scarce',
  str_wrap("6% of water scarce small-scale farms' area is irrigated", 25),
  NA)

tmp$lab <- ifelse(
  tmp$size != 'Small-scale' &
    tmp$waterScarce == 'Water Scarce',
  str_wrap("16% of water scarce small-scale farms' area is irrigated", 25),
  tmp$lab)


p <- ggplot(tmp, aes(size, value, fill = variable)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab('') +
  labs(fill = '') +
  scale_fill_manual(values = rev(c(swatch()[7], swatch()[3], swatch()[4]))) +
  ylab('\nPercent of Agricultural Land\n') +
  facet_wrap(~ waterScarce) +
  ggtitle('Irrigation Coverage') +
  theme(plot.title = element_text(hjust = .5, color = tg$text$colour),
        axis.text.y = element_text(hjust = 0)) +
  geom_text(aes(label = lab), size = 2.9, color = 'black',
            hjust = 0, nudge_y = c(30, 15), nudge_x = c(-.2, .2))


figure <- ggarrange(p, ncol = 1, nrow = 1)

annotate_figure(figure,
               
)

ant <- annotation_custom(
  grob = linesGrob(
    arrow = arrow(type = 'closed',
                  ends = 'first',
                  length = unit(3, 'mm')),
    gp = gpar(col = 'grey40',
              lwd = 1)),
  xmin = 2.8,
  xmax = 2.2,
  ymin = 2,
  ymax = 4
)
p

ggsave('output/fig_irrigation_gap.png')



######################################
# Irrigation gaps - country level
######################################

df_irr_gap_c <- read.csv('input/water_stressed_fs_countryLevel_hdiLow_irrig.csv')
df_irr_gap_c <- merge(df_irr_gap_c, wb_class, 
                      by.x = 'ISO3', 
                      by.y = 'CountryCode', 
                      all.x = T, all.y = F)


df_irr_gap_c$gap2 <- ifelse(df_irr_gap_c$gap == 0, 0,
                     ifelse(df_irr_gap_c$gap > 0, sqrt(df_irr_gap_c$gap),
                     ifelse(df_irr_gap_c$gap < 0, -1 * sqrt(-1 * df_irr_gap_c$gap), 
                                                            df_irr_gap_c$gap)))
 
ggplot() +
  geom_map(data = map, 
           map = map,
           aes(x = long, y = lat, map_id = id, group = group),
           fill = '#d6d7dc', 
           size = .05,
           alpha = .6,
           color = NA) +
  geom_map(data = df_irr_gap_c, 
           map = map, 
           color = 'white', 
           size = 0.15,
           aes(fill = -1 * gap2, 
               group = ISO3, 
               map_id = ISO3)) +
  scale_fill_gradient2(low = '#b2432f',
                       mid = 'white',
                       high = '#62bba5',  
                       breaks = c(-1*sqrt(c(10,50,100)), sqrt(c(10,50,100))),
                       labels = c(-10, -50, -100, 10, 50, 100),
                       limits = c(-10, 10)) +
  labs(fill = 'Irrigation Gap (%)') +
  coord_equal(ratio = 1) + 
  theme_map() +
  xlim(c(-1.15e+07, 1.3e+07)) +
  theme(legend.position = 'right') +
  theme(legend.key  =  element_blank()) +
  theme(plot.title = element_text(size = 14,
                                  face = 'bold', 
                                  color = tg$text$colour,
                                  hjust = .5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  ggtitle('Irrigation Gap')

ggsave('output/fig_irrigation_gap_country.png')







######################################
# Misc
######################################
# hic <- read.csv('input/worldbank_countryclassification.csv')
# hic <- hic %>% filter(GroupName == 'High income')
# hic <- unique(as.character(hic$CountryName))
# hic <- ifelse(hic == 'United States', 'United States of America', hic)
# hic <- str_trim(hic)
# hic <- str_to_lower(hic)
# hics <- df %>% filter(keyword_subcategory %in% hic)
# hics$hic = hics$keyword_subcategory
# hics$keyword_category <- hics$keyword_subcategory <- NULL
# 
# tmp <- merge(df, hics, by = 'studyID', all = T)
# 
# tmp %>% 
#   filter(keyword_category == 'type of study') %>% 
#   # filter(!is.na(hic)) %>%
#   group_by(keyword_subcategory) %>%
#   summarise(n = n_distinct(studyID)) %>%
#   ungroup() %>%
#   mutate(p = n / length(unique(df$studyID)),
#          pt = sum(n) / length(unique(df$studyID))) %>%
#   arrange(desc(n))
# 
# 
# tmp <- df %>% 
#   filter(keyword_category == 'outcomes') %>%
#   mutate(keyword_subcategory = as.character(keyword_subcategory),
#          keyword_subcategory = ifelse(grepl('yield|productivity', keyword_subcategory, ignore.case = T), 
#                                       'yield', 
#                                       keyword_subcategory))
# tmp$keyword_category <- NULL  
# tmp <- dcast(tmp, studyID ~ keyword_subcategory)
# tmp$check <- ifelse(tmp$`income/expenditure` > 0 & tmp$yield > 0, 'both',
#                     ifelse(tmp$`income/expenditure` > 0, 'income',
#                            ifelse(tmp$yield > 0, 'yield', NA)))
# tmp %>% filter(check == 1) %>% n_distinct(studyID)
# 
# # filter(!is.na(hic)) %>%
#   group_by(keyword_subcategory) %>%
#   summarise(n = n_distinct(studyID)) %>%
#   ungroup() %>%
#   mutate(p = n / length(unique(df$studyID)),
#          pt = sum(n) / length(unique(df$studyID))) %>%
#   arrange(desc(n))

