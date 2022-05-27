#https://timss2019.org/international-database/

library(haven)
library(glue)
library(labelled)

get_country <- function(cntry, grade="8", year="19", file="bcgfinm7"){
  temp <- tempfile()
  download.file(glue(
    "https://timss20{year}.org/international-database/downloads/data/grade{grade}/T{year}_G{grade}_{cntry}_SPSS.zip"),
  temp) # download zip file and 
  return(read_sav(unz(temp, glue("{file}.sav")))) # load the SPSS file for...
}

# fetch the school data for any given set of countries
school_data <- 
  map_dfr(c("ENG", "FIN", "RUS", "FRA", "HKG", "SWE", "USA", "IRL", "ISR", "NOR", "TUR"),
         ~ {
           low_name <- tolower(.x)
           get_country(.x, file=glue("bcg{low_name}m7")) %>%
             map_dfc(function(y) to_factor(y)) %>% 
             mutate(CNTRY = .x)}
        )

# convert everything to factors
# school_data <- school_data %>% 
#   map_dfc(~ to_factor(.x))

# how long are the school days?
school_data %>% filter(!is.na(BCDGTIHY)) %>%
  mutate(BCDGTIHY = as.numeric(levels(BCDGTIHY)[BCDGTIHY])) %>%
  #group_by(CNTRY) %>% count()
  ggplot(aes(x=CNTRY, y=BCDGTIHY)) +
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = 
                       quantile(as.numeric(levels(school_data$BCDGTIHY)[school_data$BCDGTIHY]), 
                                c(0.1, 0.9),
                                na.rm=TRUE))
