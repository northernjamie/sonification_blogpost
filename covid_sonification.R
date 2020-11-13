# single script to get the data for England, animate the line chart and make the .wav file

#libraries

library(tidyverse) # for munging the data
library(audio) # to mmake the wav file
library(gganimate) # to animate the line chart
library(zoo) # for the rolling average

# get and prepare the data ----

# population estimates (for calculating rates)

pop_est <- readr::read_csv(paste0("data/ons_pop_est.csv")) %>% 
  select(1,4) %>% 
  rename(all_ages = 2)

# get the data from the coronavirus dashboard

la_case_data <- readr::read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  rename(specimen_date = 'Specimen date') %>% # rename so col name doesn't have spaces
  rename(area_name = 'Area name') %>% # rename so col name doesn't have spaces
  rename(area_code = 'Area code') %>% # rename so col name doesn't have spaces
  rename(area_type = 'Area type') %>% # rename so col name doesn't have spaces
  rename(no_cases = 'Daily lab-confirmed cases') %>% # rename so col name doesn't have spaces
  rename(cum_cases = 'Cumulative lab-confirmed cases') %>% # rename so col name doesn't have spaces
  rename(cum_cases_rate = 'Cumulative lab-confirmed cases rate') %>% # rename so col name doesn't have spaces
  filter(area_type == 'ltla')  # filter so that the dataframe only contains lower tier local authorities

latest_date <- max(la_case_data$specimen_date)

# sort the buckinghamshire problem by merging the old districts

la_case_data_bucks <- la_case_data %>% 
  filter(area_code %in% c('E07000004','E07000005','E07000006','E07000007')) %>% 
  group_by(specimen_date) %>% 
  summarise_at(c("no_cases","cum_cases"), sum,na.rm = TRUE) %>% 
  add_column(area_code = 'E06000060') %>% 
  add_column(area_type = 'ltla') %>% 
  add_column(area_name = 'Buckinghamshire')


la_case_data <- la_case_data %>% 
  filter(!(area_code %in% c('E07000004','E07000005','E07000006','E07000007'))) %>% 
  bind_rows(la_case_data_bucks)

# # # # # # # # # #

# calculate rolling 7 day averages for each local authority

la_case_data_rolling_7_day <- la_case_data %>% 
  filter(specimen_date < (latest_date-4)) %>% # remove the latest 5 days of data as it is inaccurate
  arrange(desc(area_code)) %>%  # sort the data by LA
  group_by(area_code) %>% # apply grouping for summarising
  mutate(case_07day = zoo::rollsum(no_cases,k = 7, fill =NA)) %>% # create a new column with a rolling 7 day average count
  drop_na(case_07day) %>% # get rid of any NAs
  ungroup() %>% # remove the grouping
  left_join(pop_est, by = c("area_code" = "Code")) %>% # join this dataframe with the population estimates
  mutate(rolling_7_day_rate = ((case_07day/all_ages)*100000)) %>%  #add a new column for the rate per 100,000 people
  select(area_code, specimen_date, rolling_7_day_rate) %>% # remove extra columns 
  drop_na(rolling_7_day_rate) # remove any rows with NA

# aggregate the local authority  data to get the data for England
la_case_data_rolling_7_day_england <- la_case_data %>%
  filter(specimen_date < (latest_date-4)) %>% # remove the latest 5 days of data as it is inaccurate
  group_by(specimen_date) %>% # group by the data of the sample
  summarise_at('no_cases',sum, na.rm = T) %>% # add the number of cases on each data across all local authorities
  ungroup() %>% # remove the grouping
  mutate(area_code = 'E92000001') %>% # add the code for England 
  arrange(specimen_date) %>%  # sort by date
  mutate(case_07day = zoo::rollsum(no_cases,k=7, fill = NA)) %>% #calculate the rolling average
  mutate(rolling_7_day_rate = ((case_07day/56286961)*100000)) %>% # calculate the rate per 100,000
  select(area_code, specimen_date,rolling_7_day_rate) %>% # remove extra columns
  drop_na(rolling_7_day_rate) %>% # remove NAs
  rbind(la_case_data_rolling_7_day) # add the england data to all LAs

# # # # # # # # # #

# make the music ----
# use ths data frame created above to make some music

music_dataset <- la_case_data_rolling_7_day_england %>% 
  filter(area_code == 'E92000001') %>% # filter to england only. Us local authority codes to make an LA tune
  mutate(max_value = max(rolling_7_day_rate)) %>%
  mutate(note = (rolling_7_day_rate/max_value)) %>% # calculate the value as a percentage of the max 
  mutate(frequency = (note * 440)+220) %>% # turn the percentage into a musical note frequency
  mutate(duration = 1) # set all durations to be equal to one

tempo <- 240 # set the tempo - this must must be 60 x the fps of the animated line chart for them to match up
sample_rate <- 44100 # set the sample rate (not sure what this does)

# this fucntion is the bit that turns the frequency and duration values into a song. Might as well be magic
make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

# do the above function on each row of the dataframe
covid_wave <-
  mapply(make_sine, music_dataset$frequency, music_dataset$duration) 

# play the tune
play(covid_wave)

# save the tune
save.wave(covid_wave,"outputs/sonification/england_cases_240bpm.wav")

# # # # # # # # # #

# Make the animated line chart ----

line_chart <- ggplot(music_dataset) +
  geom_vline(aes(xintercept=specimen_date),colour = "red") + # add the vertical red line
  theme_minimal() + #set the theme
  labs(title = "Rolling 7 day average daily covid-19 case rates in England",y = "Rolling 7 day average daily rate \n(new cases per 100,000 people)", x = "Date", caption = '2020-11-07 | @northernjamie') + # titles etc
  theme(axis.title = element_text(family = 'Gill Sans'), # fonts
        axis.text  = element_text(family = 'Gill Sans'),
        plot.caption = element_text(family = 'Gill Sans'),
        plot.title = element_text(family = 'Gill Sans')) +
  transition_reveal(specimen_date) + # set the animation style, and variable
  ease_aes('linear') + 
  geom_line(aes(x = specimen_date, y = rolling_7_day_rate, group = area_code)) # draw the line of covid case rates

# create the animation - one frame for each day. The frame rate (fps) must match the bpm for the chart and tune to line up
animate(line_chart,nframes = nrow(music_dataset), fps = 4, width = 16, height = 9, units = 'cm', res = 150)

# save the animated line chart as a gif
anim_save(file = 'outputs/sonification/animate_line_4fps.gif')

