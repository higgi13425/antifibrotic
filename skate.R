#Link for original data
#https://results.sporthive.com/events/PC1500302/races/4759037/bib/39_senior-women


# data entry with datapasta
library(datapasta)
library(tidyverse)
library(lubridate)
library(skimr)

#install addin for datapasta
# Rstudio/Tools/BrowseAddins/select datapasta

# use addin
# go to website
#select and copy data
# back to Rstudio

skatedf <- tibble::tribble(
              ~LAP.NO., ~LAPS.KM., ~POSITION,          ~PACE,  ~LAP.TIME,
               "Lap 2",  "0.8 km",        4L, "01:55 min/km",   "46.064",
               "Lap 3",  "1.2 km",        4L, "01:57 min/km",   "46.627",
               "Lap 4",  "1.6 km",        3L, "01:45 min/km",   "42.177",
               "Lap 5",    "2 km",        3L, "01:41 min/km",   "40.290",
               "Lap 6",  "2.4 km",        2L, "01:38 min/km",   "39.184",
               "Lap 7",  "2.8 km",        2L, "01:37 min/km",   "38.852",
               "Lap 8",  "3.2 km",        1L, "01:39 min/km",   "39.457",
               "Lap 9",  "3.6 km",        1L, "01:42 min/km",   "40.837",
              "Lap 10",    "4 km",        3L, "01:44 min/km",   "41.565",
              "Lap 11",  "4.4 km",        3L, "01:40 min/km",   "40.076",
              "Lap 12",  "4.8 km",        2L, "01:40 min/km",   "40.060",
              "Lap 13",  "5.2 km",        2L, "01:41 min/km",   "40.362",
              "Lap 14",  "5.6 km",        1L, "01:40 min/km",   "39.887",
              "Lap 15",    "6 km",        1L, "01:39 min/km",   "39.729",
              "Lap 16",  "6.4 km",        3L, "01:38 min/km",   "39.239",
              "Lap 17",  "6.8 km",        3L, "01:35 min/km",   "38.040",
              "Lap 18",  "7.2 km",        2L, "01:39 min/km",   "39.754",
              "Lap 19",  "7.6 km",        1L, "01:38 min/km",   "39.123",
              "Lap 20",    "8 km",        1L, "01:41 min/km",   "40.213",
              "Lap 21",  "8.4 km",        1L, "01:40 min/km",   "39.976",
              "Lap 22",  "8.8 km",        3L, "01:42 min/km",   "40.692",
              "Lap 23",  "9.2 km",        3L, "01:39 min/km",   "39.538",
              "Lap 24",  "9.6 km",        2L, "01:45 min/km",   "41.931",
              "Lap 25",   "10 km",        2L, "01:44 min/km",   "41.566",
              "Lap 26", "10.4 km",        1L, "01:41 min/km",   "40.249",
              "Lap 27", "10.8 km",        1L, "01:38 min/km",   "39.384",
              "Lap 28", "11.2 km",        1L, "01:41 min/km",   "40.265",
              "Lap 29", "11.6 km",        2L, "01:38 min/km",   "39.337",
              "Lap 30",   "12 km",        2L, "01:37 min/km",   "38.845",
              "Lap 31", "12.4 km",        1L, "01:40 min/km",   "39.868",
              "Lap 32", "12.8 km",        1L, "01:41 min/km",   "40.495",
              "Lap 33", "13.2 km",        1L, "01:44 min/km",   "41.786",
              "Lap 34", "13.6 km",        2L, "01:43 min/km",   "41.187",
              "Lap 35",   "14 km",        2L, "01:40 min/km",   "40.158",
              "Lap 36", "14.4 km",        1L, "01:44 min/km",   "41.656",
              "Lap 37", "14.8 km",        1L, "01:44 min/km",   "41.623",
              "Lap 38", "15.2 km",        1L, "01:41 min/km",   "40.585",
              "Lap 39", "15.6 km",        1L, "01:38 min/km",   "39.371",
              "Lap 40",   "16 km",        1L, "01:37 min/km",   "38.882",
              "Lap 41", "16.4 km",        2L, "01:42 min/km",   "40.655",
              "Lap 42", "16.8 km",        2L, "01:39 min/km",   "39.791",
              "Lap 43", "17.2 km",        1L, "01:39 min/km",   "39.631",
              "Lap 44", "17.6 km",        1L, "01:39 min/km",   "39.556",
              "Lap 45",   "18 km",        2L, "01:41 min/km",   "40.317",
              "Lap 46", "18.4 km",        2L, "01:41 min/km",   "40.464",
              "Lap 47", "18.8 km",        1L, "01:39 min/km",   "39.685",
              "Lap 48", "19.2 km",        1L, "01:38 min/km",   "39.281",
              "Lap 49", "19.6 km",        1L, "01:40 min/km",   "40.104",
              "Lap 50",   "20 km",        2L, "01:43 min/km",   "41.037",
              "Lap 51", "20.4 km",        1L, "01:42 min/km",   "40.604",
              "Lap 52", "20.8 km",        1L, "01:41 min/km",   "40.591",
              "Lap 53", "21.2 km",        1L, "01:40 min/km",   "40.067",
              "Lap 54", "21.6 km",        1L, "01:40 min/km",   "39.903",
              "Lap 55",   "22 km",        2L, "01:43 min/km",   "41.313",
              "Lap 56", "22.4 km",        2L, "01:39 min/km",   "39.515",
              "Lap 57", "22.8 km",        1L, "01:41 min/km",   "40.513",
              "Lap 58", "23.2 km",        1L, "01:44 min/km",   "41.631",
              "Lap 59", "23.6 km",        1L, "01:42 min/km",   "40.996",
              "Lap 60",   "24 km",        2L, "01:43 min/km",   "41.122",
              "Lap 61", "24.4 km",        2L, "01:41 min/km",   "40.370",
              "Lap 62", "24.8 km",        1L, "01:43 min/km",   "41.308",
              "Lap 63", "25.2 km",        1L, "01:42 min/km",   "40.791",
              "Lap 64", "25.6 km",        2L, "01:44 min/km",   "41.473",
              "Lap 65",   "26 km",        2L, "01:45 min/km",   "42.177",
              "Lap 66", "26.4 km",        1L, "01:43 min/km",   "41.051",
              "Lap 67", "26.8 km",        1L, "01:42 min/km",   "40.958",
              "Lap 68", "27.2 km",        2L, "01:46 min/km",   "42.497",
              "Lap 69", "27.6 km",        2L, "01:45 min/km",   "41.883",
              "Lap 70",   "28 km",        2L, "01:42 min/km",   "40.942",
              "Lap 71", "28.4 km",        2L, "01:41 min/km",   "40.547",
              "Lap 72", "28.8 km",        1L, "01:41 min/km",   "40.414",
              "Lap 73", "29.2 km",        1L, "01:40 min/km",   "40.023",
              "Lap 74", "29.6 km",        1L, "01:43 min/km",   "41.335",
              "Lap 75",   "30 km",        1L, "01:43 min/km",   "41.070",
              "Lap 76", "30.4 km",        2L, "01:46 min/km",   "42.224",
              "Lap 77", "30.8 km",        1L, "01:45 min/km",   "41.949",
              "Lap 78", "31.2 km",        1L, "01:43 min/km",   "41.181",
              "Lap 79", "31.6 km",        1L, "01:44 min/km",   "41.503",
              "Lap 80",   "32 km",        2L, "01:44 min/km",   "41.721",
              "Lap 81", "32.4 km",        2L, "01:43 min/km",   "41.391",
              "Lap 82", "32.8 km",        1L, "01:42 min/km",   "40.768",
              "Lap 83", "33.2 km",        1L, "01:40 min/km",   "40.199",
              "Lap 84", "33.6 km",        1L, "01:41 min/km",   "40.558",
              "Lap 85",   "34 km",        2L, "01:44 min/km",   "41.616",
              "Lap 86", "34.4 km",        2L, "01:45 min/km",   "41.815",
              "Lap 87", "34.8 km",        1L, "01:46 min/km",   "42.250",
              "Lap 88", "35.2 km",        1L, "01:44 min/km",   "41.508",
              "Lap 89", "35.6 km",        2L, "01:46 min/km",   "42.358",
              "Lap 90",   "36 km",        1L, "01:45 min/km",   "41.866",
              "Lap 91", "36.4 km",        1L, "01:45 min/km",   "41.925",
              "Lap 92", "36.8 km",        1L, "01:47 min/km",   "42.655",
              "Lap 93", "37.2 km",        2L, "01:46 min/km",   "42.360",
              "Lap 94", "37.6 km",        1L, "01:43 min/km",   "41.246",
              "Lap 95",   "38 km",        1L, "01:43 min/km",   "41.154",
              "Lap 96", "38.4 km",        1L, "01:41 min/km",   "40.407",
              "Lap 97", "38.8 km",        1L, "01:43 min/km",   "41.296",
              "Lap 98", "39.2 km",        2L, "01:46 min/km",   "42.566",
              "Lap 99", "39.6 km",        1L, "01:44 min/km",   "41.580",
             "Lap 100",   "40 km",        1L, "01:43 min/km",   "41.257",
             "Lap 101", "40.4 km",        2L, "01:45 min/km",   "42.178",
             "Lap 102", "40.8 km",        2L, "01:44 min/km",   "41.468",
             "Lap 103", "41.2 km",        2L, "01:46 min/km",   "42.516",
             "Lap 104", "41.6 km",        1L, "01:43 min/km",   "41.233",
             "Lap 105",   "42 km",        1L, "01:41 min/km",   "40.365",
             "Lap 106", "42.4 km",        1L, "01:45 min/km",   "41.853",
             "Lap 107", "42.8 km",        2L, "01:50 min/km",   "44.170",
             "Lap 108", "43.2 km",        1L, "01:40 min/km",   "40.069",
             "Lap 109", "43.6 km",        1L, "01:36 min/km",   "38.406",
             "Lap 110",   "44 km",        1L, "01:38 min/km",   "39.314",
             "Lap 111", "44.4 km",        1L, "01:38 min/km",   "39.303",
             "Lap 112", "44.8 km",        1L, "01:41 min/km",   "40.201",
             "Lap 113", "45.2 km",        1L, "01:43 min/km",   "41.350",
             "Lap 114", "45.6 km",        1L, "01:42 min/km",   "40.736",
             "Lap 115",   "46 km",        1L, "01:41 min/km",   "40.304",
             "Lap 116", "46.4 km",        1L, "01:41 min/km",   "40.296",
             "Lap 117", "46.8 km",        1L, "01:42 min/km",   "40.967",
             "Lap 118", "47.2 km",        1L, "01:45 min/km",   "41.965",
             "Lap 119", "47.6 km",        1L, "01:43 min/km",   "41.064",
             "Lap 120",   "48 km",        1L, "01:42 min/km",   "40.858",
             "Lap 121", "48.4 km",        1L, "01:44 min/km",   "41.406",
             "Lap 122", "48.8 km",        1L, "01:43 min/km",   "41.053",
             "Lap 123", "49.2 km",        1L, "01:42 min/km",   "40.927",
             "Lap 124", "49.6 km",        1L, "01:44 min/km",   "41.508",
             "Lap 125",   "50 km",        1L, "01:43 min/km",   "41.015",
             "Lap 126", "50.4 km",        1L, "01:42 min/km",   "40.802",
             "Lap 127", "50.8 km",        1L, "01:42 min/km",   "40.825",
             "Lap 128", "51.2 km",        1L, "01:40 min/km",   "40.194",
             "Lap 129", "51.6 km",        1L, "01:39 min/km",   "39.453",
             "Lap 130",   "52 km",        1L, "01:38 min/km",   "39.169",
             "Lap 130",   "52 km",        NA, "07:27 min/km", "2:58.701",
             "Lap 130",   "52 km",        NA, "12:33 min/km", "5:01.200",
             "Lap 130",   "52 km",        NA, "05:23 min/km", "2:09.353"
             )

# Addins - paste as tribble

# now have dataframe.
str(skatedf) #see what you have

#lots of character strings, lots of units junk

skatedf$lap_num <- as.numeric(str_replace(skatedf$LAP.NO., "Lap ", ""))

skatedf$km <- as.numeric(str_replace(skatedf$LAPS.KM., " km", ""))

skatedf$position <- as.numeric(skatedf$POSITION)

skatedf$pace <- ms(skatedf$PACE)

skatedf$lap_time <- as.numeric(skatedf$LAP.TIME)

# get rid of extra rows

skatedf <- skatedf[!is.na(skatedf$POSITION),]
str(skatedf)
skim(skatedf)

#plots
ggplot(skatedf, aes(x=lap_num, y=lap_time)) + geom_point()

ggplot(skatedf, aes(x=lap_num, y=lap_time)) + geom_line()

ggplot(skatedf, aes(x=km, y=position)) + geom_line()


