
#install.packages("tidycensus")
#install.packages("tidyverse")
library(tidycensus)
library(tidyverse)

state <- "TX"

variables <- c("B01001_001", "B01001_003", "B01001_027", "B01001_002", "B01003_001",
               "AREALAND", "B03002_001", "B03002_003", "B03002_004", "B03002_006",
               "B03002_005", "B03002_007", "B03002_008", "B03002_009", "B05002_001",
               "B05002_010", "B07001_003", "B07001_016", "B07001_019", "B07001_032",
               "B07001_035", "B07001_048", "B08012_001", "B08012_002", "B08012_004",
               "B08012_007", "B08012_012", "B08012_013", "B08301_001", "B08301_003",
               "B08301_004", "B08301_010", "B08301_018", "B08301_019", "B11001_001",
               "B11003_001", "B11005_001", "B11005_006", "B11005_007", "B11005_009",
               "B14001_001", "B14001_003", "B14002_001", "B14002_019", "B14002_022",
               "B14002_043", "B14002_046", "B14005_001", "B14005_012", "B14005_026",
               "B16001_001", "B16001_003", "B16009_001", "B16009_002", "B17001_001",
               "B17001_002", "B17001_004", "GEOID10",
               "B15002_011",  "B15002_018",  "B15002_028", "B15002_035", "B15002_001", "B19013_001",
              "B05002_019",  "C24010_026",  "C24010_063", "C24010_001", 'B15002_015',
              'B15002_032',
              'B15002_012',
             'B15002_014',
             'B15002_029',
             'B15002_031', 'C24010_003', 'C24010_040', 'C24010_029', 'C24010_066', 
             'C24010_030', 'C24010_067', 'C24010_033' , 'acs$C24010_070', 'C24010_070',
             'C24010_018' , 'C24010_055', 'C24010_001',
             'B27010_017', 'B27010_033', 'B27010_050' , 'B27010_066', 'B27010_001',
             'B23001_008', 'B23001_015', 'B23001_022', 'B23001_029', 'B23001_036', 'B23001_043',
             'B23001_050', 'B23001_057', 'B23001_064', 'B23001_071', 'B23001_076', 'B23001_081',
             'B23001_086', 'B23001_094', 'B23001_101', 'B23001_108', 'B23001_115', 'B23001_122',
             'B23001_129', 'B23001_136', 'B23001_143', 'B23001_150', 'B23001_157', 'B23001_162',
             'B23001_167', 'B23001_172','B23001_006', 'B23001_013', 'B23001_020', 'B23001_027', 'B23001_034', 'B23001_041',
             'B23001_048', 'B23001_055', 'B23001_062', 'B23001_069', 'B23001_074', 'B23001_079',
             'B23001_084', 'B23001_092', 'B23001_099', 'B23001_106', 'B23001_113', 'B23001_120',
             'B23001_127', 'B23001_134', 'B23001_141', 'B23001_148', 'B23001_155', 'B23001_160',
             'B23001_165', 'B23001_170',
             'B23001_007', 'B23001_014', 'B23001_021', 'B23001_028', 'B23001_035', 'B23001_042',
             'B23001_049', 'B23001_056', 'B23001_063', 'B23001_070', 'B23001_075', 'B23001_080',
             'B23001_085', 'B23001_093', 'B23001_100', 'B23001_107', 'B23001_114', 'B23001_121',
             'B23001_128', 'B23001_135', 'B23001_142', 'B23001_149', 'B23001_156', 'B23001_161',
             'B23001_166', 'B23001_171',
             'B23001_006', 'B23001_013', 'B23001_020', 'B23001_027', 'B23001_034', 'B23001_041',
             'B23001_048', 'B23001_055', 'B23001_062', 'B23001_069', 'B23001_074', 'B23001_079',
             'B23001_084', 'B23001_092', 'B23001_099', 'B23001_106', 'B23001_113', 'B23001_120',
             'B23001_127', 'B23001_134', 'B23001_141', 'B23001_148', 'B23001_155', 'B23001_160',
             'B23001_165', 'B23001_170'
             
              )


# Download data for the specified years
acs_data <- get_acs( geography = "County",
                                        variables = variables,
                                        state = state,
                                        survey = "acs5",
                                        year = 2015,
                                        geometry = F)
 
acs_data<-acs_data[c("GEOID", "variable", "estimate")]

#library(tidyr)

# Pivot the data to create a wide format data frame

acs <- acs_data %>%
  pivot_wider(names_from = variable, values_from = estimate)



#get in the texas gazeteer file, which has some of the geospatial data we need (like ALAND)

texas_gazeteer_file <- read.delim("C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/texas_gazeteer_file.txt")

texas_gazeteer_file$GEOID<-as.character(texas_gazeteer_file$GEOID)
acs<-merge(texas_gazeteer_file, acs, by="GEOID")



####################
# Create the new variables and store them in the "acs" data frame

acs$pop_lf <- acs$B01003_001
acs$popden_km <- acs$B01003_001 / (acs$ALAND * 0.000001)
acs$popden_mi <- acs$B01003_001 / (acs$ALAND * 0.0000003861022)

acs$race_hisp <- acs$B03002_009 / acs$B03002_001
acs$race_whiteNH <- acs$B03002_003 / acs$B03002_001
acs$race_blackNH <- acs$B03002_004 / acs$B03002_001
acs$race_asianNH <- acs$B03002_006 / acs$B03002_001
acs$race_otherNH <- (acs$B03002_005 + acs$B03002_007 + acs$B03002_008) / acs$B03002_001
acs$race_multipleNH <- acs$B03002_009 / acs$B03002_001
acs$birth_foreign <- acs$B05002_010 / acs$B05002_001
acs$birth_puertorico <- acs$B05002_019 / acs$B05002_001
acs$samehouse <- (sum(acs$B07001_019 - acs$B07001_032)) / (sum(acs$B07001_003 - acs$B07001_016))

acs$Educ_minHS <- ((sum(acs$B15002_011 - acs$B15002_018)) + (sum(acs$B15002_028 - acs$B15002_035))) / acs$B15002_001
acs$Educ_HSonly <- (acs$B15002_011 + acs$B15002_028) / acs$B15002_001
acs$Educ_minBA <- ((sum(acs$B15002_015 - acs$B15002_018)) + (sum(acs$B15002_032 - acs$B15002_035))) / acs$B15002_001
acs$Educ_somecoll <- ((sum(acs$B15002_012 - acs$B15002_014)) + (sum(acs$B15002_029 - acs$B15002_031))) / acs$B15002_001
acs$pov_allage<- acs$B17001_001/acs$B17001_001
acs$inc_medHH=acs$B19013_001

employed <- sum(acs$B23001_007, acs$B23001_014, acs$B23001_021, acs$B23001_028, acs$B23001_035, acs$B23001_042,
                acs$B23001_049, acs$B23001_056, acs$B23001_063, acs$B23001_070, acs$B23001_075, acs$B23001_080,
                acs$B23001_085, acs$B23001_093, acs$B23001_100, acs$B23001_107, acs$B23001_114, acs$B23001_121,
                acs$B23001_128, acs$B23001_135, acs$B23001_142, acs$B23001_149, acs$B23001_156, acs$B23001_161,
                acs$B23001_166, acs$B23001_171) /
  sum(acs$B23001_006, acs$B23001_013, acs$B23001_020, acs$B23001_027, acs$B23001_034, acs$B23001_041,
      acs$B23001_048, acs$B23001_055, acs$B23001_062, acs$B23001_069, acs$B23001_074, acs$B23001_079,
      acs$B23001_084, acs$B23001_092, acs$B23001_099, acs$B23001_106, acs$B23001_113, acs$B23001_120,
      acs$B23001_127, acs$B23001_134, acs$B23001_141, acs$B23001_148, acs$B23001_155, acs$B23001_160,
      acs$B23001_165, acs$B23001_170)

acs$unemployed <- sum(acs$B23001_008, acs$B23001_015, acs$B23001_022, acs$B23001_029, acs$B23001_036, acs$B23001_043,
                  acs$B23001_050, acs$B23001_057, acs$B23001_064, acs$B23001_071, acs$B23001_076, acs$B23001_081,
                  acs$B23001_086, acs$B23001_094, acs$B23001_101, acs$B23001_108, acs$B23001_115, acs$B23001_122,
                  acs$B23001_129, acs$B23001_136, acs$B23001_143, acs$B23001_150, acs$B23001_157, acs$B23001_162,
                  acs$B23001_167, acs$B23001_172) /
  sum(acs$B23001_006, acs$B23001_013, acs$B23001_020, acs$B23001_027, acs$B23001_034, acs$B23001_041,
      acs$B23001_048, acs$B23001_055, acs$B23001_062, acs$B23001_069, acs$B23001_074, acs$B23001_079,
      acs$B23001_084, acs$B23001_092, acs$B23001_099, acs$B23001_106, acs$B23001_113, acs$B23001_120,
      acs$B23001_127, acs$B23001_134, acs$B23001_141, acs$B23001_148, acs$B23001_155, acs$B23001_160,
      acs$B23001_165, acs$B23001_170)
 
acs$Occup_I<-(acs$C24010_003 + acs$C24010_040)/acs$C24010_001                  
acs$Occup_II=(acs$C24010_018 + acs$C24010_055)/ acs$C24010_001
acs$Occup_III <- (acs$C24010_026 + acs$C24010_063) / acs$C24010_001
acs$Occup_IV <- (acs$C24010_029 + acs$C24010_066) / acs$C24010_001
acs$Occup_V <- (acs$C24010_030 + acs$C24010_067) / acs$C24010_001
acs$Occup_VI <- (acs$C24010_033 + acs$C24010_070) / acs$C24010_001
acs$Occup_nonManagement <- (acs$Occup_II + acs$Occup_III + acs$Occup_IV + acs$Occup_V 
                            + acs$Occup_VI)
acs$insur_no_allage=(acs$B27010_017+acs$B27010_033+acs$B27010_050+acs$B27010_066)/acs$B27010_001

# keep only variables we need

acs_tx<-acs[c(1:5,7,178:204)]

names(acs_tx)

#assigning these data to year 2013 because this is the mid-point of the ACS years
acs_tx$year=2013

###################
setwd("C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data/gridmet_meteorological")

library(dplyr)
library(lubridate)

file_names <- list.files()
library(data.table)

# Empty list to store data frames
data_list <- list()

# Loop through the files and read them into data frames
for (file_name in file_names) {
  data_list[[file_name]] <- fread(file_name)
}

first_df <- data_list[[1]]

all<-rbind(data_list[[1]], data_list[[2]], data_list[[3]], data_list[[4]],
           data_list[[5]], data_list[[6]], data_list[[7]], 
           data_list[[8]], data_list[[9]])

all$date<-ymd(all$date_ymd)
all$month<-month(all$date)
all$year<-year(all$date)

# convert the temperature variables from kelvin to celsius

all$tmmn_c<-all$tmmn-273.15
all$tmmx_c<-all$tmmx-273.15


# Calculate the mean values for 'pr', 'tmmn', and 'tmmx' for each unique combination of 'GEOID', 'month', and 'year'
monthly_avg <- all %>%
  group_by(GEOID, month, year) %>%
  summarise(average_pr = mean(pr),
            average_tmmn = mean(tmmn_c),
            average_tmmx = mean(tmmx_c))

dallas<-monthly_avg[which(monthly_avg$GEOID=='48113'),]

dallas_monthly<-dallas


### IDENTIFY the months where there was at least one day with temperatures above 80 degrees F


#############################################################

# Now get in the Bureau of labor statistics data
setwd("C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data/BLS_industryemploymentdata")

#names<-list.files()

# List of zipped folders (replace 'folder1.zip', 'folder2.zip', etc., with your actual zip file names)
#zip_files <- c(names[1:10])

# Unzip each file one by one
#for (zip_file in zip_files) {
 # unzip(zip_file)
#}


#names<-list.files()

# Load necessary packages
library(readxl)
library(dplyr)
library(tidyr)

# List of file names
file_names <- dataframe_list <- c(
  paste0("allhlcn", sprintf("%02d", 131), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 141), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 151), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 161), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 171), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 181), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 191), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 201), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 211), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 221), ".xlsx")
)

test<-read_excel("allhlcn161.xlsx")
# Create an empty data frame to store the combined data
combined_df <- data.frame()

# Loop through each file and combine the data using rbind
for (file_name in file_names) {
  # Read the data from the Excel file
  data <- read_excel(file_name)
  
  # Filter rows for "Construction" and "Manufacturing" industries
  data <- data %>% filter(NAICS %in% c("1012", "1013") )
  
  #data<- subset(data, Ownership=="Total Covered")
  
  data<-data[c("St", "Cnty", "Ownership", "Year", "NAICS", "January Employment", "February Employment", "March Employment")]
  
  # Reshape the data frame from wide to long format
  data_long <- pivot_longer(data, cols = c("January Employment", "February Employment", "March Employment"), 
                            names_to = "Month", values_to = "Employment")
  
  # Convert the Month column to a factor with custom levels
  data_long$Month <- factor(data_long$Month, levels = c("January Employment", "February Employment", "March Employment"),
                            labels = c("January", "February", "March"))
  
  # Combine the data frames using rbind
  combined_df <- rbind(combined_df, data_long)
}

# Print the combined data frame
print(combined_df)

quarter_one<-combined_df

### Quarter two
# List of file names
file_names <- dataframe_list <- c(
  paste0("allhlcn", sprintf("%02d", 132), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 142), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 152), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 162), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 172), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 182), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 192), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 202), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 212), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 222), ".xlsx")
)

# Create an empty data frame to store the combined data
combined_df <- data.frame()

# Loop through each file and combine the data using rbind
for (file_name in dataframe_list) {
  # Read the data from the Excel file
  data <- read_excel(file_name)
  
  # Filter rows for "Construction" and "Manufacturing" industries
  data <- data %>% filter(NAICS %in% c("1012", "1013"))
  
  data<-data[c("St", "Cnty", "Ownership", "Year", "NAICS", "April Employment", "May Employment", "June Employment")]
  
  # Reshape the data frame from wide to long format
  data_long <- pivot_longer(data, cols = c("April Employment", "May Employment", "June Employment"), 
                            names_to = "Month", values_to = "Employment")
  
  # Convert the Month column to a factor with custom levels
  data_long$Month <- factor(data_long$Month, levels = c("April Employment", "May Employment", "June Employment"),
                            labels = c("April", "May", "June"))
  
  # Combine the data frames using rbind
  combined_df <- rbind(combined_df, data_long)
}

# Print the combined data frame
print(combined_df)

quarter_two<-combined_df

###### Quarter three ###################

# List of file names
file_names <- dataframe_list <- c(
  paste0("allhlcn", sprintf("%02d", 133), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 143), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 153), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 163), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 173), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 183), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 193), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 203), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 213), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 223), ".xlsx")
)

# Create an empty data frame to store the combined data
combined_df <- data.frame()

# Loop through each file and combine the data using rbind
for (file_name in dataframe_list) {
  # Read the data from the Excel file
  data <- read_excel(file_name)
  
  # Filter rows for "Construction" and "Manufacturing" industries
  data <- data %>% filter(NAICS %in% c("1012", "1013"))
  
  data<-data[c("St", "Cnty", "Ownership", "Year", "NAICS", "July Employment", "August Employment", "September Employment")]
  
  # Reshape the data frame from wide to long format
  data_long <- pivot_longer(data, cols = c("July Employment", "August Employment", "September Employment"), 
                            names_to = "Month", values_to = "Employment")
  
  # Convert the Month column to a factor with custom levels
  data_long$Month <- factor(data_long$Month, levels = c("July Employment", "August Employment", "September Employment"),
                            labels = c("July", "August", "September"))
  
  # Combine the data frames using rbind
  combined_df <- rbind(combined_df, data_long)
}

# Print the combined data frame
print(combined_df)

quarter_three<-combined_df

#######Quarter four #######################

# List of file names
file_names <- dataframe_list <- c(
  paste0("allhlcn", sprintf("%02d", 134), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 144), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 154), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 164), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 174), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 184), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 194), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 204), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 214), ".xlsx"),
  paste0("allhlcn", sprintf("%02d", 224), ".xlsx")
)

# Create an empty data frame to store the combined data
combined_df <- data.frame()

# Loop through each file and combine the data using rbind
for (file_name in dataframe_list) {
  # Read the data from the Excel file
  data <- read_excel(file_name)
  
  # Filter rows for "Construction" and "Manufacturing" industries
  data <- data %>% filter(NAICS %in% c("1012", "1013"))
  
  data<-data[c("St", "Cnty", "Ownership", "Year", "NAICS", "October Employment", "November Employment", "December Employment")]
  
  # Reshape the data frame from wide to long format
  data_long <- pivot_longer(data, cols = c("October Employment", "November Employment", "December Employment"), 
                            names_to = "Month", values_to = "Employment")
  
  # Convert the Month column to a factor with custom levels
  data_long$Month <- factor(data_long$Month, levels = c("October Employment", "November Employment", "December Employment"),
                            labels = c("October", "November", "December"))
  
  # Combine the data frames using rbind
  combined_df <- rbind(combined_df, data_long)
}

# Print the combined data frame
print(combined_df)

quarter_four<-combined_df

#########################################################################

all_labor<-rbind(quarter_one, quarter_two, quarter_three, quarter_four)

setwd('C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data')

write.csv(all_labor, "dol_manufact_construct_bycounty.csv")


#subset to dallas tx

tx_labor<-subset(all_labor, St=='48')

dallas_labor<-subset(tx_labor, Cnty=='057')

############################

#Merge labor counts with the monthly meteorological data

df1<-dallas_labor
df2<-dallas_monthly

df2$month <- ifelse(df2$month == "1", "January",
                    ifelse(df2$month == "2", "February",
                           ifelse(df2$month == "3", "March",
                                  ifelse(df2$month == "4", "April",
                                         ifelse(df2$month == "5", "May",
                                                ifelse(df2$month == "6", "June",
                                                       ifelse(df2$month == "7", "July",
                                                              ifelse(df2$month == "8", "August",
                                                                     ifelse(df2$month == "9", "September",
                                                                            ifelse(df2$month == "10", "October",
                                                                                   ifelse(df2$month == "11", "November",
                                                                                          ifelse(df2$month == "12", "December", "Invalid Month"))))))))))))
                                                                                   
# For df1
df1$YearMonth <- paste(df1$Year, df1$Month, sep = "-")

# For df2
df2$YearMonth <- paste(df2$year, df2$month, sep = "-")


merged_df <- merge(df2, df1, by = "YearMonth")
names(merged_df)

merged_df<-merged_df[c("GEOID"     ,   "month"  ,      "year"       ,  "average_pr"   ,"average_tmmn",
 "average_tmmx" ,"St"    ,       "Cnty"   ,      "Ownership" ,"NAICS", "Employment")]

# get in the workers comp data
setwd("C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data")
wc<-read_excel("workerscomp_texas.xlsx")

#construction=1012

#manufacturing=1013

wc$NAICS<-ifelse(wc$industry=="Construction", "1012",
                 ifelse(wc$industry=="Manufacturing", "1013", NA))
#wc: 504x8
#merged_df: 216 obs. of  15 variables


wc$industry_YearMonth <- paste(wc$NAICS, wc$year, wc$month, sep = "-")

wc<-wc[which(wc$county=="Dallas"),]


merged_df$industry_YearMonth<-paste(merged_df$NAICS, merged_df$year, merged_df$month, sep='-')

all<-merge(wc, merged_df, by="industry_YearMonth", all.x=T)
                 
names(all)

dallas<-all[c("month.x"    ,        "n_claims" ,        
  "year.x"      ,       "county"      ,       "industry" ,         
       "NAICS.x"  ,          "GEOID" , "average_pr"  ,      
  "average_tmmn"   ,    "average_tmmx"    ,"Employment" )]

dallas$injury_rate<-(dallas$n_claims/dallas$Employment)*1000



#plot 
df<-dallas
# Convert month.x and year.x to Date format

library(zoo)

df$year<-as.character(df$year.x)
# Convert month to a factor with ordered levels to ensure correct ordering in the plot
df$month <- factor(df$month.x, levels = c("January", "February", "March", "April", "May", "June",
                                          "July", "August", "September", "October", "November", "December"),
                   ordered = TRUE)


# Create the date variable using zoo::yearmon function
df$date <- as.Date(as.yearmon(paste(df$year.x, df$month.x), format = "%Y %B"))
# Convert the date when the policy was implemented to yearmon format
policy_date <- as.Date(as.yearmon("2015 June", format = "%Y %B"))

df_sub<-subset(df, year %in% c(2013:2016))
df_sub<-subset(df_sub, month %in% c("May", "June", "July", "August", "September"))



# Create the time series plot
ggplot(df, aes(x = date, y = n_claims, color = industry)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(policy_date), linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Rate of Claims", color = "Industry") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme_minimal()


# Create the time series plot with secondary y-axis for average_tmmx
ggplot(df, aes(x = date, color = industry)) +
  geom_line(aes(y = n_claims), size = 1) +
  geom_line(aes(y = average_tmmx), size = 1, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date(policy_date)), linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Number of Claims", color = "Industry") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(name = "Number of Claims", sec.axis = sec_axis(~., name = "Average Max Temperature")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################



# Create the plot
p <- ggplot(df, aes(x = date, color = industry)) +
  geom_line(aes(y = n_claims), size = 1) +
  geom_line(aes(y = average_tmmx), size = 1, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(as.Date(policy_date)), linetype = "dashed", color = "red") +
  labs(x = "Date", y = "Number of Claims", color = "Industry") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(name = "Number of Claims", sec.axis = sec_axis(~., name = "Average Max Temperature")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot with increased width
ggsave("dallas_claimratesbymonth.png", plot = p, width = 12, height = 6)








