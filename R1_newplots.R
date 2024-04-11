
# CODE TO PRODUCE TABLE 1, FIGURE 1, and Supplemental Figure 1

setwd("C:/Users/lhs36/OneDrive - Drexel University/JPB_WORK/Texas/Data")

# read in the annual claims data. This was prepared at the bottom of the code file titled v2.texas_heatinjury_data_prep_withTarrant.R
data<-read.csv('dallas_tarrant_annual_claimsemployment.csv')

#Prep the data a little bit

#create a time in study variable

# Create a named vector to map month names to numeric values

data <- data %>%
  mutate(
    year = Year,
    pre_post = if_else(year >= 2016, 1, 0)
  ) %>%
  filter(county %in% c("Dallas",  "Tarrant"))


#adjust the total employed number because not all employers provided workers compensation 

#use the following estimates:
#2012: 81%
#2014: 80%
#2016: 82%
#2018: 82%

#data come from the figure labeled: "Percentage of Texas Private
#Sector Employers That Are
#Subscribers and Texas Employees Employed by Subscribers:
# 1993 2018
#in the following document: https://www.tdi.texas.gov/reports/wcreg/documents/nonsub2022.pdf

data$adj_employ<-ifelse(data$year==2013, (data$Employment*(0.805)),
                        ifelse( data$year==2014, (data$Employment*(0.80)),
                                ifelse(data$year==2015, (data$Employment*(0.81)),
                                       ifelse(data$year==2016, (data$Employment*(0.81)),
                                              ifelse(data$year==2017, (data$Employment*(0.82)),
                                                     ifelse(data$year==2018, (data$Employment*(0.82)),
                                                            ifelse(data$year==2019, (data$Employment*(0.82)),
                                                                   NA)))))))
#subset to before year 2019
data<-subset(data, year<2019)

#keep only the variables we need, to keep things simpler and easier to read 

data<-data[c("year", "pre_post", "adj_employ", "n_claims", "county", "average_tmean" )]


data$intervention_group<-ifelse(data$county=="Dallas", 1,0)

#create a time in study variable 

# Sort the data by the Date variable (if not already sorted)
data <- data %>% arrange(year)

# Calculate the time variable incrementally from 1 to xxx
data <- data %>% 
  group_by(county) %>% 
  mutate(time_since_start=row_number())


#minus one so time since start begins at zero
data$T = data$time_since_start - 1

#minus 3 because we want all dates prior to January 2016 to be coded as 0, 
#and we want incrementally increasing values for time (year) since intervention start past this point

data$time_since_intervention = ifelse(data$pre_post==1, data$T - 3,0)


data$rate<-(data$n_claims/data$adj_employ)*1000
data$group<-ifelse(data$county=="Dallas",1,
                   ifelse(data$county=="Tarrant",2,3))
data$year<-as.numeric(data$year)    
data$time_since_start<-as.numeric(data$time_since_start)

# FOR TABLE 1: RATES BY COUNTY & YEAR

tab1<-data[c("county", "year", "rate", "n_claims", "adj_employ", "average_tmean")]

dallas_tab1<-subset(tab1, county=="Dallas")

tarrant_tab1<-subset(tab1, county=="Tarrant")


colnames(tarrant_tab1)<-c("County", "Year", "Tarrant","total claims" ,"total employed", "mean temp")
colnames(dallas_tab1)<-c("County", "Year", "Dallas", "total claims", "total employed", "mean temp")

tab1_a<-as.data.frame(cbind(dallas_tab1, tarrant_tab1))

#write.csv(tab1_a, "PATH/R1.Table1.csv")

####################SUPPLEMENTAL FIGURE 1
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11))

Figure1 <- ggplot(data=subset(data, group==1), aes(x=time_since_start, y=rate)) +
  theme_classic() +
  geom_point(aes(color = "Dallas Construction", shape = "Dallas Construction")) +
  geom_line(aes(group = "Dallas Construction", linetype = "Dallas Construction"), color = "blue") +
  geom_point(data = subset(data, group==2), aes(x = time_since_start, y = rate, color = "Tarrant Construction", shape = "Tarrant Construction")) +
  geom_line(data = subset(data, group==2), aes(x = time_since_start, y = rate, group = "Tarrant Construction", linetype = "Tarrant Construction"), color = "red") +
  #geom_point(data = subset(data, group==3), aes(x = time_since_start, y = rate, color = "Dallas Manufacturing", shape = "Dallas Manufacturing")) +
  # geom_line(data = subset(data, group==3), aes(x = time_since_start, y = rate, group = "Dallas Manufacturing", linetype = "Dallas Manufacturing"), color = "green") +
  geom_vline(xintercept = 4, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle=45, hjust=0.8, vjust=0.8, size=11), axis.ticks = element_blank()) +
 
  scale_x_continuous('Year', labels=seq(2013, 2018, by=1), seq(1, 6, by = 1), expand=c(0,0.5)) +
  scale_y_continuous('Claims per 1000 construction employees', limits = c(0, 20)) +
 # scale_y_continuous('Claims per 1000 construction employees') +
  labs(color = "Group", shape = "Group", linetype = "Group") +
  scale_color_manual(values = c("Dallas Construction" = "blue", "Tarrant Construction" = "red")) +
  scale_shape_manual(values = c("Dallas Construction" = 16, "Tarrant Construction" = 17)) +
  scale_linetype_manual(values = c("Dallas Construction" = "solid", "Tarrant Construction" = "dotted")) +
  guides(shape = guide_legend(override.aes = list(color = c("blue", "red"))))

#ggsave("PATH/20230314_observedratesplotted_dallastarrant.png", plot =Figure1 , width = 10, height = 8)

# FIGURE 1 : LOESS
Figure1_LOESS <- ggplot(data=subset(data, group==1), aes(x=time_since_start, y=rate)) +
  theme_classic() +
  geom_point(aes(color = "Dallas Construction", shape = "Dallas Construction")) +
  geom_smooth(method = "loess", se = FALSE, aes(group = "Dallas Construction", linetype = "Dallas Construction"), color = "blue") +
  geom_point(data = subset(data, group==2), aes(x = time_since_start, y = rate, color = "Tarrant Construction", shape = "Tarrant Construction")) +
  geom_smooth(data = subset(data, group==2), method = "loess", se = FALSE, aes(x = time_since_start, y = rate, group = "Tarrant Construction", linetype = "Tarrant Construction"), color = "red") +
  
  geom_vline(xintercept = 4, linetype = "dashed", color = "black") +
  theme(axis.text.x = element_text(angle=45, hjust=0.8, vjust=0.8, size=11), axis.ticks = element_blank()) +
  
  scale_x_continuous('Year', labels=seq(2013, 2018, by=1), seq(1, 6, by = 1), expand=c(0,0.5)) +
  scale_y_continuous('Claims per 1000 construction employees', limits = c(0, 20)) +
  
  #scale_y_continuous('Claims per 1000 construction employees') +
  labs(color = "Group", shape = "Group", linetype = "Group") +
  scale_color_manual(values = c("Dallas Construction" = "blue", "Tarrant Construction" = "red")) +
  scale_shape_manual(values = c("Dallas Construction" = 16, "Tarrant Construction" = 17)) +
  scale_linetype_manual(values = c("Dallas Construction" = "solid", "Tarrant Construction" = "dotted")) +
  guides(shape = guide_legend(override.aes = list(color = c("blue", "red"))))


#ggsave("PATH/20230314_Loessobservedratesplotted_harrisdallas.png", plot =Figure1_LOESS , width = 10, height = 6)

