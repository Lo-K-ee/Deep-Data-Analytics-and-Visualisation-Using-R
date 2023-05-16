############## PACKAGES ################

library(tidyverse)
library(ggrepel)
library(tidyr)
library(sf)
library(geojsonsf)
library(rgdal)
library(Rcpp)
library(ggmap)
library(leaflet)
library(gridExtra)
library(plotly)
library(readr)

############## DATA FETCH #################

setwd("C:/Files/UoE/Modules/Spring/MA304-7-SP Data Visualisation")
data_304 <- read.csv("37-00049_UOF-P_2016_prepped.csv")

############## DATA MANIPULATION ################

data_304 <- data_304[-c(1), ] # removing first row as it repeats the title
data_304$INCIDENT_TIME = format(strptime(data_304$INCIDENT_TIME, "%I:%M:%S %p"), format="%H:%M:%S") # converting the 12hr timeformat to 24hr format
data_304$INCIDENT_TIME = as.POSIXct(data_304$INCIDENT_TIME, format="%H:%M:%S") # converting the text into datetime format 
data_304$houroftheday = cut(data_304$INCIDENT_TIME, breaks="1 hour") #getting the hour of the day and storing it in a new column
data_304$day <- weekdays(as.Date(data_304$INCIDENT_DATE, format= "%m/%d/%y")) #getting the days and creating a new column as day
data_304$day <- ordered(data_304$day, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) #defining the order of days
data_304$month <- months(as.Date(data_304$INCIDENT_DATE, format = "%m/%d/%y")) #getting the months and creating a new column as month
data_304$month <- ordered(data_304$month, levels= c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
data_304$INCIDENT_DATE <- as.Date(data_304$INCIDENT_DATE, format = "%m/%d/%Y") #converting the date in %m/%d/%y format 
data_304$INCIDENT_DATE <- gsub("00", "20",data_304$INCIDENT_DATE) #as the year has 0016, converting substituting them with "20"
data_304$INCIDENT_DATE <- as.Date(data_304$INCIDENT_DATE, format = "%Y-%m-%d") #seperating the dates with -
data_304$monthnum <- format(data_304$INCIDENT_DATE, "%m") #months in number
data_304$OFFICER_YEARS_ON_FORCE <- as.numeric(data_304$OFFICER_YEARS_ON_FORCE)

# Grouping years, months and days

years_dat <- data_304 %>%
  group_by(date = INCIDENT_DATE, month, day) %>%
  summarize(counts = n())

months_dat <- data_304 %>%
  group_by(month = (format(data_304$INCIDENT_DATE, "%m"))) %>%
  summarize(counts = n())

days_dat <- data_304 %>%
  group_by(day = wday(data_304$INCIDENT_DATE, label = TRUE)) %>%
  summarize(counts = n())

hours_dat <- data_304 %>%
  group_by(hour=(hour(data_304$houroftheday))) %>%
  summarize(average = n())

############## Two way Table ############################

table(data_304$SUBJECT_RACE)
SubjPerRace <- data_304 %>%
  group_by(SUBJECT_RACE, INCIDENT_REASON) %>%
  summarise(count = n()) %>% 
  mutate(prop = count / sum(count)*100) %>%
  filter(SUBJECT_RACE != "NULL")
print("The tables depicts the proportion of incidents per subject's race. We can interpret that the cases are most among the blacks and 
      the table also explains the relative frequency of incidents across different races")
SubjPerRace

############# Dot Plot ##################################

OffiSubjRace <- data_304 %>%
  count(OFFICER_RACE,SUBJECT_RACE) %>%
  filter(SUBJECT_RACE != "NULL", SUBJECT_RACE != "Other")
print("The below plot depicts the relation between the officer's race and the subject's. This can provide insights into the potential 
      racial disparities in the use of force by law enforcement.")
ggplot(OffiSubjRace, aes(OFFICER_RACE, SUBJECT_RACE, size = n)) + 
  geom_point(color = "red") +
  geom_text(aes(label=n), vjust = 2) +
  labs(title = "Officer's race vs Subject's", x = "Officer's Race", y = "Subject's Race", size = "n") +
  theme(legend.position = "none")

########################## EXTRAS - Injuries #########################

ofcr_expr <- data_304 %>%
  filter(OFFICER_INJURY == "Yes") %>%
  arrange(OFFICER_YEARS_ON_FORCE) %>%
  group_by(OFFICER_YEARS_ON_FORCE, OFFICER_INJURY) %>%
  summarise(counts = n())
  
ofcr_expr_no <- data_304 %>%
  filter(OFFICER_INJURY == "No") %>%
  arrange(OFFICER_YEARS_ON_FORCE) %>%
  group_by(OFFICER_YEARS_ON_FORCE, OFFICER_INJURY) %>%
  summarise(counts = n())

ggplot(ofcr_expr, aes(x = OFFICER_YEARS_ON_FORCE, y = counts)) +
  geom_point() +
  geom_text(aes(label= counts), vjust = -1) +
  labs(x = "Officer Years of Service", y = "Officer Injury") +
  ggtitle("Officers' amount of injuries with their years of experience")

ggplot(ofcr_expr_no, aes(x = OFFICER_YEARS_ON_FORCE, y = counts)) +
  geom_point() +
  geom_text(aes(label= counts), vjust = -1) +
  labs(x = "Officer Years of Service", y = "Officer Injury") +
  ggtitle("Officers with no history of injuries")

############# PIE CHART (Gender~Race) ####################

subgender_race <- data_304 %>%
  filter(SUBJECT_RACE != "NULL", SUBJECT_GENDER != "Unknown", SUBJECT_GENDER != "NULL") %>%
  group_by(SUBJECT_GENDER, SUBJECT_RACE) %>%
  summarize(counts = n())

spie_M <- subgender_race %>%
  filter(SUBJECT_GENDER == "Male") %>%
  ggplot(aes(x="", y=counts, fill=SUBJECT_RACE)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(aes(x = 1, label = paste0(counts)), nudge_x = 0.8, size = 4, show.legend = FALSE, box.padding = 1) +
  labs(title = "Male Subject's Race", fill = "Race") +
  scale_fill_brewer(palette = "Set1") +
  theme_void() 

spie_F <- subgender_race %>%
  filter(SUBJECT_GENDER == "Female") %>%
  ggplot(aes(x="", y=counts, fill=SUBJECT_RACE)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(aes(x = 1, label = paste0(counts)), nudge_x = 0.8, size = 4, show.legend = FALSE, box.padding = 1) +
  labs(title = "Female Subject's Race", fill = "Race") +
  scale_fill_brewer(palette = "Set1") +
  theme_void()

grid.arrange(spie_M, spie_F, nrow=2, ncol=1)

ofcrgender_race <- data_304 %>%
  filter(OFFICER_RACE != "NULL", OFFICER_GENDER != "Unknown", OFFICER_GENDER != "NULL") %>%
  group_by(OFFICER_GENDER, OFFICER_RACE) %>%
  summarize(counts = n())
  
opie_M <- ofcrgender_race %>%
  filter(OFFICER_GENDER == "Male") %>%
  ggplot(aes(x="", y=counts, fill=OFFICER_RACE)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(aes(x = 1, label = paste0(counts)), nudge_x = 0.8, size = 4, show.legend = FALSE, box.padding = 1) +
  labs(title = "Male Officer's Race", fill = "Race") +
  scale_fill_brewer(palette = "Set1") +
  theme_void() 

opie_F <- ofcrgender_race %>%
  filter(OFFICER_GENDER == "Female") %>%
  ggplot(aes(x="", y=counts, fill=OFFICER_RACE)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_label_repel(aes(x = 1, label = paste0(counts)), nudge_x = 0.8, size = 4, show.legend = FALSE, box.padding = 1) +
  labs(title = "Female Officer's Race", fill = "Race") +
  scale_fill_brewer(palette = "Set1") +
  theme_void()

grid.arrange(opie_M, opie_F, nrow=2, ncol=1)


############# Histogram ##############################

print("Now checking the crime occurences in Dallas. As we can interpret from the plot that most of the cases were from the Central division area of Dallas.
      Mainly focusing on the black subjects' crime rates as well.")

crimePerLoc1 <- table(data_304$DIVISION)
crimePerLoc1 <- as.data.frame(crimePerLoc1)
hist1 <- crimePerLoc1 %>%
  ggplot(aes(Var1, Freq, group = 1)) + 
    geom_histogram(stat = "identity", fill = "lightblue", color = "black", bins = 5) +
    xlab("Division in Dallas") + ggtitle("Overall crime rates in Dallas") + ylab("crime counts") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    geom_text(aes(label = Freq), vjust = -0.5)

crimePerLoc <- data_304 %>%
  filter(SUBJECT_RACE != "NULL") %>%
  group_by(DIVISION, SUBJECT_RACE) %>%
  summarize(counts = n())

black_data <- crimePerLoc %>% filter(SUBJECT_RACE == "Black")
hist2 <- ggplot(black_data, aes(x=DIVISION, y=counts, fill = DIVISION)) +
  geom_histogram(data=black_data, aes(fill = "Black"), stat = "identity", alpha = 0.7, fill = "black", color = "#ff7e76") +
  labs(title = "Crime counts by black subjects", x="Division in Dallas", y ="crime counts") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_text(aes(label = counts), vjust = -0.5)

grid.arrange(hist1, hist2, nrow=1, ncol=2)

############## BOX PLOT ####################

box1 <- ggplot(crimePerLoc, aes(x = SUBJECT_RACE, y = counts, fill = SUBJECT_RACE)) +
  geom_boxplot() +
  labs(x="Subject Race", y="crime counts", title="Crime occurrences per subject's race")

black_reason <- data_304 %>%
  filter(SUBJECT_RACE == "Black") %>%
  group_by(INCIDENT_DATE, Months = monthnum, SUBJECT_RACE) %>%
  summarize(average = n())
  
box2 <- ggplot(black_reason, aes(x = Months, y=average, fill = Months)) +
  geom_boxplot() +
  labs(title = "Incident rate across black subjects in 2016", y = " Incident rate") +
  scale_x_discrete(labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(legend.position = "none")

grid.arrange(box1, box2, nrow=2, ncol=1)

############# Correlation Analysis ####################
############## SCATTER PLOT ####################

correlation <- cor(as.numeric(as.factor(data_304$INCIDENT_REASON)), as.numeric(as.factor(data_304$REASON_FOR_FORCE)))
cat("Correlation between incident reason & reason for force:", correlation)

data_304 %>%
  filter(INCIDENT_REASON != "NULL", REASON_FOR_FORCE != "NULL") %>%
  ggplot(aes(x = as.factor(INCIDENT_REASON), y = as.factor(REASON_FOR_FORCE))) +
  geom_point() +
  labs(x="Incident Reason", y="Reason for Force", title="Scatter plot between Incident Reason & Reason for Force") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

############# Time Series Analysis ####################

print("Time Series Analysis")
timeplot1 <- ggplot(data = subset(hours_dat, !is.na(hour)), aes(x=hour, y=average)) + 
  geom_line(linewidth = 1, color = "indianred") +
  labs(x = "Hour of the day", y = "Crimes", title = "Hourly Crime rate")
timeplot2 <- ggplot(days_dat, aes(x=day, y=counts, group=1)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(x = "Day", y = "Crimes", title = "Daily Crime rate") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
timeplot3 <- ggplot(months_dat, aes(month, counts, group=1)) +
  geom_line(linewidth = 1, color = "green") +
  scale_x_discrete(labels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "Month", y = "Crimes", title = "Monthly Crime rate") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
grid.arrange(timeplot1, timeplot2, timeplot3, ncol = 2, nrow = 2)

############# Smoothing pattern/trend ##################

pattern <- ggplot(years_dat, aes(date, counts)) +
  geom_line(size = 1, col = "bisque3") +
  geom_smooth(method = "loess", color = "blue3", span = 1/4, size = 1.2) +
  labs(title = "Occurence of Crimes over the year", x = "Months", y = "Crimes") +
  theme_minimal()
pattern

############# Map ######################

print("Map without using shapefile")

latitude <- 32.774959
longitude <- -96.802976
gleft <- longitude - 0.3
gbottom <- latitude - 0.2
gright <- longitude + 0.3
gtop <- latitude + 0.2

bbox <- c(gleft,gbottom,gright,gtop)
map <- get_stamenmap(bbox, zoom = 11)
dallas_map <- ggmap(map)

crimeAreaBlack <- data_304%>%
  group_by(SUBJECT_RACE, LOCATION_LONGITUDE, LOCATION_LATITUDE) %>%
  filter(SUBJECT_RACE == "Black")

dallas_map +
  geom_point(aes(x = as.numeric(data_304$LOCATION_LONGITUDE), y = as.numeric(data_304$LOCATION_LATITUDE), size = 1, alpha = 0.6, color = "Remaining subjects"), data = data_304) +
  geom_point(aes(x = as.numeric(crimeAreaBlack$LOCATION_LONGITUDE), y = as.numeric(crimeAreaBlack$LOCATION_LATITUDE), size = 1, alpha = 0.6, color = "Black subjects"), data = crimeAreaBlack) +
  labs(title = "Crime zones in Dallas", x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom")

############# Interactive plots ###############################

subject_desc <- data_304 %>%
  filter(SUBJECT_RACE != "NULL", SUBJECT_RACE != "Other", SUBJECT_DESCRIPTION != "NULL") %>%
  group_by(SUBJECT_RACE, subject_desc = SUBJECT_DESCRIPTION) %>%
  summarize(counts = n())

plot_ly(subject_desc, x = ~SUBJECT_RACE, y = ~subject_desc, text = ~counts, type = "scatter", mode = "markers", color = ~counts, colors = "YlOrRd",
        marker = list(size = ~counts/5, opacity = 0.5)) %>%
          layout(title = "Description of the subject with race",
          xaxis = list(title = "SUBJECT_RACE"),
          yaxis = list(title = "SUBJECT_DESCRIPTION"))

######################### EXTRAS ############################

sunday_crimes <- data_304 %>%
  filter(day == "Sunday", SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(SUBJECT_RACE, SUBJECT_OFFENSE) %>%
  summarise(counts = n()) %>%
  filter(counts>1)

plot_ly(sunday_crimes, x= ~SUBJECT_OFFENSE, y=  ~SUBJECT_RACE, type = "lineplot", mode = "markers") %>%
  layout(title = "Sunday Offences",
         xaxis = list(title = "SUBJECT_RACE"),
         yaxis = list(title = "SUBJECT_OFFENSE"))


########################## Injuries #########################

ofcr_expr <- data_304 %>%
  filter(OFFICER_INJURY == "Yes") %>%
  arrange(OFFICER_YEARS_ON_FORCE) %>%
  group_by(OFFICER_YEARS_ON_FORCE, OFFICER_INJURY) %>%
  summarise(counts = n())

ofcr_expr_no <- data_304 %>%
  arrange(OFFICER_YEARS_ON_FORCE) %>%
  group_by(OFFICER_YEARS_ON_FORCE, y=as.factor(OFFICER_INJURY_TYPE)) %>%
  summarise(counts = n())

ggplot(ofcr_expr, aes(x = OFFICER_YEARS_ON_FORCE, y = counts)) +
  geom_point(col="brown") + geom_line(col = "grey") +
  geom_text(aes(label= counts), size=3, vjust=2, hjust=1) +
  labs(x = "Officer Years of Service", y = "Injuries") +
  ggtitle("Officers' amount of injuries with their years of experience")

ggplot(ofcr_expr_no, aes(x = y, y = counts)) +
  geom_point(col="brown") + geom_line(col = "grey") +
  geom_text(aes(label= counts), size=3, vjust=2, hjust=1) +
  labs(x = "Officer Years of Service", y = "Injuries") +
  ggtitle("Officers with no history of injuries")

########################## Subject TypeOfForceUsed1 #########################

data_mod_Force1 <- data_304 %>%
  filter(SUBJECT_RACE != "NULL", SUBJECT_GENDER != "Unknown", SUBJECT_GENDER != "NULL", TYPE_OF_FORCE_USED1 != "NULL") %>%
  group_by(SUBJECT_RACE, SUBJECT_GENDER, TYPE_OF_FORCE_USED1) %>%
  summarise(counts = n()) %>%
  filter(counts>=10)

colorsvec <- c("#ffff00", "#FFC300", "#FF5733", "#C70039", "#900C3F", "#581845", "#2E4057", "#5D6D7E", "#E74C3C", "#8E44AD", "#3498DB", "#1ABC9C", "#16A085", "#27AE60", "#F1C40F", "#F39C12", "#D35400")

data_mod_Force1 %>% 
  ggplot(aes(x = SUBJECT_RACE, y = counts,  fill = TYPE_OF_FORCE_USED1)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~ SUBJECT_GENDER, nrow = 1) +
  scale_fill_manual(values = colorsvec) +
  labs(x = "Subject Race", y = "Counts", fill = "Use of Force 1") +
  ggtitle("Majority Type of Force1 used on Subjects") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

####################### Officer TypeOfForceUsed1 ##############################

data_ofcr_Force1 <- data_304 %>%
  filter(OFFICER_RACE != "NULL", OFFICER_RACE != "Other", OFFICER_GENDER != "Unknown", OFFICER_GENDER != "NULL", TYPE_OF_FORCE_USED1 != "NULL") %>%
  group_by(OFFICER_RACE, OFFICER_GENDER, TYPE_OF_FORCE_USED1) %>%
  summarise(counts = n()) %>%
  filter(counts>=10)

data_ofcr_Force1 %>% 
  ggplot(aes(x = OFFICER_RACE, y = counts,  fill = TYPE_OF_FORCE_USED1)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~ OFFICER_GENDER, nrow = 1) +
  scale_fill_manual(values = colorsvec) +
  labs(x = "Officer Race", y = "Counts", fill = "Use of Force 1") +
  ggtitle("Majority Type of Force1 used by the officers") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#################### electronic control device (ECD) Used ######################
table(data_304$OFFICER_ID)

ECD_used <- data_304 %>%
  filter(NUMBER_EC_CYCLES != "NULL", OFFICER_ID != 0) %>%
  group_by(NUMBER_EC_CYCLES, OFFICER_ID) %>%
  summarise(counts = n())
