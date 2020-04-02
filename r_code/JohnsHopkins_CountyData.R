#GOAL: Assemble and analyze the Johns Hopkins County level data from their Daily Reports: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data 


# Set Up & Libraries ---------------------------------------------------------------
# Set Working Directory
setwd("D:/covid19_data")

# Load Libraries
library(tidyr)
library(lubridate)
library(xlsx)
library(readxl)
library(rvest)


# Load Data ---------------------------------------------------------------

# Data Links
daily.url<-"https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"
repo.zip.url<-"https://github.com/CSSEGISandData/COVID-19/archive/master.zip"

# Get the Daily Case Data & Unzip it
download.file(repo.zip.url, destfile = "COVID-19-master.zip")
unzip(zipfile="COVID-19-master.zip")

# Population Data
# Scrape table from World Population Review (because the .csv and .json file links they have are something odd that doens't have a stable link)
pop.url<-"https://worldpopulationreview.com/us-counties/ca/"
pop.html<-read_html(pop.url)
pop.table<-html_table((html_node(pop.html, "table")))



# Data Cleaning -----------------------------------------------------------

#..... Clean the population data numbers

pop.col<-as.numeric(gsub(",", "", pop.table$`2020 Population`)) #remove the commas from the numbers and change the format (character) to numeric
pop.table$`2020 Population`<-pop.col  #put the cleaned population data into the table


#..... Clean the daily cases data

# Get a lit of the .csv files
daily.csvs <- list.files(path=".\\COVID-19-master\\csse_covid_19_data\\csse_covid_19_daily_reports", pattern = "*csv$", full.names = "TRUE")

# Read the .csv files
#all.csvs <- lapply(daily.csvs, read.csv) #<- doesn't work because there's one file with a non-UTF-8 character encoding in the header

# Function to read the .csv files & replace the odd character in the column headings
read.daily<-function(a){
  daily<-read.csv(a, header = TRUE, stringsAsFactors = FALSE)
  names(daily)<-c("FIPS","Admin2","Province_State","Country_Region","Last_Update","Lat","Long_","Confirmed","Deaths","Recovered","Active","Combined_Key")
  return(daily)
}

# Apply the function we wrote to read the daily files  
admin2.data<-lapply(daily.csvs[61:length(daily.csvs)], read.daily)

# Append all the dataframes into one long dataframe
admin2.long<-do.call(rbind, admin2.data)
admin2.long<-cbind(1:length(admin2.long$Last_Update), admin2.long) #add a unique ID because it doesn't have one
names(admin2.long)[1]<-'ID' #giving the new first column a name

# Standardize the date format
dates<-substr(as.character(admin2.long$Last_Update), 1, gregexpr(pattern=' ', as.character(admin2.long$Last_Update)))
dates<-trimws(dates)
dates<-gsub('/', '-', dates)  #make all the dates use dashes instead of slashes
dates<-gsub('3-22-20', '2020-03-22', dates) #fix the dates that used a different format alltogether
admin2.long<-cbind(admin2.long, dates)  #add the new date column to the dataframe (keeping the old one)
names(admin2.long)[14]<-'Updated'  #name the new column


# Data Visualization for California Counties ------------------------------

# Pull out the California data
ca.long<-subset(admin2.long, admin2.long$Province_State == 'California')

# Change the subsetted long-format data to wide-format
ca.wide<-pivot_wider(data=ca.long, id_cols = c("Admin2", "Province_State", "Country_Region"), values_from = "Confirmed", names_from = "Updated")




# Plotting Functions
#..... Functions to plot a list of counties on one graph
#..... .. counties_list = list of counties like compare.counties above
#..... .. dataset = which dataset do you want to use? ex: ca.wide

#..... Plot Raw Case Numbers
cases.plot<-function(dataset, counties_list){
  dates.axis<-as.Date(names(dataset)[4:length(dataset)], format="%Y-%m-%d")
  first.county<-subset(dataset, dataset$Admin2 == counties_list[1])
  ymax<-first.county[[length(names(first.county))]]
  plot(0, type='n', xlim=c(min(dates.axis), max(dates.axis)), ylim=c(0, ymax), main = "Not Normalized", xlab = "Date", ylab = "Number of Cases", xaxt="n")
  axis(1, dates.axis, format(dates.axis, "%m-%d"), cex.axis=.7)
  
  for(i in 1:length(counties_list)){
    #get the data
    county.data<-subset(dataset, dataset$Admin2 == counties_list[i])
    #print(county.data)
    
    #colors for plotting
    #colors.list<-topo.colors(length(counties_list))
    colors.list<-rainbow(length(counties_list))
    
    #plot the data
    lines(y=county.data[1,4:length(names(dataset))], x=dates.axis, col=colors.list[i], lwd=2)
  }
  #legend("topleft", col=colors.list, legend=counties_list, pch = 19, title = "Counties")
}

#..... Plot Data Normalized by Population
cases.normalized.plot<-function(dataset, counties_list, pop_data){
  first.county<-subset(dataset, dataset$Admin2 == counties_list[1])
  first.county.pop<-subset(pop_data, pop_data[,1]== paste(counties_list[1], "County"))
  ymax<-(first.county[[length(names(first.county))]]/first.county.pop[[2]])
  dates.axis<-as.Date(names(dataset)[4:length(dataset)], format="%Y-%m-%d")
  plot(0, type='n', xlim=c(min(dates.axis), max(dates.axis)), ylim=c(0, ymax), main = "Normalized by Population", xlab="Date", ylab = "Number of Cases/Population", xaxt="n")
  axis(1, dates.axis, format(dates.axis, "%m-%d"), cex.axis=.7)
  
  for(i in 1:length(counties_list)){
    #get the data
    county.data<-subset(dataset, dataset$Admin2 == counties_list[i])
    county.pop<-subset(pop_data, pop_data[,1]== paste(counties_list[i], "County"))
    county.pop<-county.pop[1,2]

    #colors for plotting
    #colors.list<-topo.colors(length(counties_list))
    colors.list<-rainbow(length(counties_list))
    
    #plot the data
    lines(y=(county.data[1,4:length(names(dataset))]/county.pop), x=dates.axis, col=colors.list[i], lwd=2)
  }
  #legend("topleft", col=colors.list, legend=counties_list, pch = 19, title = "Counties")
}

#..... Make a legend in a blank plot
legend.pane<-function(counties_list){
  plot(0, type='n', xlim=c(0, 10), ylim=c(0, 10), axes = FALSE, xaxt='n', yaxt='n', ann=FALSE)
  
  #colors for plotting
  #colors.list<-topo.colors(length(counties_list))
  colors.list<-rainbow(length(counties_list))
  
  legend("top", col=colors.list, legend=counties_list, pch = 19, ncol = 4, bty = "n")
}

#..... Population histogram
pop.plot<-function(counties_list){
  county.pop<-subset(pop.table, pop.table$Name %in% paste(counties_list, "County"))
  print(county.pop)
  barplot(county.pop$`2020 Population`, names.arg = gsub(" County", "", county.pop$Name), las=2, col=rainbow(length(county.pop$Name)), main = "County Populations")
}

# Plotting Data

#..... what cities should we compare? 
#..... ...Keeping a list of ones I've tried.
compare.counties<-c("Santa Clara", "San Francisco", "Sacramento")
compare.counties<-c("Los Angeles", "Sacramento", "Yolo", "Solano")
compare.counties<-c("San Francisco", "Sacramento", "Yolo", "Solano") #A good example
compare.counties<-c("Santa Clara", "Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Solano", "Sonoma")

#..... Plot the cases graphs and legend in one image
par(mfrow=c(1,1), mar=c(3.8, 4.1, 2.0, 2.0), oma=c(0,0,0,0))
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE), heights=c(2,1))
cases.plot(dataset=ca.wide, counties_list=compare.counties)
cases.normalized.plot(dataset=ca.wide, counties_list=compare.counties, pop_data = pop.table)
legend.pane(compare.counties)

#..... Plot the population bar graph
par(mfrow=c(1,1), mar=c(6.3, 4.1, 4.1, 2.1))
pop.plot(compare.counties)

