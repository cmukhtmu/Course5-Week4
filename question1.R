library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)

question1 <- function()
{
    # downloading the zip file
    if(!file.exists("./StormData.csv.bz2"))
    {
        print("Downloading zip file, please wait...")
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "./StormData.csv.bz2")
        print("Zip file downloaded successfully!")
    }
    
    # extracting the zip file    
    if(file.exists("./StormData.csv.bz2"))
    {
        print("Reading data from CSV file, please wait...")
        dtStorm = read.table("StormData.csv.bz2", header = TRUE, sep = ",", quote = "\"")
        print("CSV file read successfully!")
        
        storm_most_injuries = group_by(dtStorm, EVTYPE) %>%
            summarise(total = sum(INJURIES))
        storm_most_injuries = storm_most_injuries[order(storm_most_injuries$total,decreasing = T),]
        storm_most_injuries = head(storm_most_injuries, 10)
        
        p1 = ggplot(storm_most_injuries, aes(EVTYPE, total)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle=90)) +
            ggtitle("Injuries by Event Types") +
            xlab("Event Type") + ylab("Injuries")

        storm_most_fatalities = group_by(dtStorm, EVTYPE) %>%
            summarise(total = sum(FATALITIES))
        storm_most_fatalities = storm_most_fatalities[order(storm_most_fatalities$total,decreasing = T),]
        storm_most_fatalities = head(storm_most_fatalities, 10)
        
        p2 = ggplot(storm_most_fatalities, aes(EVTYPE, total)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle=90)) +
            ggtitle("Fatalities by Event Types") +
            xlab("Event Type") + ylab("Fatalities")
        
        ggarrange(p1, p2)
        
    }
    
}