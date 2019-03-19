library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)

getDMGValue <- function(row, DMG, DMGEXP, DMGVALUEEXP)
{
    DMGVALUE = 0
    
    if(row[DMGEXP] == "H") 
    {
        DMGVALUE = as.numeric(row[DMG]) * 100 
    }
    else if(row[DMGEXP] == "K") 
    {
        DMGVALUE = as.numeric(row[DMG]) * 1000 
    }
    else if(row[DMGEXP] == "M") 
    {
        DMGVALUE = as.numeric(row[DMG]) * 10^6 
    }
    else if(row[DMGEXP] == "B") 
    {
        DMGVALUE = as.numeric(row[DMG]) * 10^9 
    }
    
    row[DMGVALUEEXP] = DMGVALUE
}


question2 <- function()
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
        
        print("Calculating property damages, please wait...")
        dtStorm$PROPDMGVALUE = apply(dtStorm, 1, getDMGValue, "PROPDMG", "PROPDMGEXP", "PROPDMGVALUE")
        print("Property damages calculated successfully!")
        print("Calculating crop damages, please wait...")
        dtStorm$CROPDMGVALUE = apply(dtStorm, 1, getDMGValue, "CROPDMG", "CROPDMGEXP", "CROPDMGVALUE")
        print("Crop damages calculated successfully!")
        
        print("Getting top 10 property damages.")
        propertyDamages = group_by(dtStorm, EVTYPE) %>%
            summarise(total = sum(PROPDMGVALUE))
        propertyDamages = propertyDamages[order(propertyDamages$total,decreasing = T),]
        propertyDamages = head(propertyDamages, 10)
        
        print("Getting top 10 crop damages.")
        cropDamages = group_by(dtStorm, EVTYPE) %>%
            summarise(total = sum(CROPDMGVALUE))
        cropDamages = cropDamages[order(cropDamages$total,decreasing = T),]
        cropDamages = head(cropDamages, 10)
        
        print("Drawing charts, please wait...")
        p1 = ggplot(propertyDamages, aes(EVTYPE, total)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle=90)) +
            ggtitle("Property Damage by Event Types") +
            xlab("Event Type") + ylab("Property Damages in USD")
        
        p2 = ggplot(cropDamages, aes(EVTYPE, total)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle=90)) +
            ggtitle("Crop Damage by Event Types") +
            xlab("Event Type") + ylab("Crop Damages in USD")
        
        ggarrange(p1, p2)
        
    }
    
}