#############################################################################################################################################
## Coursera - Exploratory Data Analysis by Johns Hopkins University                                                                        ##
## https://www.coursera.org/learn/exploratory-data-analysis/home/welcome                                                                   ##
##                                                                                                                                         ##
## Course Project - Week 4 - Course Project 2 - plot5()                                                                                    ##
## ---------------------------------------------                                                                                           ##
##                                                                                                                                         ##
## TODO: Change the directory path to reflect your own directory structure                                                                 ##
##                                                                                                                                         ##
## This function simply create a new png plot file in your computer with answer this question:                                             ##
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?                                                 ##
#############################################################################################################################################
plot5 <- function(){
        ###########################################################################################################################
        MY_WORKING_DIRECTORY <- "./ExploratoryDataAnalysis/course_project2/" #TODO: change it to reflect your own directory structure
        ###########################################################################################################################
        
        
        # READING DATA
        ##############
        print("READING FINE PARTICULATE MATTER (PM2.5) DATA...")
        
        library(dplyr)
        
        ## This first line will likely take a few seconds. Be patient!
        NEI <- readRDS(paste0(MY_WORKING_DIRECTORY, "/data/summarySCC_PM25.rds"))
        SCC <- readRDS(paste0(MY_WORKING_DIRECTORY, "/data/Source_Classification_Code.rds"))
        
        NEI$year = as.factor(NEI$year) ##converting years to factor to make them easier to work with
        
        #we are just interested in emissions from motor vehicle sources        
        motorVehicle <- SCC[grep("?[Vv]ehicle", SCC$EI.Sector),]
        
        #removing the NOT USED codes
        motorVehicle <- motorVehicle[-grep("?(NOT USED)", motorVehicle$Short.Name),]
        
        #How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
        dfBaltimoreMotorVehicles <-
                NEI %>%
                filter(fips == "24510") %>% #just Baltimore City
                filter(SCC %in% motorVehicle$SCC) %>% #just motor vehicle sources
                group_by(type,year) %>% 
                summarise(total_emissions = sum(Emissions)) #summarizing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        # CREATING THE PLOT
        ###################
        print("CREATING THE PLOT")
        
        library(ggplot2)
        
        #formating the file name
        plotFile <- paste0(MY_WORKING_DIRECTORY, "plot5.png")
        #call the plotting function to create a new png file
        png(filename = plotFile, width = 640, height = 480)

        #using the qplot function from the ggplot2 plotting system
        myPlot <-ggplot(dfBaltimoreMotorVehicles, aes(year, total_emissions)) +
                geom_bar(stat = "identity") +
                xlab("years") + 
                ylab("PM2.5 - Fine Particulate Matter Emissions (tons)") +
                ggtitle("TOTAL PM2.5 EMISSIONS BY MOTOR VEHICLE SOURCES IN BALTIMORE CITY", subtitle = "source: United States Environmental Protection Agency") +
                theme_bw()
        
        ggsave(plotFile, plot = myPlot)
        
        #closing the R graphics device
        dev.off()
        
        # PLOT DONE
        ###################
        print("DONE!")
        print(paste0("Please check your file inside your R working directory at:", plotFile) )
        
        # QUESTION
        ##########
        #How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
        
        # ANSWER
        ########
        #It has decreased steadly at each observation during this period. From almost 400tons in 1999 to around 90tons in 2008.
}