#############################################################################################################################################
## Coursera - Exploratory Data Analysis by Johns Hopkins University                                                                        ##
## https://www.coursera.org/learn/exploratory-data-analysis/home/welcome                                                                   ##
##                                                                                                                                         ##
## Course Project - Week 4 - Course Project 2 - plot6()                                                                                    ##
## ---------------------------------------------                                                                                           ##
##                                                                                                                                         ##
## TODO: Change the directory path to reflect your own directory structure                                                                 ##
##                                                                                                                                         ##
## This function simply create a new png plot file in your computer with answer this question:                                             ##
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County,         ##
## California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?                                 ##
#############################################################################################################################################
plot6 <- function(){
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
        
        NEI$year = as.factor(NEI$year) #converting years to factor to make them easier to work with
        
        #we are just interested in emissions from motor vehicle sources        
        motorVehicle <- SCC[grep("?[Vv]ehicle", SCC$EI.Sector),]
        
        #Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037").
        #Which city has seen greater changes over time in motor vehicle emissions?
        dfBaltimoreLosAngeles <-
                NEI %>%
                filter(fips == "24510" | fips == "06037") %>% #just Baltimore City or Los Angeles
                filter(SCC %in% motorVehicle$SCC) %>% #just motor vehicle sources
                group_by(fips,year) %>% 
                summarise(total_emissions = sum(Emissions)) #summarizing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        #making legends easier to print
        dfBaltimoreLosAngeles$fips[dfBaltimoreLosAngeles$fips=="24510"] <- "Baltimore City"
        dfBaltimoreLosAngeles$fips[dfBaltimoreLosAngeles$fips=="06037"] <- "Los Angeles County"
        
        # CREATING THE PLOT
        ###################
        print("CREATING THE PLOT")
        
        library(ggplot2)
        
        #formating the file name
        plotFile <- paste0(MY_WORKING_DIRECTORY, "plot6.png")
        #call the plotting function to create a new png file
        png(filename = plotFile, width = 640, height = 480)
        
        #plot the variation of PM2.5 by city 
        myPlot <- ggplot(data=dfBaltimoreLosAngeles, aes(x=year, y=total_emissions,  group=fips)) +
                facet_grid(fips~., scales="free_y") + #one plot per city
                geom_line() +
                geom_point(size=4) +
                theme_bw() +
                labs(title="COMPARISON OF TOTAL PM2.5 (FINE PARTICULATE MATTER) EMISSIONS FROM \nMOTOR VEHICLE SOURCES IN BALTIMORE VS LOS ANGELES PER YEAR", 
                     y="PM2.5 - Emissions (tons)", 
                     x="years", 
                     caption="Source: United States Environmental Protection Agency (https://www.epa.gov/air-emissions-inventories)") 
        
        ggsave(plotFile, plot = myPlot)
        
        #closing the R graphics device
        dev.off()
        
        # PLOT DONE
        ###################
        print("DONE!")
        print(paste0("Please check your file inside your R working directory at:", plotFile) )
 
        # QUESTION
        ##########
        #Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County,
        #California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?                                 ##
        
        # ANSWER
        ########
        #We can look at change from two perspectives: 
        #1) Tons of emission (sheer number), Los Angeles had more changes than Baltimore City. L.A. has changed around 300 tons its volume of emission at each observation.
        #2) Tons of emission (variation), Baltimore City had bigger changes than Los Angeles. Baltimore has decreased more than 3.5 times the volume of emission in that decade.  
        
}