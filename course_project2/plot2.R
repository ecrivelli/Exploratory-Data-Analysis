#############################################################################################################################################
## Coursera - Exploratory Data Analysis by Johns Hopkins University                                                                        ##
## https://www.coursera.org/learn/exploratory-data-analysis/home/welcome                                                                   ##
##                                                                                                                                         ##
## Course Project - Week 4 - Course Project 2 - plot2()                                                                                    ##
## ---------------------------------------------                                                                                           ##
##                                                                                                                                         ##
## TODO: Change the directory path to reflect your own directory structure                                                                 ##
##                                                                                                                                         ##
## This function simply create a new png plot file in your computer with answer this question:                                             ##
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?                          ##
## Use the base plotting system to make a plot answering this question.                                                                    ##
#############################################################################################################################################
plot2 <- function(){
        ###########################################################################################################################
        MY_WORKING_DIRECTORY <- "./ExploratoryDataAnalysis/course_project2/" #TODO: change it to reflect your own directory structure
        ###########################################################################################################################
        
        library(dplyr)
        
        # READING DATA
        ##############
        print("READING FINE PARTICULATE MATTER (PM2.5) DATA...")
        
        ## This first line will likely take a few seconds. Be patient!
        NEI <- readRDS(paste0(MY_WORKING_DIRECTORY, "/data/summarySCC_PM25.rds"))

        #Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?                        
        result <-
                NEI %>%
                filter(fips == "24510") %>% #just Baltimore City
                group_by(year) %>% 
                summarise(total_emissions = sum(Emissions)) #summarizing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        
        # CREATING THE PLOT
        ###################
        print("CREATING THE PLOT")
        #Using the base plotting system, create a plot to answaer this question: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
        
        #formating the file name
        plotFile <- paste0(MY_WORKING_DIRECTORY, "plot2.png")
        #call the plotting function to create a new png file
        png(filename = plotFile, width = 640, height = 480)
        
        #using the barplot function from the base plotting system
        barplot(result$total_emissions,
                beside = TRUE,
                names.arg = as.vector(result$year),
                main = "TOTAL PM2.5 EMISSIONS FROM ALL SOURCES IN BALTIMORE CITY PER YEAR", 
                sub = "source: United States Environmental Protection Agency",
                xlab = "",
                ylab = "PM2.5 - Fine Particulate Matter Emissions (tons)",
                ylim = c(0, 3500),
                col = "gray",
                border = "black")
        
        #closing the R graphics device
        dev.off()
        
        # PLOT DONE
        ###################
        print("DONE!")
        print(paste0("Please check your file inside your R working directory at:", plotFile) )
 
        # QUESTION
        ##########
        #Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
        
        # ANSWER
        ########
        #Yes, it has decreased substantially. The total PM2.5 emission has decreased 43% if we compare 1999 to 2008.
}