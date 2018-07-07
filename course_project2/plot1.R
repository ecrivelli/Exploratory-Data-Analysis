#############################################################################################################################################
## Coursera - Exploratory Data Analysis by Johns Hopkins University                                                                        ##
## https://www.coursera.org/learn/exploratory-data-analysis/home/welcome                                                                   ##
##                                                                                                                                         ##
## Course Project - Week 4 - Course Project 2 - plot1()                                                                                    ##
## ---------------------------------------------                                                                                           ##
##                                                                                                                                         ##
## TODO: Change the directory path to reflect your own directory structure                                                                 ##
##                                                                                                                                         ##
## This function simply create a new png plot file in your computer with answer this question:                                             ##
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?                                                       ##
## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005,   ##
## and 2008.                                                                                                                               ##
#############################################################################################################################################
plot1 <- function(){
        ###########################################################################################################################
        MY_WORKING_DIRECTORY <- "./ExploratoryDataAnalysis/course_project2/" #TODO: change it to reflect your own directory structure
        ###########################################################################################################################
        
        library(dplyr)
        
        # READING DATA
        ##############
        print("READING FINE PARTICULATE MATTER (PM2.5) DATA...")
        
        ## This first line will likely take a few seconds. Be patient!
        NEI <- readRDS(paste0(MY_WORKING_DIRECTORY, "/data/summarySCC_PM25.rds"))

        NEI$year = as.factor(NEI$year) ##converting years to factor to make them easier to work with
        
        #summarizing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        result <-
                NEI %>%
                group_by(year) %>%
                summarise(total_emissions = sum(Emissions))
                
        #making the emissions more readable in the plot
        result$total_emissions <- result$total_emissions / 1000000
        
        
        # CREATING THE PLOT
        ###################
        print("CREATING THE PLOT")
        ## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        #formating the file name
        plotFile <- paste0(MY_WORKING_DIRECTORY, "plot1.png")
        #call the plotting function to create a new png file
        png(filename = plotFile, width = 640, height = 480)
        
        #using the barplot function from the base plotting system
        plot(y=result$total_emissions,
             x=result$year,
             type = "l",
             main = "TOTAL PM2.5 EMISSIONS FROM ALL SOURCES IN THE USA FROM 1999 to 2008", 
             sub = "source: United States Environmental Protection Agency",
             xlab = "",
             ylab = "PM2.5 - Fine Particulate Matter Emissions (millions of tons)", #adjusting the scale
             ylim = as.double(c(3,8)),
             col = "black",
             lwd=3)
        
        #closing the R graphics device
        dev.off()
        
        # PLOT DONE
        ###################
        print("DONE!")
        print(paste0("Please check your file inside your R working directory at:", plotFile) )
        
        # QUESTION
        ##########
        #Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
        
        # ANSWER
        ########
        #Yes the total emission has diminished considerably from 7.33 Millions of tons in 1999 to 3,46 Millions of tons in 2008.
}