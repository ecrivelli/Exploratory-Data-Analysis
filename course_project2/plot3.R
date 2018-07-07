#############################################################################################################################################
## Coursera - Exploratory Data Analysis by Johns Hopkins University                                                                        ##
## https://www.coursera.org/learn/exploratory-data-analysis/home/welcome                                                                   ##
##                                                                                                                                         ##
## Course Project - Week 4 - Course Project 2 - plot3()                                                                                    ##
## ---------------------------------------------                                                                                           ##
##                                                                                                                                         ##
## TODO: Change the directory path to reflect your own directory structure                                                                 ##
##                                                                                                                                         ##
## This function simply create a new png plot file in your computer with answer this question:                                             ##
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen   ##
## decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008?                        ##
## Use the ggplot2 plotting system to make a plot answer this question.                                                                    ##
#############################################################################################################################################
plot3 <- function(){
        ###########################################################################################################################
        MY_WORKING_DIRECTORY <- "./ExploratoryDataAnalysis/course_project2/" #TODO: change it to reflect your own directory structure
        ###########################################################################################################################
        
        
        # READING DATA
        ##############
        print("READING FINE PARTICULATE MATTER (PM2.5) DATA...")
        
        library(dplyr)
        
        ## This first line will likely take a few seconds. Be patient!
        NEI <- readRDS(paste0(MY_WORKING_DIRECTORY, "/data/summarySCC_PM25.rds"))

        NEI$type[NEI$type=="NONPOINT"] <- "NON-POINT" #just putting the NONs in the same wording
        NEI$type = as.factor(NEI$type) #converting type as a factor (NONPOINT, NON-ROAD, ON-ROAD, POINT) to make it easier to work with
        
        #Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
        #which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
        #Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
        dfBaltimore <-
                NEI %>%
                filter(fips == "24510") %>% #just Baltimore City
                group_by(type,year) %>% 
                summarise(total_emissions = sum(Emissions)) #summarizing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        
        
        # CREATING THE PLOT
        ###################
        print("CREATING THE PLOT")
        
        library(ggplot2)
        
        #formating the file name
        plotFile <- paste0(MY_WORKING_DIRECTORY, "plot3.png")
        #call the plotting function to create a new png file
        png(filename = plotFile, width = 640, height = 480)
        
        #using the qplot function from the ggplot2 plotting system
        myPlot <-ggplot(dfBaltimore, aes(dfBaltimore$year, dfBaltimore$total_emissions)) +
                geom_point(size=4)+
                geom_smooth(method="lm", se=FALSE, , linetype="dashed")+  # Add linear regression line
                facet_wrap(~type, scales="free_y", ncol=2) + #one plot per type
                theme_bw() +
                labs(title="TOTAL PM2.5 EMISSIONS BY SOURCES TYPES IN BALTIMORE CITY PER YEAR", 
                     y="PM2.5 - Fine Particulate Matter Emissions (tons)", 
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
        #Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases 
        #in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008?
        # Use the ggplot2 plotting system to make a plot answer this question.

        # ANSWER
        ########
        # I decided to add a linear regression curve to make the tendency analysis more obvious. Three source types have seem an important decrease in the total PM2.5 emissions: on-road, non-road and non-point. 
        #The only source type that has its emissions increased was point in Baltimore city which represents larger sources that are located at a fixed, stationary location like industrial and commercial facilities.
}