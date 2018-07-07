#############################################################################################################################################
## Coursera - Exploratory Data Analysis by Johns Hopkins University                                                                        ##
## https://www.coursera.org/learn/exploratory-data-analysis/home/welcome                                                                   ##
##                                                                                                                                         ##
## Course Project - Week 4 - Course Project 2 - plot4()                                                                                    ##
## ---------------------------------------------                                                                                           ##
##                                                                                                                                         ##
## TODO: Change the directory path to reflect your own directory structure                                                                 ##
##                                                                                                                                         ##
## This function simply create a new png plot file in your computer with answer this question:                                             ##
## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?                               ##
#############################################################################################################################################
plot4 <- function(){
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
        
        #Definitions:
        #- Coal is a natural mineral that is formed under the earth's crust due to the prolonged decay of plant and animal matter due to the heat and pressure.
        #- Charcoal is generally prepared by burning wood and sometimes animal matter and extinguishing the fire just before they turn ash. 
        #so we are just using coal combustion-related sources        
        CombCoal <- SCC[grep("?(Comb)?(Coal)", SCC$EI.Sector),]
        
        
        ## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?    
        dfCoalCombUSA <-
                NEI %>%
                filter(SCC %in% CombCoal$SCC) %>% #just coal combustion-related sources
                select(year, Emissions) %>%
                group_by(year) %>% 
                summarise(total_emissions = sum(Emissions)) #summarizing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        #making the emissions more readable in the plot
        dfCoalCombUSA$total_emissions <- dfCoalCombUSA$total_emissions / 1000
        
        # CREATING THE PLOT
        ###################
        
        print("CREATING THE PLOT")
        
        library(ggplot2)
        
        #formating the file name
        plotFile <- paste0(MY_WORKING_DIRECTORY, "plot4.png")
        #call the plotting function to create a new png file
        png(filename = plotFile, width = 640, height = 480)

        #using the qplot function from the ggplot2 plotting system
        myPlot <-ggplot(dfCoalCombUSA, aes(x=year, y=total_emissions)) +
                geom_line(color="red", size=2) +
                geom_point(color="red", size=4) +
                theme_bw()+
                labs(title="TOTAL PM2.5 EMISSIONS FROM COAL COMBUSTION-RELATED SOURCES TYPES IN THE USA", 
                     y="PM2.5 - Fine Particulate Matter Emissions (thousands of tons)", 
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
        #Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
        
        # ANSWER
        ########
        #Considering the whole decade, the total emissions from coal combustion-related sources in the USA have decreased drastically from near 600ktons in 1999 to less then 350ktons in 2008.
}