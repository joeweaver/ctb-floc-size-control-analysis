################################################################################
# Read in data for experimental time course determination and create plot
#
# Reads: time course data  (time course.csv)
#           from: data\sampling time determination
#        CTB particle data (rpm_vs_volume_frac.csv)
#           from: data\CTB particle data
#
# Outputs: 1-panel graph of particle changes over 3 hours in oCTB @50, 100 RPM
#              output: "timecourse.eps"
#          
# Joseph E. Weaver
# joe.e.weaver@gmail.com
# jeweave4@ncsu.edu
# NC State University
################################################################################


# Set up environment ------------------------------------------------------
library("dplyr")
library("here") #keep @jennybc from setting my computer on fire
library("readr")
library("tidyr")
library('ggplot2')


tc <- read_csv(here("data","sampling time determination","time course.csv"))
#tc %<>% subset(Minutes %in% c(30,90,120,180))

#directlabel gets grumpy when this isn't a factor
tc$RPM <- as.factor(tc$RPM)

#set some default theme elements
theme_bw()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=18),
             axis.text=element_text(size=18))

levels(tc$RPM) <- list("50 RPM"="50", "100 RPM"="100")

p<-ggplot(tc,aes(x=Minutes,y=Diameter,group=Minutes,color=RPM))+geom_boxplot()+facet_grid(. ~ RPM)
p <- p + ylab("Diameter (μm)") + xlab("Elapsed Time (min)")



postscript(here("output","time_course.eps"),colormodel="cmyk",width=7,height=3.5)
grobCex=1.4
p
dev.off()

sink(here("sessionInfo","timeCourse.R.sessionInfo.txt"))
sessionInfo()
sink()
