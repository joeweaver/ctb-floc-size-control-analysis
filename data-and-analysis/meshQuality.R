################################################################################
# Create histograms showing each CFD mesh #of elements and quality
#
# Reads: Tidied cfd data ("reactor_*_shear_100_wderivs.csv", "*qual_only.csv")
#        from: data\CFD exports\SBRs and data\tidied CFD exports\CTBs
#
# Outputs: Figure comparing SBR and CTB mesh qualities (Fig. S10)
#           file: meshQuality.eps              
#              
# Joseph E. Weaver
# joe.e.weaver@gmail.com
# jeweave4@ncsu.edu
# NC State University
#
################################################################################


# Set up environment ------------------------------------------------------
library("dplyr")
library("readr")
library("tidyr")
library("magrittr")
library("ggplot2")
library(Weighted.Desc.Stat)
library(here) #keep @jennybc from setting my computer on fire

# Read CFD results and combine into a useful dataframe -------------------
cols_to_read <- cols_only(`qual [Mesh quality]` = "d")

fname <- here("data","CFD exports","SBRs","reactor_squat_shear_100_wderivs.csv")
squat <- read_csv(fname,col_types = cols_to_read) %>% mutate(reactor = "(c) Squat SBR")

fname <- here("data","CFD exports","SBRs","reactor_med_shear_100_wderivs.csv")
med <- read_csv(fname,col_types = cols_to_read) %>% mutate(reactor = "(d) Med SBR")

fname <- here("data","CFD exports","SBRs","reactor_tall_shear_100_wderivs.csv")
tall <- read_csv(fname,col_types = cols_to_read ) %>% mutate(reactor = "(e) Tall SBR")

fname <- here("data","tidied CFD exports","CTBs","3d sliding wall - iCTB - qual_only.csv")
iCTB <- read_csv(fname,col_types = cols_to_read ) %>% mutate(reactor = "(a) iCTB")

fname <- here("data","tidied CFD exports","CTBs","3d sliding wall - oCTB - qual_only.csv")
oCTB <- read_csv(fname,col_types = cols_to_read ) %>% mutate(reactor = "(b) oCTB")

allCfd <- bind_rows(list(squat,med,tall,iCTB,oCTB))

#get rid of big, unused dataframes
rm(squat)
rm(med)
rm(tall)
rm(iCTB)
rm(oCTB)

# Create a plot comparing mesh qualties between reactors ------------------
theme_classic()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=12),
             axis.text=element_text(size=12),
             strip.text=element_text(size=12))

elCount <- allCfd %>% group_by(reactor) %>% summarise(nel=n())
elCount %<>% mutate(elText=paste0(nel," total elements"))

#allCfd$reactor <- factor(allCfd$reactor, levels=c("oCTB", "iCTB", "Squat SBR", "Med SBR", "Tall SBR"))


p <-  ggplot(allCfd, aes(`qual [Mesh quality]`,group=reactor,
                         color=reactor,fill=reactor)) 
p <- p + geom_histogram(bins=1000) + facet_grid(reactor ~ .,scales="free_y") 
p <- p + xlab("Mesh Quality") + ylab("Element Count")
p <- p + geom_text(data=elCount,aes(label=elText,x=0.5,y=100,vjust=-.15),color="black")
p

postscript(here("output","meshQuality.eps"),colormodel="cmyk",width=6,height=10)
p
dev.off()

# Save our session info ------------------

sink(here("sessionInfo","meshQuality.R.sessionInfo.txt"))
sessionInfo()
sink()

