################################################################################
# Compare CFD estimated shear rates between three SBR geometries with 
# superficial gas velocities estimated to cause a mean hydrodyanmic shear
# rate of 100 s-1
#
# Reads: Tidied cfd data from SBR runs ("reactor_*_shear_100_wderivs.csv")
#        from: data\CFD exports\SBRs
#
# Outputs: Figure comparing SBR shear distributions (Fig. 6)
#           file: sbrShears.eps
#          CSV of desriptive stats for the shear distributions
#           file: SBRmoments.csv
#              
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
cols_to_read <- cols_only(`mm.mu [Dynamic viscosity - mixture]` = "d",
                          `mm.U [Velocity field - mixture]` = "d",
                          `u3 [Velocity field - mixture - x component]` = "d",
                          `v3 [Velocity field - mixture - y component]` = "d",
                          `w3 [Velocity field - mixture - z component]` = "d",
                          `dvol [Volume scale factor]` = "d",
                          `meshtype []` = "i",
                          `u3x []` = "d",`u3y []` = "d",`u3z []` = "d",
                          `v3x []` = "d",`v3y []` = "d",`v3z []` = "d",
                          `w3x []` = "d",`w3y []` = "d",`w3z []` = "d")

fname <- here("data","CFD exports","SBRs","reactor_squat_shear_100_wderivs.csv")
squat <- read_csv(fname,col_types = cols_to_read) %>% mutate(reactor = "squat")

fname <- here("data","CFD exports","SBRs","reactor_med_shear_100_wderivs.csv")
med <- read_csv(fname,col_types = cols_to_read) %>% mutate(reactor = "medium")

fname <- here("data","CFD exports","SBRs","reactor_tall_shear_100_wderivs.csv")
tall <- read_csv(fname,col_types = cols_to_read ) %>% mutate(reactor = "tall")

allCfd <- bind_rows(list(squat,med,tall))

#get rid of big, unused dataframes
rm(squat)
rm(med)
rm(tall)

# Process CFD results to calculate volume fractions and such ------------------
#depending on meshtype, we have to apply a correction factor to dvol
#TODO these data have been checked and no other mesh types than 9 and 6 exist
#it would be better to warn in case that assumption gets violated
correctionfactor <- function(meshType){
   ifelse(meshType == 9,0.5,(1/6))
}

# calculate true volume of each element
allCfd %<>% group_by(reactor) %>%
   mutate(correction=correctionfactor(`meshtype []`))
allCfd$truevol <- allCfd$`dvol [Volume scale factor]` * allCfd$correction


allCfd %<>% group_by(reactor) %>%
   mutate(volfrac=truevol/sum(truevol))

# calculate the per element magnitude of the velocity gradient
allCfd %<>% group_by(reactor) %>%
   mutate(mvg=sqrt((`u3y []`+`v3x []`)^2+(`v3z []`+`w3y []`)^2+(`w3x []`+`u3z []`)^2))

# create factors for use in plotting
allCfd$reactor <- factor(allCfd$reactor, levels = c("squat", "medium", "tall"))

# Generate descriptive stats of shear for SBR shear runs ------------------
shearDescStats<-allCfd %>%
   group_by(reactor) %>%
   summarize(mean=w.mean(mvg,volfrac),
             sd=w.sd(mvg,volfrac),
             kurt=w.kurtosis(mvg,volfrac),
             skew=w.skewness(mvg,volfrac),
             med=median(mvg),
             w.cv=w.cv(mvg,volfrac))

#write the merged summary statistics to a csv which we will then make pretty 
write.table(shearDescStats,file=here("output","SBRmoments.csv"),sep=",",
            quote=FALSE,row.names=F)

# Create a plot comparing shear between reactors ------------------
theme_classic()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=18),
             axis.text=element_text(size=18),
             strip.text=element_text(size=18))

p <-  ggplot(allCfd, aes(mvg,..density.., weight=volfrac,group=reactor,
                    color=reactor,fill=reactor)) 
p <- p + geom_histogram(bins=1000) + facet_grid(reactor ~ .) 
p <- p + xlab("Velocity gradient (1/s)")+ylab("Volume fraction")
p <- p + coord_cartesian(xlim=c(0,15))
p

postscript(here("output","sbrShears.eps"),colormodel="cmyk",width=3,height=6)
p
dev.off()

# Save our session info ------------------

sink(here("sessionInfo","sbrShears.R.sessionInfo.txt"))
sessionInfo()
sink()
