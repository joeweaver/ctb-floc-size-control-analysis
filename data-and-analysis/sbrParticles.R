################################################################################
# Compare particle size distributions taken fro FIJI image analysis
#  between different SBRs operated overthe range of superficial gas velocities
# 
# Reads:
# 
# Outputs:
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
library("purrr")
library("readr")
library("tidyr")
library("magrittr")
library("stringr")
library('ggplot2')
library(here) #keep @jennybc from setting my computer on fire

# Read in particle data from FIJI analysis ---------------------------------
# Reading multiple csvs, much credit to:
# http://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R

data_dir = here("data","SBR particle data")

files <- dir(data_dir, pattern = "*.csv") 

data <- data_frame(filename = files) %>% 
  mutate(file_contents = map(filename,          
           ~ read_csv(file.path(data_dir, .)))
        )  
data <- unnest(data)

# Process the particle image data -----------------------------------------
# Figure out the reactor name and shear rate based on the filename
data$reactor <- 
   stringr::str_extract(data$filename,"(IMED|ISQUAT|ITALL|MED|SQUAT|TALL)")
data$shear <- stringr::str_match(data$filename,"shear-(\\d+)")[,2]

# Convert the areas reported by FIJI to diameters and volumes
data$eqd_um <- sqrt(data$Area*1e8/2)/pi
data$eqvol_um <- ((data$eqd_um/2)^3)*(4/3)

# count particles measured
cound<-data %>% group_by(reactor,shear) %>% summarize(n=n())

# calculate the volume fraction of particles
a <- data %>% group_by(reactor,shear) %>%
   mutate(volfrac=eqvol_um/sum(eqvol_um))

#We only care about the final reactors and at the shears specified
toPlot <- subset(a,reactor %in% c("MED","SQUAT","TALL") & shear %in% c(200,100,250,50))
toPlot$shear <- factor(toPlot$shear, levels = c("50", "100", "200","250"))
levels(toPlot$shear) <- list("50 1/s"="50", "100 1/s"="100", "200 1/s"="200", "250 1/s"="250")



# Generate descriptive stats of particles for SBR  runs ------------------

partDescStats<-toPlot %>%
   group_by(reactor,shear) %>%
   summarize(mean=w.mean(eqd_um,volfrac),
             sd=w.sd(eqd_um,volfrac),
             kurt=w.kurtosis(eqd_um,volfrac),
             skew=w.skewness(eqd_um,volfrac),
             med=median(eqd_um),
             w.cv=w.cv(eqd_um,volfrac))

#write the merged summary statistics to a csv which we will then make pretty 
write.table(partDescStats,file=here("output","SBRmomentsParticles.csv"),sep=",",
            quote=FALSE,row.names=F)

# Create a plot comparing particles between reactors ------------------

theme_classic()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=14),
             axis.text=element_text(size=11,color="black"),
             strip.text=element_text(size=14))


# make the units look nice
shear_names <- list(
   '50 1/s' = expression(paste("50 s"^"-1")),
   '100 1/s' = expression(paste("100 s"^"-1")),
   '200 1/s' = expression(paste("200 s"^"-1")),
   '250 1/s' = expression(paste("250 s"^"-1"))
)

shear_labeller <- function(variable,value){
   return(shear_names[value])
}

#plotting as a boxplot sinnce the distributions are the same 
p <-  ggplot(toPlot, aes(reactor,eqd_um,group=reactor,fill=reactor))
p <- p + geom_violin()
p <- p + coord_cartesian(ylim=c(0,100)) + 
   facet_grid(. ~ shear, labeller=shear_labeller) 
p <-  p + xlab("Reactors, grouped by target mean shear rate") + 
   ylab("Equivalent Diameter (μm)")
p

postscript(here("output","sbrParticles.eps"),colormodel="cmyk",width=7,height=3)
p
dev.off()


# Save our session info ------------------

sink(here("sessionInfo","sbrParticles.R.sessionInfo.txt"))
sessionInfo()
sink()

