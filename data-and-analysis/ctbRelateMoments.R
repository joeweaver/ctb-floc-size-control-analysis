################################################################################
# Compare the moments for particle size and shear rate distributions within
# CTBs and Evans and Liu paper
#
# Reads: cached CTB moments and summarized previous pub (Evans and Liu) data
#
#
# Outputs: 1-panel graph of iCTB and oCTB means, compared w/ Evans & Liu (Fig 8)
#           output: "ctbRelateMoments.eps"
#              
#              
# Joseph E. Weaver
# joe.e.weaver@gmail.com
# jeweave4@ncsu.edu
# NC State University
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(ggplot2)
library(Cairo)
library(Weighted.Desc.Stat)
library(gridExtra)
library(grid)
library(directlabels)
library(here) #keep @jennybc from setting my computer on fire

# Read cached desriptive stats of CTB distributions & append Evans data----
# ASSUMES that ctbMoments.R has been run and that the cached file
# is current
q<-readRDS(here("cached","ctbDescStats.rds"))


# Append means from (Biggs & Lant 2000) and (Evans and Liu 2003)
otherPubFile <- here("data","other pubs","biggs and evans data.csv")
otherPubFrame <- read_csv(otherPubFile)

descStats<-full_join(q,otherPubFrame)


# Create plots ------------------------------------------------------------
#general idea is that we're going for a 1 col, 3 row set of plots*
#each figure will the relation between mean/std dev/cv of shear and psd
#row 1 = mean
#row 2 = std. dev
#row 3 = c.v.
#in reality, the grid will have 3 extra rows for labels
# UPDATE: We've decided only first panel is needed

#I'm being explicit, rather than clever with repeated code to get the plots
#built and in the paper. I may have to refactor later, but I'm ok
#with the technical debt here

#directlabel gets grumpy when this isn't a factor
descStats$Reactor <- as.factor(descStats$Reactor)

#set some default theme elements
theme_classic()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=16),
             axis.text=element_text(size=16,color="black"),
             strip.background=element_blank(),
             strip.text=element_blank())

#Omitting Biggs and Lant, as it is not a CTB
levels(descStats$Reactor) <- list("oCTB"="oCTB", "iCTB"="iCTB", 
                                  "iCTB Evans & Liu (2003)"="Evans & Liu")


descStats %<>% filter(Reactor %in% c("oCTB","iCTB","iCTB Evans & Liu (2003)"))

#means
p.means<-ggplot(descStats, aes(x=mean.shear,y=mean.psd,color=Reactor,group=Reactor,shape=Reactor,linetype=Reactor)) 
p.means<-p.means+geom_line(size=1.0)
p.means<-p.means+geom_point(size=3.0,fill="white")+geom_point(size=2.0,color="white")
p.means<-p.means+scale_color_manual(values=c("#74c476","#ca0020","#4575b4"))
p.means<-p.means+scale_shape_manual(values=c(18,17,16))
p.means<-p.means+scale_linetype_manual(values=c(6,2,3))
p.means<-p.means+xlab("Shear Rate (1/s)")+ylab("Particle Size (μm)")+ylim(0,150)


postscript("output\\ctbRelateMoments.eps",colormodel="cmyk",width=6,height=3)
direct.label(p.means,list("first.bumpup",vjust=0,hjust=-0.25))#,
dev.off()


sink(here("sessionInfo","ctbRelateMoments.R.sessionInfo.txt"))
sessionInfo()
sink()
