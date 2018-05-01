################################################################################
# Calculate the moments for particle size and shear rate distributions within
# CTBs and generate plots suitable for publication.
#
# Reads: cached CTB shear and particle data
#
# Caches: descriptived statistics for CTB particle and shear data
#           in: "ctbDescStats.rds"
#
# Outputs: 2-panel graph of CTB shear and particle CV (should be Fig. 3)
#              output: "ctbMoments.eps"
#          Text summary of CTB shear and particle moments (in SI)
#              output: "CTBmoments.csv"
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
library(matrixStats)
library(gridExtra)
library(grid)
library(directlabels)
library(here) #keep @jennybc from setting my computer on fire

# Read cached PSD data ----------------------------------------------------
psdFrame<-readRDS(here("cached","CTBparticleSizeDataProcessed.rds"))

# Read cached shear distributions -----------------------------------------
shearFrame<-readRDS(here("cached","CTBshearDataProcessed.rds"))

# see ctbDistributions.R to see how these rds were generated
#
# ASSUMES that ctbDistributions_for_pub has been run and that the cached file
# is current

# Calculate descriptive stats for each distribution and merge into --------

#lets merge stuff

#note we're using the weighted.Desc.Stat package since our data is presented
#in the form of volume fractions

shearDescStats<-shearFrame %>%
  group_by(reactor,flowSim,`rotPM [rotations per min]`) %>%
  filter(flowSim=="Turbulent") %>%
  summarize(mean=w.mean(`spf4.sr [Shear rate]`,volfrac),
            sd=w.sd(`spf4.sr [Shear rate]`,volfrac),
            kurt=w.kurtosis(`spf4.sr [Shear rate]`,volfrac),
            skew=w.skewness(`spf4.sr [Shear rate]`,volfrac),
            cv=sd/mean)

psdDescstats<-psdFrame %>%
  group_by(Reactor,RPM) %>%
  filter(Reactor=="iCTB" | Reactor=="oCTB") %>%
  summarize(mean=w.mean(`Bin median`,`Volume Fraction`),
            sd=w.sd(`Bin median`,`Volume Fraction`),
            kurt=w.kurtosis(`Bin median`,`Volume Fraction`),
            skew=w.skewness(`Bin median`,`Volume Fraction`),
            cv=sd/mean) 

#do the actual merge
q<-inner_join(psdDescstats,shearDescStats,by=c("RPM"="rotPM [rotations per min]","Reactor"="reactor"),suffix=c(".psd",".shear"))
#cache this for later
saveRDS(q,here("cached","ctbDescStats.rds"))

#going to drop shearFrame since it's large and we don't need it for further work
rm(shearFrame)


# Create plots ------------------------------------------------------------
#general idea is that we're going for a 2 col, 3 row set of plots*
#each figure will show how RPM affects a distribution stat within both reactors
#col 1 = shear stuff
#col 2 = particle stuff
#row 1 = mean
#row 2 = std. dev
#row 3 = c.v.
#in reality, the grid will have 3 extra rows for labels

#I'm being explicit, rather than clever with repeated code to get the plots
#built and in the paper. I may have to refactor later, but I'm ok
#with the technical debt here

#directlabel gets grumpy when this isn't a factor
q$Reactor <- as.factor(q$Reactor)

#set some default theme elements
theme_classic()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=18),
             axis.text=element_text(size=18),
             strip.background=element_blank(),
             strip.text=element_blank())


#when evalauted in grid.arrange, throws a 'don't know how to pick scale' warning
#graph comes out OK
plot_dstat<-function(stat_colName,xText="",yText="",posList=list("last.bumpup"),ymax=""){
  p.dstat<-ggplot(q, aes(x=`RPM`,y=get(stat_colName),color=Reactor,group=Reactor,shape=Reactor,linetype=Reactor)) 
  p.dstat<-p.dstat+geom_line(size=1.3)+xlim(0,275)
  if(ymax != ""){
    p.dstat<-p.dstat+ylim(0,ymax)
  }
  p.dstat<-p.dstat+geom_point(size=3.7)+geom_point(size=2.7,color="white")
  p.dstat<-p.dstat+ylab(yText)+xlab(xText)
  p.dstat<-p.dstat + scale_color_manual(values=c("#ca0020","#4575b4"))
  p.dstat<-p.dstat + scale_linetype_manual(values=c(2,3))
  p.dstat<-p.dstat + scale_shape_manual(values=c(17,16))
  return(direct.label(p.dstat,posList))
}


postscript(here("output","ctbMoments.eps"),colormodel="cmyk",width=4,height=6)
grobCex=1.4
grid.arrange(#means
            # textGrob("(a) Mean Shear rate vs RPM",gp=gpar(cex=grobCex)),
             #textGrob("(b) Mean Particle Size RPM",gp=gpar(cex=grobCex)),
             #plot_dstat('mean.shear',yText=expression(paste(hat(mu)," (",s^-1,")")),ymax=150,posList=list("last.bumpup",hjust=-.4)),
             #plot_dstat('mean.psd',yText=expression(paste(hat(mu)," (",mu,"m)")),ymax=150,posList=list("first.bumpup",hjust=1.4)),
             #std devs
             #textGrob("(c) Std. Dev. Shear rate vs RPM",gp=gpar(cex=grobCex)),
            # textGrob("(d) Std. Dev.  Particle Size RPM",gp=gpar(cex=grobCex)),
            # plot_dstat('sd.shear',yText=expression(paste(hat(sigma)," (",s^-1,")")),ymax=150,posList=list("last.bumpup",hjust=-.4)),
            # plot_dstat('sd.psd',yText=expression(paste(hat(sigma)," (",mu,"m)")),ymax=150,posList=list("first.bumpup",hjust=1.4)),
             #cv's 
             textGrob("     (a) C.V. Shear rate vs RPM",gp=gpar(cex=grobCex)),
             plot_dstat('cv.shear',"RPM",yText=expression(hat(sigma)/hat(mu)),posList=list("last.bumpup",cex=1.1,hjust=-.2)),
             textGrob("",gp=gpar(cex=grobCex)),
             textGrob("      (b) C.V Particle Size vs RPM",gp=gpar(cex=grobCex)),
             plot_dstat('cv.psd',"RPM",yText=expression(hat(sigma)/hat(mu)),ymax=0.8,posList=list("first.bumpup",cex=1.1,hjust=1.3,vjust=-0.025)),
             ncol=1,nrow=5,heights=c(1,5,1,1,5))
dev.off()

#write the merged summary statistics to a csv which we will then make pretty in word
write.table(q,file=here("output","CTBmoments.csv"),sep=",",quote=FALSE,row.names=F)


sink(here("sessionInfo","ctbMoments.R.sessionInfo.txt"))
sessionInfo()
sink()
