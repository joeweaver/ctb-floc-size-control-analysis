################################################################################
# Read in data for particle size and shear rate distributions within
# CTBs and generate plots suitable for publication.
#
# Reads: tidied CFD exports for CTBs (various *.csv files)
#           from: data\tidied CFD exports\CTBs
#        CTB particle data (rpm_vs_volume_frac.csv)
#           from: data\CTB particle data
#
# Caches: processed CTB particle and shear data
#
# Outputs: 4-panel graph of CTB shear and particle dists (should be Fig. 2)
#              output: "ctbDists.eps"
#          Text summary of weighted means and modes of shear dists (in SI)
#              output: "weighted_shear_median_mode.csv"
# 
# Joseph E. Weaver
# joe.e.weaver@gmail.com
# jeweave4@ncsu.edu
# NC State University
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(Cairo)
library(directlabels)
library(gtable)
library(grid)
library(gridExtra)
library(plotrix)
library(here) #keep @jennybc from setting my computer on fire

source(here('multiplot.r'))

#Note, reading the files takes a while
#you can skip to the '(un)cache shear data' and '(un)cache particle size data' 
#if you haven't changed the data and are sure you know what you're doing

# Shear Data location and description ------------------------------------------

#define the experiment as a combination of CTB reactors, rpms, and flow sims 
#(Here just Turbulent)
iCTBExperiments<-
  expand.grid(reactor="iCTB",flowSim="Turbulent",rpm=c("45","120","240")) %>%
  mutate( pattern= paste0("*",reactor,".*",rpm,".*",flowSim,".csv"))

oCTBExperiments<-
  expand.grid(reactor="oCTB",flowSim="Turbulent",rpm=c("50","100","200")) %>%
  mutate( pattern= paste0("*",reactor,".*",rpm,".*",flowSim,".csv"))

experiment <- bind_rows(iCTBExperiments,oCTBExperiments)


# Read in the shear data files -------------------------------------------------

#for each experimental run, read the relevant data file(s) into a dataframe





#Note this is a simplfied version which expects turbulent flowsim variables
readExperimentRow<-function (experimentRow){
   #location of tidied up CFD exports
   dataDir <- here("data","tidied CFD exports","CTBs")
   
   files<- dir(dataDir,pattern=experimentRow$pattern)
  
  data <- 
    data_frame(filename=files,flowSim=experimentRow$flowSim,reactor=experimentRow$reactor) %>%
    mutate(file_contents = map(filename,
            ~ read_csv(file.path(dataDir, .)))
    )

  data<-unnest(data)
  
  return(data)
}

#TODO probaby a more clever way to do this than looping through each row
readExperiment<-function(experiment){
  expData<-readExperimentRow(experiment[1,])
  for( i in 2:nrow(experiment)){
    expData %<>% bind_rows(readExperimentRow(experiment[i,]))
     #return(expData)
  }
  return(expData)
}

shearData<-readExperiment(experiment)

# Post process shear data for things like volume fraction -----------------

#depending on meshtype, we have to apply a correction factor to dvol
#see: https://www.comsol.com/community/forums/general/thread/116/
#Triangle : 1/2
#Tetrahedron : 1/6
#Prism : 1/2
#Pyramid : 1/3

# There should only be tets and prisms here, better to fold into method
# but let's just check here
if(!all(unique(shearData$`meshtype []`) == c(6,9))){
   warning("Found unexpected meshtype")
}

##depending on meshtype, we have to apply a correction factor to dvol
correctionfactor <- function(meshType){
   ifelse(meshType == 9,0.5,(1/6))
}

shearData %<>%  group_by(reactor) %>%
   mutate(correction=correctionfactor(`meshtype []`)) %>%
   mutate(truevol = correction*`dvol [Volume scale factor]`)


# determine the volfrac per element
shearData %<>%  group_by(reactor,`rotPM [rotations per min]`,flowSim) %>%
   mutate(volfrac=truevol/sum(truevol))


#sanity check
# sanity <- shearData %>% group_by(reactor,`rotPM [rotations per min]`,flowSim) %>%
#    summarise(totalfrac=sum(volfrac),totalVol=sum(truevol))
#for this project we expect
#1.7 L and 3.0 L (note totalvol in data is m^3
#and totalfrac to add to 1
## A tibble: 6 x 5
# # Groups:   reactor, rotPM [rotations per min] [?]
# reactor `rotPM [rotations per min]`   flowSim totalfrac    totalVol
# <chr>                       <int>    <fctr>     <dbl>       <dbl>
# 1    iCTB                          45 Turbulent         1 0.001700527
# 2    iCTB                         120 Turbulent         1 0.001700527
# 3    iCTB                         240 Turbulent         1 0.001700527
# 4    oCTB                          50 Turbulent         1 0.003003492
# 5    oCTB                         100 Turbulent         1 0.003003492
# 6    oCTB                         200 Turbulent         1 0.003003492

## TODO do we actually use these cumulatives?
#calculate the cumulative volume fraction of each element
shearData %<>%
  group_by(reactor, flowSim, `rotPM [rotations per min]`) %>%
  arrange(`spf4.sr [Shear rate]`)

shearData %<>%
  group_by(reactor, flowSim, `rotPM [rotations per min]`) %>%
  mutate(cvolfrac=cumsum(volfrac))


# (un)cache shear data --------------------------
#comment/uncomment as appropriate
#used to save time reading files during EDA, the final publication run was done
#without caching
saveRDS(shearData,here("cached","CTBshearDataProcessed.rds"))
#shearData<-readRDS(here("cached","CTBshearDataProcessed.rds"))


# Read particle size data -----------------------------

#location of particle size data
#for the CTBs we only have the histograms
psdFile <- here("data","CTB particle data","rpm_vs_volume_frac.csv")
psdFrame <- read_csv(psdFile)

#to make reactor names match, remove hyphens (thus i-CTB becomes iCTB)
psdFrame$Reactor<-gsub("-", "", psdFrame$Reactor)

#we only care about the iCTB and oCTB data for this
psdFrame %<>% filter(Reactor %in% c("iCTB","oCTB")) %>%
  arrange(Shear)

#let's add a new factor rpmGroup for ease of plotting
mapRPM <- function(rpm){
  if_else(rpm %in% c("50"),"SR ~ 025-s ",
    if_else(rpm %in% c("45","100"),"SR ~ 050-s",
      if_else(rpm %in% c("120","200"),"SR ~ 100-s ",
        if_else(rpm %in% c("240"),"SR ~ 240-s ",
          "Unknown RPM"))))
    }

psdFrame %<>% mutate(rpmGroup=mapRPM(RPM))

# (un)cache particle size data --------------------------
#comment/uncomment as appropriate
#used to save time reading files during EDA, the final publication run was done
#without caching
saveRDS(psdFrame,here("cached","CTBparticleSizeDataProcessed.rds"))
#psdFrame<-readRDS(here("cached","CTBparticleSizeDataProcessed.rds"))


#begin to generate the distribution plots----
#note that to get things just right a few hardcoded values have been put in here
#notably the position and content of text annotations, as well as some ylims
#set showplots to TRUE if you want plots to show up as well as be saved
showplots = FALSE

#list of how we should save plots can include png, eps
#Useful if you prefer to look at pngs while tweaking
#then save as eps for a journal
saveAs<-c("eps")

if(showplots){ CairoWin() }

#shear distribution plot ----
#set some default theme elements
theme_classic()
theme_update(panel.background = element_blank(),
             legend.position="none",
             axis.line.x = element_line(color="black", size = 1),
             axis.line.y = element_line(color="black", size = 1),
             axis.ticks.x = element_line(color="black", size = 1),
             axis.ticks.y = element_line(color="black", size = 1),
             text=element_text(size=18,color="black"),
             axis.text=element_text(size=18,color="black"),
             strip.background=element_blank(),
             strip.text=element_blank())

#directlabel gets grumpy when this isn't a factor
shearData %<>% mutate(RPM=`rotPM [rotations per min]`)
shearData$RPM <- as.factor(shearData$RPM)

p<-ggplot(shearData,
      aes(x=`spf4.sr [Shear rate]`, y=..density..,weight=volfrac,group=RPM,color=RPM,linetype=RPM)) + 
      geom_freqpoly(bins=75,size=1.2)+xlim(0,75)+facet_grid(reactor ~ .)
p<-p+xlab("Shear rate (1/s)")+ylab("Volume Fraction ")+ylim(0,0.5)
p<-p +annotate("text",y=.5,x=30,size=7,label=c("(a) iCTB - Shear by RPM ","(c) oCTB - Shear by RPM "))
p<-p + scale_color_manual(values=c("#ca0020", "#ca0020", "#1b7837", "#1b7837","#4575b4","#4575b4"))
p<-p + scale_linetype_manual(values=c(5,5,4,4,1,1))
pd<-direct.label(p,list(cex=1.5,dl.trans(y=y+.15),"top.bumptwice"))
if(showplots){
  pd
}

#particle distribution plot -----
#directlabel gets grumpy when this isn't a factor
psdFrame$RPM<-as.factor(psdFrame$RPM)



p2<-ggplot(psdFrame,
      aes(x=`Bin median`, y=`Volume Fraction`,group=RPM,color=RPM,linetype=RPM))
p2<-p2+geom_line(size=1.2)+ylim(0,.3)
p2<-p2+xlab("Particle Equivalent Diameter (μm)")
p2<-p2 +annotate("text",y=.3,x=140,size=7,label=c("(b) iCTB - Particle Size by RPM","(d) oCTB - Particle Size by RPM"))
p2<-p2 + scale_color_manual(values=c("#ca0020", "#ca0020", "#1b7837", "#1b7837","#4575b4","#4575b4"))
p2<-p2 + scale_linetype_manual(values=c(5,5,4,4,1,1))
p2<-p2 +facet_grid(Reactor ~ .)
p2d<-direct.label(p2,list(cex=1.4,dl.trans(y=y+.15),"top.bumptwice"))
if(showplots){ 

  p2d
}

if("eps" %in% saveAs){
  postscript(here("output","ctbDists.eps"),colormodel="cmyk")
  multiplot(pd,p2d,cols=2)
  dev.off()
}


# Determine weighted mode and median of shear -----------------------------


#determine the weighted median
#pretty sure I found this in the CNprep library
weighted.median <-
function(v,weights){
	weights<-weights[order(v)]
	v<-sort(v)
	sw<-sum(weights)
	return(v[which.min(abs(cumsum(weights)-0.5*sw))])
}


#determine the mode by using a plotrix weighted histogram and the midpoint of
#the highest peak
weighted.mode<-function(v,weights,breaks){
  hw=weighted.hist(v,weights, breaks=breaks)
  return((hw$breaks[which.max(hw$counts)]+hw$breaks[which.max(hw$counts)+1])/2)
}


#create a small dataframe summarizing the weighted mean and mode for
#preparing supporting information table
shear_averages<-shearData %>%
  group_by(reactor,flowSim,RPM) %>%
  summarize(median_w=weighted.median(`spf4.sr [Shear rate]`,volfrac),
            mode_w=weighted.mode(`spf4.sr [Shear rate]`,volfrac,250))

write.csv(shear_averages,here("output","weighted_shear_median_mode.csv"))

sink(here("sessionInfo","ctbDistributions.R.sessionInfo.txt"))
sessionInfo()
sink()
