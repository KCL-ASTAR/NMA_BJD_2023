

###################
# LIBRARIES
###################

require(tidyverse)
require(gemtc)
require(eoffice)
require(pcnetmeta)
require(dosresmeta)
library(meta)



par(ask=F)



#myfile <- "AD-NMA-2022-10-04_DP.xlsx"
myfile <- "AD-NMA-2023-05-18 March 2023 ABC.xlsx"


source("AD-NMA_read_data5.R")


# Filter data
# Remove results in Adolescents
# for continuous outcomes
DN <- filter(dn, study != "Pacor Italy 2004", !grepl("Adolescents", study))
# For binary outcomes
DB <- filter(db, !grepl("Adolescents", study))

table(DB$outcome)

##################################.
####
####   For symptoms
####


outc <- "poem"
path <- "POEM"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- -1 # Preferred Direction

# Pair-wise comparison meta-analysis
fpwc(DN, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageB"), TIM="s", AGE="B", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageC"), TIM="s", AGE="C", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageB"), TIM="s", AGE="B", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageC"), TIM="s", AGE="C", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageB"), TIM="s", AGE="B", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageC"), TIM="s", AGE="C", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageB"), TIM="s", AGE="B", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageC"), TIM="s", AGE="C", TOP=NULL, NLR=7, REP=NULL)



####   For node splitting  ####.

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP=NULL, NLR=NULL, REP=NULL)


fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP=NULL, NLR=7, REP=NULL)






##################################.
####
####   For  QoL
####

outc <- "qol"
path <- "QoL"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- -1 # Preferred Direction


# Pair-wise comparison meta-analysis
fpwc(DN, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageB"), TIM="s", AGE="B", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageC"), TIM="s", AGE="C", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb"), TIM="s", TOP=NULL, NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb_ageB"), TIM="s", AGE="B", TOP=NULL, NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb_ageC"), TIM="s", AGE="C", TOP=NULL, NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageB"), TIM="s", AGE="B", TOP=NULL, NLR=7, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageC"), TIM="s", AGE="C", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb"), TIM="s", TOP=NULL, NLR=7, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=7, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb_ageB"), TIM="s", AGE="B", TOP=NULL, NLR=7, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb_ageC"), TIM="s", AGE="C", TOP=NULL, NLR=7, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageB"), TIM="s", AGE="B", TOP="y", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageC"), TIM="s", AGE="C", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb"), TIM="s", TOP="y", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb_ageB"), TIM="s", AGE="B", TOP="y", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb_ageC"), TIM="s", AGE="C", TOP="y", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageB"), TIM="s", AGE="B", TOP="n", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageC"), TIM="s", AGE="C", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb"), TIM="s", TOP="n", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb_ageA"), TIM="s", AGE="A", TOP="n", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb_ageB"), TIM="s", AGE="B", TOP="n", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb_ageC"), TIM="s", AGE="C", TOP="n", NLR=NULL, REP="c")


####   For node splitting  ####.

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP=NULL, NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP=NULL, NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP=NULL, NLR=7, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb_ageA"), TMOD="nodesplit", AGE="A", TIM="s", TOP=NULL, NLR=7, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb_ageB"), TMOD="nodesplit", AGE="B", TIM="s", TOP=NULL, NLR=7, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb_ageC"), TMOD="nodesplit", AGE="C", TIM="s", TOP=NULL, NLR=7, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP="y", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb_ageA"), TMOD="nodesplit", AGE="A", TIM="s", TOP="y", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb_ageB"), TMOD="nodesplit", AGE="B", TIM="s", TOP="y", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb_ageC"), TMOD="nodesplit", AGE="C", TIM="s", TOP="y", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP="n", NLR=NULL, REP=NULL)

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb_ageB"), TMOD="nodesplit", TIM="s", AGE="B", TOP="n", NLR=NULL, REP="c")

#fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb_ageC"), TMOD="nodesplit", TIM="s", AGE="C", TOP="n", NLR=NULL, REP="c")
     
##################################.
####
####   For Signs
####

outc <- "easi"
path <- "EASI"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
# Pair-wise comparison meta-analysis
fpwc(DN, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)
PF <- -1 # Preferred Direction

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)


####   For node splitting  ####.     
     
fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)




##################################.
####
####   For Itch
####

outc <- "itch"
path <- "Itch"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- -1 # Preferred Direction


# Pair-wise comparison meta-analysis
fpwc(DN, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)


fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb"), TIM="s", TOP=NULL, NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb"), TIM="s", TOP=NULL, NLR=7, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb"), TIM="s", TOP="y", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb"), TIM="s", TOP="n", NLR=NULL, REP="c")

####   For node splitting  ####.   
     
fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_cfb"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_cfb"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_cfb"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP="c")

fnma(DN, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_cfb"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP="c")     
     

     
     
     
##################################.
####
####   For SAE
####


path <- "SAE"
outc <- "sae"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- -1 # Preferred Direction

fpwc(DB, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

####   For node splitting  ####.   

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)





##################################.
####
####   For WD
####


path <- "WD"
outc <- "wdr"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- -1 # Preferred Direction


fpwc(DB, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_adult"), AGE=c("adult"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

####   For node splitting  ####.   

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_adult"), AGE=c("adult"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)








##################################.
####
####   For IGA
####


path <- "IGA"
outc <- "iga"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- 1 # Preferred Direction


fpwc(DB, outc, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short_ageA", TIM="s",AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL, SUG="adults")

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TIM="s",AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TIM="s",AGE="A", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

#fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_adult"), AGE=c("adult"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

####   For node splitting  ####.   

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

#fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_adult"), AGE=c("adult"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)



##################################.
####
####   For EASI50
####


path <- "EASI50"
outc <- "easi50"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- 1 # Preferred Direction

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short_ageA", TIM="s",AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL, SUG="adults")

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TIM="s",AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TIM="s",AGE="A", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

####   For node splitting  ####.   

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

##################################
####
####   For EASI75
####


path <- "EASI75"
outc <- "easi75"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- 1 # Preferred Direction

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short_ageA", TIM="s",AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL, SUG="adults")

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TIM="s",AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TIM="s",AGE="A", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

####   For node splitting  ####.   

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)





##################################.
####
####   For EASI90
####


path <- "EASI90"
outc <- "easi90"
outc <- arrange(filter(dc, !is.na(get(outc))), get(outc))$outcome
PF <- 1 # Preferred Direction

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short", TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fpwc(DB, outc, PTH="PAIRWISE", PRE="_short_ageA", TIM="s",AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL)

#fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TIM="s", TOP=NULL, NLR=NULL, REP=NULL, SUG="adults")

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TIM="s",AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TIM="s", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TIM="s",AGE="A", TOP="n", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)

####   For node splitting  ####.   

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB"), TMOD="nodesplit", TIM="s", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_lowRoB_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP=NULL, NLR=7, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical"), TMOD="nodesplit", TIM="s", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_notopical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="n", NLR=NULL, REP=NULL) #

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical"), TMOD="nodesplit", TIM="s", TOP="y", NLR=NULL, REP=NULL)

fnma(DB, outc, DIR=PF, PTH=path, PRE=paste0(path,"_short_topical_ageA"), TMOD="nodesplit", TIM="s", AGE="A", TOP="y", NLR=NULL, REP=NULL)


##################################.
####
####   For SMD
####

# read all SMD domains
smds <- grep("SMD", names(dc), value=T)
PF <- -1 # Preferred Direction

# for each domain run the analysis
for(v in smds[-1]){
   outc <- arrange(filter(dc, !is.na(get(v))), get(v))$outcome
   fnma(DN, outc, DIR=PF, PTH=v, PRE=paste0(v,"_short"), SMD=T, TIM="s")
   fnma(DN, outc, DIR=PF, PTH=v, PRE=paste0(v,"_short"), SMD=T, TIM="s", TMOD="nodesplit")
}


## Pairwise analysis
for(v in smds[-1]){
  path <- v
  outc <- arrange(filter(dc, !is.na(get(v))), get(v))$outcome
  fpwc(DN, outc, DIR=PF, PTH="PAIRWISE", PRE="_short", TIM="s", SMD=T)
}








