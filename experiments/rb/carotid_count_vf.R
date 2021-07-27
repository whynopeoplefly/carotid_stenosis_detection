###################################################################################
###This program is designed by Hsu KAI-CHENG for the classification of angiography report.
###Please contact Dr. HSU(e-mail: edwardfirst@gmail.com) for further discussion.
###################################################################################
carotid2017_v1_test2 <- read_excel("~/R language/carotid2017-v1-test.xlsx")
data <- carotid2017_v1_test
install.packages('stringr')
library(stringi)
library(stringr)
rm(carotid2017_v1_test)
ptno<-nrow(data)

content1 <- data$CONTENT
content2 <- stri_replace_all_regex(content1, " ","")
content3 <- stri_replace_all_regex(content2, "[\r\n]","")
data <- cbind(content3,data)
colnames(data)[1]<-"SCONTENT"

######################Stastistics############################
b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
b1 <- str_detect(data$SCONTENT, "CTAshow")
summary(b1)
b2 <- str_detect(data$SCONTENT, "MRAshow")
summary(b2)
b3 <- str_detect(data$SCONTENT, "DSAshow")
summary(b2)

c <- str_detect(data$SCONTENT, ">50%")
summary(c)

d <- str_detect(data$SCONTENT, "significant")
summary(d)

e1 <- str_detect(data$SCONTENT, ">=50%stenosis")
summary(e1)

e2 <- str_detect(data$SCONTENT, ">=50%narrowing")
summary(e2)

e3 <- str_detect(data$SCONTENT, ">50%stenosis")
summary(e3)

e4 <- str_detect(data$SCONTENT, "significantstenosis")
summary(e4)

e5 <- str_detect(data$SCONTENT, "significantnarrowing")
summary(e5)

e6 <- str_detect(data$SCONTENT, "tightstenosis")
summary(e6)

e7 <- str_detect(data$SCONTENT, "tightnarrowing")
summary(e7)

e8 <- str_detect(data$SCONTENT, "severestenosis")
summary(e8)

e9 <- str_detect(data$SCONTENT, "severenarrowing")
summary(e9)

e10 <- str_detect(data$SCONTENT, "highgradestenosis")
summary(e10)

e11 <- str_detect(data$SCONTENT, "highgradenarrowing")
summary(e11)

e12 <- str_detect(data$SCONTENT, "high-gradestenosis")
summary(e12)

e13 <- str_detect(data$SCONTENT, "high-gradenarrowing")
summary(e13)

e14 <- str_detect(data$SCONTENT, "highdegreestenosis")
summary(e14)

e15 <- str_detect(data$SCONTENT, "highdegreenarrowing")
summary(e15)

e16 <- str_detect(data$SCONTENT, "high-degreestenosis")
summary(e16)

e17 <- str_detect(data$SCONTENT, "high-degreenarrowing")
summary(e17)

e18 <- str_detect(data$SCONTENT, "prominentstenosis")
summary(e18)

e19 <- str_detect(data$SCONTENT, "prominentnarrowing")
summary(e19)

e20 <- str_detect(data$SCONTENT, "criticalstenosis")
summary(e20)

e21 <- str_detect(data$SCONTENT, "criticalnarrowing")
summary(e21)

#############Start Calculation###################
b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
summary(data$SCONTENT)
for(i in 1:ptno){
  if (b[i] == TRUE){
    data$Table[i]<-1}
  else {data$Table[i]<-0}
}
tablecount <- str_detect(data$Table, "1")
summary(tablecount)


######################################################################
#Exclude >50% Stenosis
stringstenosis1=c("CCA:", "Extra-ICA:", " Intra-ICA:", "MCA:", "Extra-VA:",
                  "Int-VA:", "Intra-VA:", "BA","Arterialstenosismeasurement",
                  ">50%", "significant", ">=50%stenosis", ">=50%narrowing", 
                  ">50%stenosis", "significantstenosis", "significantnarrowing", 
                  "tightstenosis", "tightnarrowing", "severestenosis", 
                  "severenarrowing", "highgradestenosis", "highgradenarrowing", 
                  "high-gradestenosis", "high-gradenarrowing", 
                  "highdegreestenosis", "highdegreenarrowing", 
                  "high-degreestenosis", "high-degreenarrowing", 
                  "prominentstenosis", "prominentnarrowing", "criticalstenosis", 
                  "criticalnarrowing", "visual", "opacification", "opacity", 
                  "absence", "occlusion", "obstruction")
stenosis1 <- str_detect(data$SCONTENT, paste(stringstenosis1, collapse = '|'))
summary(stenosis1)

for(i in 1:ptno){
  if (stenosis1[i] == FALSE){
    data$NoStenosis[i]<-1}
  else{data$NoStenosis[i]<-9}
}

for(i in 1:ptno){
  if (stenosis1[i] == FALSE){
    data$RCCA[i]<-1; data$REICA[i]<-1; data$RIICA[i]<-1; data$RACA[i]<-1; 
    data$RMCA[i]<-1; data$RPCA[i]<-1; data$REVA[i]<-1; data$RIVA[i]<-1; 
    data$BA[i]<-1; data$LCCA[i]<-1; data$LEICA[i]<-1; data$LIICA[i]<-1; 
    data$LACA[i]<-1; data$LMCA[i]<-1; data$LPCA[i]<-1; data$LEVA[i]<-1; 
    data$LIVA[i]<-1}
}

stringstenosis2=c("nosignificantstenosis", "withoutsignificantstenosis",
"noobviousextra/intra-cranialarterialstenosis,cerebralaneurysmorvascularmalformation.nosignificantintracranialarterialstenosis", 
"nosignificantarterialstenosisorocclusion", "no evidenceofsignificantstenosis,occlusion", "nodefiniteevidenceofarterialstenosis")
stenosis2 <- str_detect(data$SCONTENT, paste(stringstenosis2, collapse = '|'))
summary(stenosis2)

for(i in 1:ptno){
  if (stenosis2[i] == TRUE){
    data$nowithout[i]<-1}
  else{data$nowithout[i]<-9}
}

for(i in 1:ptno){
  if (stenosis2[i] == TRUE){
    data$RCCA[i]<-1; data$REICA[i]<-1; data$RIICA[i]<-1; data$RACA[i]<-1; 
    data$RMCA[i]<-1; data$RPCA[i]<-1; data$REVA[i]<-1; data$RIVA[i]<-1; 
    data$BA[i]<-1; data$LCCA[i]<-1; data$LEICA[i]<-1; data$LIICA[i]<-1; 
    data$LACA[i]<-1; data$LMCA[i]<-1; data$LPCA[i]<-1; data$LEVA[i]<-1; 
    data$LIVA[i]<-1}
}

###################################################################
#CCA
#Right CCA
stringrcca1=c("CCA:Rt:.%", "CCA:Rt:0%", "CCA:Rt:<50%")
rcca1 <- str_detect(data$SCONTENT, paste(stringrcca1, collapse = '|'))
summary(rcca1)

for(i in 1:ptno){
  if (rcca1[i] == TRUE){
    data$RCCA[i]<-1
  }
}

stringrcca2=c("CCA:Rt:>50%", "CCA:Rt:100%")
rcca2 <- str_detect(data$SCONTENT, paste(stringrcca2, collapse = '|'))
summary(rcca2)

for(i in 1:ptno){
  if (rcca2[i] == TRUE){
    data$RCCA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
rcca3 <- str_detect(data$SCONTENT, "CCA:Rt:")
summary(rcca3)

for(i in 1:ptno){
  if (b[i] == TRUE & rcca3[i] == FALSE){
    data$RCCA[i]<-1
  }
}

rccadata1 <- str_detect(data$RCCA, "1")
summary(rccadata1)
rccadata2 <- str_detect(data$RCCA, "2")
summary(rccadata2)


#Left CCA
stringlcca1=c("CCA:Rt:.%Lt:.%", "CCA:Rt:.%Lt:0%","CCA:Rt:.%Lt:<50%", 
         "CCA:Rt:0%Lt:.%", "CCA:Rt:0%Lt:0%","CCA:Rt:0%Lt:<50%",
        "CCA:Rt:<50%Lt:.%","CCA:Rt:<50%Lt:0%", "CCA:Rt:<50%Lt:<50%",
        "CCA:Rt:>50%Lt:.%", "CCA:Rt:>50%Lt:0%","CCA:Rt:>50%Lt:<50%", 
        "CCA:Rt:100%Lt:.%", "CCA:Rt:100%Lt:0%", "CCA:Rt:100%Lt:<50%")
lcca1 <- str_detect(data$SCONTENT, paste(stringlcca1, collapse = '|'))
summary(lcca1)

for(i in 1:ptno){
  if (lcca1[i] == TRUE){
    data$LCCA[i]<-1
  }
}

stringlcca2=c("CCA:Rt:.%Lt:>50%", "CCA:Rt:.%Lt:100%",
             "CCA:Rt:0%Lt:>50%", "CCA:Rt:0%Lt:100%", 
             "CCA:Rt:<50%Lt:>50%", "CCA:Rt:<50%Lt:100%",
             "CCA:Rt:>50%Lt:>50%", "CCA:Rt:>50%Lt:100%", 
             "CCA:Rt:100%Lt:>50%", "CCA:Rt:100%Lt:100%")
lcca2 <- str_detect(data$SCONTENT, paste(stringlcca2, collapse = '|'))
summary(lcca2)

for(i in 1:ptno){
  if (lcca2[i] == TRUE){
    data$LCCA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
lcca3 <- str_detect(data$SCONTENT, "CCA:Rt:")
summary(lcca3)

for(i in 1:ptno){
  if (b[i] == TRUE & lcca3[i] == FALSE){
    data$LCCA[i]<-1
  }
}


lccadata1 <- str_detect(data$LCCA, "1")
summary(lccadata1)
lccadata2 <- str_detect(data$LCCA, "2")
summary(lccadata2)


####################################################################
#Extra-ICA
#Right extra-ICA
stringreica1=c("Extra-ICA:Rt:.%", "Extra-ICA:Rt:0%", "Extra-ICA:Rt:<50%")
reica1 <- str_detect(data$SCONTENT, paste(stringreica1, collapse = '|'))
summary(reica1)

for(i in 1:ptno){
  if (reica1[i] == TRUE){
    data$REICA[i]<-1
  }
}

stringreica2=c("Extra-ICA:Rt:>50%", "Extra-ICA:Rt:100%")
reica2 <- str_detect(data$SCONTENT, paste(stringreica2, collapse = '|'))
summary(reica2)

for(i in 1:ptno){
  if (reica2[i] == TRUE){
    data$REICA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
reica3 <- str_detect(data$SCONTENT, "Extra-ICA:Rt:")
summary(reica3)

for(i in 1:ptno){
  if (b[i] == TRUE & reica3[i] == FALSE){
    data$REICA[i]<-1
  }
}

reicadata1 <- str_detect(data$REICA, "1")
summary(reicadata1)
reicadata2 <- str_detect(data$REICA, "2")
summary(reicadata2)

#Left extra-ICA
stringleica1=c("Extra-ICA:Rt:.%Lt:.%", "Extra-ICA:Rt:.%Lt:0%","Extra-ICA:Rt:.%Lt:<50%", 
              "Extra-ICA:Rt:0%Lt:.%", "Extra-ICA:Rt:0%Lt:0%","Extra-ICA:Rt:0%Lt:<50%",
              "Extra-ICA:Rt:<50%Lt:.%","Extra-ICA:Rt:<50%Lt:0%", "Extra-ICA:Rt:<50%Lt:<50%",
              "Extra-ICA:Rt:>50%Lt:.%", "Extra-ICA:Rt:>50%Lt:0%", "Extra-ICA:Rt:>50%Lt:<50%", 
              "Extra-ICA:Rt:100%Lt:.%", "Extra-ICA:Rt:100%Lt:0%", "Extra-ICA:Rt:100%Lt:<50%")
leica1 <- str_detect(data$SCONTENT, paste(stringleica1, collapse = '|'))
summary(leica1)

for(i in 1:ptno){
  if (leica1[i] == TRUE){
    data$LEICA[i]<-1
  }
}

stringleica2=c("Extra-ICA:Rt:.%Lt:>50%", "Extra-ICA:Rt:.%Lt:100%",
              "Extra-ICA:Rt:0%Lt:>50%", "Extra-ICA:Rt:0%Lt:100%", 
              "Extra-ICA:Rt:<50%Lt:>50%", "Extra-ICA:Rt:<50%Lt:100%",
              "Extra-ICA:Rt:>50%Lt:>50%", "Extra-ICA:Rt:>50%Lt:100%", 
              "Extra-ICA:Rt:100%Lt:>50%", "Extra-ICA:Rt:100%Lt:100%")
leica2 <- str_detect(data$SCONTENT, paste(stringleica2, collapse = '|'))
summary(leica2)

for(i in 1:ptno){
  if (leica2[i] == TRUE){
    data$LEICA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
leica3 <- str_detect(data$SCONTENT, "Extra-ICA:Rt:")
summary(leica3)

for(i in 1:ptno){
  if (b[i] == TRUE & leica3[i] == FALSE){
    data$LEICA[i]<-1
  }
}


leicadata1 <- str_detect(data$LEICA, "1")
summary(leicadata1)
leicadata2 <- str_detect(data$LEICA, "2")
summary(leicadata2)


####################################################################
#Intra-ICA
#Right intra-ICA
stringriica1=c("Intra-ICA:Rt:.%", "Intra-ICA:Rt:0%", "Intra-ICA:Rt:<50%")
riica1 <- str_detect(data$SCONTENT, paste(stringriica1, collapse = '|'))
summary(riica1)

for(i in 1:ptno){
  if (riica1[i] == TRUE){
    data$RIICA[i]<-1
  }
}

stringriica2=c("Intra-ICA:Rt:>50%", "Intra-ICA:Rt:100%")
riica2 <- str_detect(data$SCONTENT, paste(stringriica2, collapse = '|'))
summary(riica2)

for(i in 1:ptno){
  if (riica2[i] == TRUE){
    data$RIICA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
riica3 <- str_detect(data$SCONTENT, "Intra-ICA:Rt:")
summary(riica3)

for(i in 1:ptno){
  if (b[i] == TRUE & riica3[i] == FALSE){
    data$RIICA[i]<-1
  }
}

riicadata1 <- str_detect(data$RIICA, "1")
summary(riicadata1)
riicadata2 <- str_detect(data$RIICA, "2")
summary(riicadata2)

#Left intra-ICA
stringliica1=c("Intra-ICA:Rt:.%Lt:.%", "Intra-ICA:Rt:.%Lt:0%","Intra-ICA:Rt:.%Lt:<50%", 
               "Intra-ICA:Rt:0%Lt:.%", "Intra-ICA:Rt:0%Lt:0%","Intra-ICA:Rt:0%Lt:<50%",
               "Intra-ICA:Rt:<50%Lt:.%","Intra-ICA:Rt:<50%Lt:0%", "Intra-ICA:Rt:<50%Lt:<50%",
               "Intra-ICA:Rt:>50%Lt:.%", "Intra-ICA:Rt:>50%Lt:0%", "Intra-ICA:Rt:>50%Lt:<50%", 
               "Intra-ICA:Rt:100%Lt:.%", "Intra-ICA:Rt:100%Lt:0%", "Intra-ICA:Rt:100%Lt:<50%")
liica1 <- str_detect(data$SCONTENT, paste(stringliica1, collapse = '|'))
summary(liica1)

for(i in 1:ptno){
  if (liica1[i] == TRUE){
    data$LIICA[i]<-1
  }
}

stringliica2=c("Intra-ICA:Rt:.%Lt:>50%", "Intra-ICA:Rt:.%Lt:100%",
               "Intra-ICA:Rt:0%Lt:>50%", "Intra-ICA:Rt:0%Lt:100%", 
               "Intra-ICA:Rt:<50%Lt:>50%", "Intra-ICA:Rt:<50%Lt:100%",
               "Intra-ICA:Rt:>50%Lt:>50%", "Intra-ICA:Rt:>50%Lt:100%", 
               "Intra-ICA:Rt:100%Lt:>50%", "Intra-ICA:Rt:100%Lt:100%")
liica2 <- str_detect(data$SCONTENT, paste(stringliica2, collapse = '|'))
summary(liica2)

for(i in 1:ptno){
  if (liica2[i] == TRUE){
    data$LIICA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)

liica3 <- str_detect(data$SCONTENT, "Intra-ICA:Rt:")
summary(liica3)

for(i in 1:ptno){
  if (b[i] == TRUE & liica3[i] == FALSE){
    data$LIICA[i]<-1
  }
}


liicadata1 <- str_detect(data$LIICA, "1")
summary(liicadata1)
liicadata2 <- str_detect(data$LIICA, "2")
summary(liicadata2)

###################################################################
#ACA
#Right ACA
stringraca1=c("ACA:Rt:.%", "ACA:Rt:0%", "ACA:Rt:<50%")
raca1 <- str_detect(data$SCONTENT, paste(stringraca1, collapse = '|'))
summary(raca1)

for(i in 1:ptno){
  if (raca1[i] == TRUE){
    data$RACA[i]<-1
  }
}

stringraca2=c("ACA:Rt:>50%", "ACA:Rt:100%")
raca2 <- str_detect(data$SCONTENT, paste(stringraca2, collapse = '|'))
summary(raca2)

for(i in 1:ptno){
  if (raca2[i] == TRUE){
    data$RACA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
raca3 <- str_detect(data$SCONTENT, "ACA:Rt:")
summary(raca3)

for(i in 1:ptno){
  if (b[i] == TRUE & raca3[i] == FALSE){
    data$RACA[i]<-1
  }
}

racadata1 <- str_detect(data$RACA, "1")
summary(racadata1)
racadata2 <- str_detect(data$RACA, "2")
summary(racadata2)

#Left ACA
stringlaca1=c("ACA:Rt:.%Lt:.%", "ACA:Rt:.%Lt:0%","ACA:Rt:.%Lt:<50%", 
              "ACA:Rt:0%Lt:.%", "ACA:Rt:0%Lt:0%","ACA:Rt:0%Lt:<50%",
              "ACA:Rt:<50%Lt:.%","ACA:Rt:<50%Lt:0%", "ACA:Rt:<50%Lt:<50%",
              "ACA:Rt:>50%Lt:.%", "ACA:Rt:>50%Lt:0%", "ACA:Rt:>50%Lt:<50%", 
              "ACA:Rt:100%Lt:.%", "ACA:Rt:100%Lt:0%", "ACA:Rt:100%Lt:<50%")
laca1 <- str_detect(data$SCONTENT, paste(stringlaca1, collapse = '|'))
summary(laca1)

for(i in 1:ptno){
  if (laca1[i] == TRUE){
    data$LACA[i]<-1
  }
}

stringlaca2=c("ACA:Rt:.%Lt:>50%", "ACA:Rt:.%Lt:100%",
              "ACA:Rt:0%Lt:>50%", "ACA:Rt:0%Lt:100%", 
              "ACA:Rt:<50%Lt:>50%", "ACA:Rt:<50%Lt:100%",
              "ACA:Rt:>50%Lt:>50%", "ACA:Rt:>50%Lt:100%", 
              "ACA:Rt:100%Lt:>50%", "ACA:Rt:100%Lt:100%")
laca2 <- str_detect(data$SCONTENT, paste(stringlaca2, collapse = '|'))
summary(laca2)

for(i in 1:ptno){
  if (laca2[i] == TRUE){
    data$LACA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
laca3 <- str_detect(data$SCONTENT, "ACA:Rt:")
summary(laca3)

for(i in 1:ptno){
  if (b[i] == TRUE & laca3[i] == FALSE){
    data$LACA[i]<-1
  }
}


lacadata1 <- str_detect(data$LACA, "1")
summary(lacadata1)
lacadata2 <- str_detect(data$LACA, "2")
summary(lacadata2)

###################################################################
#MCA
#Right MCA
stringrmca1=c("MCA:Rt:.%", "MCA:Rt:0%", "MCA:Rt:<50%")
rmca1 <- str_detect(data$SCONTENT, paste(stringrmca1, collapse = '|'))
summary(rmca1)

for(i in 1:ptno){
  if (rmca1[i] == TRUE){
    data$RMCA[i]<-1
  }
}

stringrmca2=c("MCA:Rt:>50%", "MCA:Rt:100%")
rmca2 <- str_detect(data$SCONTENT, paste(stringrmca2, collapse = '|'))
summary(rmca2)

for(i in 1:ptno){
  if (rmca2[i] == TRUE){
    data$RMCA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
rmca3 <- str_detect(data$SCONTENT, "MCA:Rt:")
summary(rmca3)

for(i in 1:ptno){
  if (b[i] == TRUE & rmca3[i] == FALSE){
    data$RMCA[i]<-1
  }
}

rmcadata1 <- str_detect(data$RMCA, "1")
summary(rmcadata1)
rmcadata2 <- str_detect(data$RMCA, "2")
summary(rmcadata2)

#Left MCA
stringlmca1=c("MCA:Rt:.%Lt:.%", "MCA:Rt:.%Lt:0%","MCA:Rt:.%Lt:<50%", 
              "MCA:Rt:0%Lt:.%", "MCA:Rt:0%Lt:0%","MCA:Rt:0%Lt:<50%",
              "MCA:Rt:<50%Lt:.%","MCA:Rt:<50%Lt:0%", "MCA:Rt:<50%Lt:<50%",
              "MCA:Rt:>50%Lt:.%", "MCA:Rt:>50%Lt:0%", "MCA:Rt:>50%Lt:<50%", 
              "MCA:Rt:100%Lt:.%", "MCA:Rt:100%Lt:0%", "MCA:Rt:100%Lt:<50%")
lmca1 <- str_detect(data$SCONTENT, paste(stringlmca1, collapse = '|'))
summary(lmca1)

for(i in 1:ptno){
  if (lmca1[i] == TRUE){
    data$LMCA[i]<-1
  }
}

stringlmca2=c("MCA:Rt:.%Lt:>50%", "MCA:Rt:.%Lt:100%",
              "MCA:Rt:0%Lt:>50%", "MCA:Rt:0%Lt:100%", 
              "MCA:Rt:<50%Lt:>50%", "MCA:Rt:<50%Lt:100%",
              "MCA:Rt:>50%Lt:>50%", "MCA:Rt:>50%Lt:100%", 
              "MCA:Rt:100%Lt:>50%", "MCA:Rt:100%Lt:100%")
lmca2 <- str_detect(data$SCONTENT, paste(stringlmca2, collapse = '|'))
summary(lmca2)

for(i in 1:ptno){
  if (lmca2[i] == TRUE){
    data$LMCA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
lmca3 <- str_detect(data$SCONTENT, "MCA:Rt:")
summary(lmca3)

for(i in 1:ptno){
  if (b[i] == TRUE & lmca3[i] == FALSE){
    data$LMCA[i]<-1
  }
}


lmcadata1 <- str_detect(data$LMCA, "1")
summary(lmcadata1)
lmcadata2 <- str_detect(data$LMCA, "2")
summary(lmcadata2)

###################################################################
#PCA
#Right PCA
stringrpca1=c("PCA:Rt:.%", "PCA:Rt:0%", "PCA:Rt:<50%")
rpca1 <- str_detect(data$SCONTENT, paste(stringrpca1, collapse = '|'))
summary(rpca1)

for(i in 1:ptno){
  if (rpca1[i] == TRUE){
    data$RPCA[i]<-1
  }
}

stringrpca2=c("PCA:Rt:>50%", "PCA:Rt:100%")
rpca2 <- str_detect(data$SCONTENT, paste(stringrpca2, collapse = '|'))
summary(rpca2)

for(i in 1:ptno){
  if (rpca2[i] == TRUE){
    data$RPCA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
rpca3 <- str_detect(data$SCONTENT, "PCA:Rt:")
summary(rpca3)

for(i in 1:ptno){
  if (b[i] == TRUE & rpca3[i] == FALSE){
    data$RPCA[i]<-1
  }
}

rpcadata1 <- str_detect(data$RPCA, "1")
summary(rpcadata1)
rpcadata2 <- str_detect(data$RPCA, "2")
summary(rpcadata2)

#Left PCA
stringlpca1=c("PCA:Rt:.%Lt:.%", "PCA:Rt:.%Lt:0%","PCA:Rt:.%Lt:<50%", 
              "PCA:Rt:0%Lt:.%", "PCA:Rt:0%Lt:0%","PCA:Rt:0%Lt:<50%",
              "PCA:Rt:<50%Lt:.%","PCA:Rt:<50%Lt:0%", "PCA:Rt:<50%Lt:<50%",
              "PCA:Rt:>50%Lt:.%", "PCA:Rt:>50%Lt:0%", "PCA:Rt:>50%Lt:<50%", 
              "PCA:Rt:100%Lt:.%", "PCA:Rt:100%Lt:0%", "PCA:Rt:100%Lt:<50%")
lpca1 <- str_detect(data$SCONTENT, paste(stringlpca1, collapse = '|'))
summary(lpca1)

for(i in 1:ptno){
  if (lpca1[i] == TRUE){
    data$LPCA[i]<-1
  }
}

stringlpca2=c("PCA:Rt:.%Lt:>50%", "PCA:Rt:.%Lt:100%",
              "PCA:Rt:0%Lt:>50%", "PCA:Rt:0%Lt:100%", 
              "PCA:Rt:<50%Lt:>50%", "PCA:Rt:<50%Lt:100%",
              "PCA:Rt:>50%Lt:>50%", "PCA:Rt:>50%Lt:100%", 
              "PCA:Rt:100%Lt:>50%", "PCA:Rt:100%Lt:100%")
lpca2 <- str_detect(data$SCONTENT, paste(stringlpca2, collapse = '|'))
summary(lpca2)

for(i in 1:ptno){
  if (lpca2[i] == TRUE){
    data$LPCA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
lpca3 <- str_detect(data$SCONTENT, "PCA:Rt:")
summary(lpca3)

for(i in 1:ptno){
  if (b[i] == TRUE & lpca3[i] == FALSE){
    data$LPCA[i]<-1
  }
}


lpcadata1 <- str_detect(data$LPCA, "1")
summary(lpcadata1)
lpcadata2 <- str_detect(data$LPCA, "2")
summary(lpcadata2)

####################################################################
#Extra-VA
#Right extra-VA
stringreva1=c("Extra-VA:Rt:.%", "Extra-VA:Rt:0%", "Extra-VA:Rt:<50%")
reva1 <- str_detect(data$SCONTENT, paste(stringreva1, collapse = '|'))
summary(reva1)

for(i in 1:ptno){
  if (reva1[i] == TRUE){
    data$REVA[i]<-1
  }
}

stringreva2=c("Extra-VA:Rt:>50%", "Extra-VA:Rt:100%")
reva2 <- str_detect(data$SCONTENT, paste(stringreva2, collapse = '|'))
summary(reva2)

for(i in 1:ptno){
  if (reva2[i] == TRUE){
    data$REVA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
reva3 <- str_detect(data$SCONTENT, "Extra-VA:Rt:")
summary(reva3)

for(i in 1:ptno){
  if (b[i] == TRUE & reva3[i] == FALSE){
    data$REVA[i]<-1
  }
}

revadata1 <- str_detect(data$REVA, "1")
summary(revadata1)
revadata2 <- str_detect(data$REVA, "2")
summary(revadata2)

#Left extra-VA
stringleva1=c("Extra-VA:Rt:.%Lt:.%", "Extra-VA:Rt:.%Lt:0%","Extra-VA:Rt:.%Lt:<50%", 
               "Extra-VA:Rt:0%Lt:.%", "Extra-VA:Rt:0%Lt:0%","Extra-VA:Rt:0%Lt:<50%",
               "Extra-VA:Rt:<50%Lt:.%","Extra-VA:Rt:<50%Lt:0%", "Extra-VA:Rt:<50%Lt:<50%",
               "Extra-VA:Rt:>50%Lt:.%", "Extra-VA:Rt:>50%Lt:0%", "Extra-VA:Rt:>50%Lt:<50%", 
               "Extra-VA:Rt:100%Lt:.%", "Extra-VA:Rt:100%Lt:0%", "Extra-VA:Rt:100%Lt:<50%")
leva1 <- str_detect(data$SCONTENT, paste(stringleva1, collapse = '|'))
summary(leva1)

for(i in 1:ptno){
  if (leva1[i] == TRUE){
    data$LEVA[i]<-1
  }
}

stringleva2=c("Extra-VA:Rt:.%Lt:>50%", "Extra-VA:Rt:.%Lt:100%",
               "Extra-VA:Rt:0%Lt:>50%", "Extra-VA:Rt:0%Lt:100%", 
               "Extra-VA:Rt:<50%Lt:>50%", "Extra-VA:Rt:<50%Lt:100%",
               "Extra-VA:Rt:>50%Lt:>50%", "Extra-VA:Rt:>50%Lt:100%", 
               "Extra-VA:Rt:100%Lt:>50%", "Extra-VA:Rt:100%Lt:100%")
leva2 <- str_detect(data$SCONTENT, paste(stringleva2, collapse = '|'))
summary(leva2)

for(i in 1:ptno){
  if (leva2[i] == TRUE){
    data$LEVA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
leva3 <- str_detect(data$SCONTENT, "Extra-VA:Rt:")
summary(leva3)

for(i in 1:ptno){
  if (b[i] == TRUE & leva3[i] == FALSE){
    data$LEVA[i]<-1
  }
}


levadata1 <- str_detect(data$LEVA, "1")
summary(levadata1)
levadata2 <- str_detect(data$LEVA, "2")
summary(levadata2)

####################################################################
#Intra-VA
#Right intra-VA
stringriva1=c("Intra-VA:Rt:.%", "Intra-VA:Rt:0%", "Intra-VA:Rt:<50%",
              "Int-VA:Rt:.%", "Int-VA:Rt:0%", "Int-VA:Rt:<50%")
riva1 <- str_detect(data$SCONTENT, paste(stringriva1, collapse = '|'))
summary(riva1)

for(i in 1:ptno){
  if (riva1[i] == TRUE){
    data$RIVA[i]<-1
  }
}

stringriva2=c("Intra-VA:Rt:>50%", "Intra-VA:Rt:100%",
              "Int-VA:Rt:>50%", "Int-VA:Rt:100%")
riva2 <- str_detect(data$SCONTENT, paste(stringriva2, collapse = '|'))
summary(riva2)

for(i in 1:ptno){
  if (riva2[i] == TRUE){
    data$RIVA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
stringriva3=c("Intra-VA:Rt:", "Int-VA:Rt:")
riva3 <- str_detect(data$SCONTENT, paste(stringriva3, collapse = '|'))
summary(riva3)

for(i in 1:ptno){
  if (b[i] == TRUE & riva3[i] == FALSE){
    data$RIVA[i]<-1
  }
}

rivadata1 <- str_detect(data$RIVA, "1")
summary(rivadata1)
rivadata2 <- str_detect(data$RIVA, "2")
summary(rivadata2)

#Left intra-VA
stringliva1=c("Intra-VA:Rt:.%Lt:.%", "Intra-VA:Rt:.%Lt:0%","Intra-VA:Rt:.%Lt:<50%", 
               "Intra-VA:Rt:0%Lt:.%", "Intra-VA:Rt:0%Lt:0%","Intra-VA:Rt:0%Lt:<50%",
               "Intra-VA:Rt:<50%Lt:.%","Intra-VA:Rt:<50%Lt:0%", "Intra-VA:Rt:<50%Lt:<50%",
               "Intra-VA:Rt:>50%Lt:.%", "Intra-VA:Rt:>50%Lt:0%", "Intra-VA:Rt:>50%Lt:<50%", 
               "Intra-VA:Rt:100%Lt:.%", "Intra-VA:Rt:100%Lt:0%", "Intra-VA:Rt:100%Lt:<50%", 
              "Int-VA:Rt:.%Lt:.%", "Int-VA:Rt:.%Lt:0%","Int-VA:Rt:.%Lt:<50%", 
              "Int-VA:Rt:0%Lt:.%", "Int-VA:Rt:0%Lt:0%","Int-VA:Rt:0%Lt:<50%",
              "Int-VA:Rt:<50%Lt:.%","Int-VA:Rt:<50%Lt:0%", "Int-VA:Rt:<50%Lt:<50%",
              "Int-VA:Rt:>50%Lt:.%", "Int-VA:Rt:>50%Lt:0%", "Int-VA:Rt:>50%Lt:<50%", 
              "Int-VA:Rt:100%Lt:.%", "Int-VA:Rt:100%Lt:0%", "Int-VA:Rt:100%Lt:<50%")
liva1 <- str_detect(data$SCONTENT, paste(stringliva1, collapse = '|'))
summary(liva1)

for(i in 1:ptno){
  if (liva1[i] == TRUE){
    data$LIVA[i]<-1
  }
}

stringliva2=c("Intra-VA:Rt:.%Lt:>50%", "Intra-VA:Rt:.%Lt:100%",
               "Intra-VA:Rt:0%Lt:>50%", "Intra-VA:Rt:0%Lt:100%", 
               "Intra-VA:Rt:<50%Lt:>50%", "Intra-VA:Rt:<50%Lt:100%",
               "Intra-VA:Rt:>50%Lt:>50%", "Intra-VA:Rt:>50%Lt:100%", 
               "Intra-VA:Rt:100%Lt:>50%", "Intra-VA:Rt:100%Lt:100%",
              "Int-VA:Rt:.%Lt:>50%", "Int-VA:Rt:.%Lt:100%",
              "Int-VA:Rt:0%Lt:>50%", "Int-VA:Rt:0%Lt:100%", 
              "Int-VA:Rt:<50%Lt:>50%", "Int-VA:Rt:<50%Lt:100%",
              "Int-VA:Rt:>50%Lt:>50%", "Int-VA:Rt:>50%Lt:100%", 
              "Int-VA:Rt:100%Lt:>50%", "Int-VA:Rt:100%Lt:100%")
liva2 <- str_detect(data$SCONTENT, paste(stringliva2, collapse = '|'))
summary(liva2)

for(i in 1:ptno){
  if (liva2[i] == TRUE){
    data$LIVA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
stringliva3=c("Intra-VA:Rt:", "Int-VA:Rt:")
liva3 <- str_detect(data$SCONTENT, paste(stringliva3, collapse = '|'))
summary(liva3)

for(i in 1:ptno){
  if (b[i] == TRUE & liva3[i] == FALSE){
    data$LIVA[i]<-1
  }
}


livadata1 <- str_detect(data$LIVA, "1")
summary(livadata1)
livadata2 <- str_detect(data$LIVA, "2")
summary(livadata2)

###################################################################
#BA
stringba1=c("BA:.%", "BA:0%", "BA:<50%")
ba1 <- str_detect(data$SCONTENT, paste(stringba1, collapse = '|'))
summary(ba1)

for(i in 1:ptno){
  if (ba1[i] == TRUE){
    data$BA[i]<-1
  }
}

stringba2=c("BA:>50%", "BA:100%")
ba2 <- str_detect(data$SCONTENT, paste(stringba2, collapse = '|'))
summary(ba2)

for(i in 1:ptno){
  if (ba2[i] == TRUE){
    data$BA[i]<-2
  }
}

b <- str_detect(data$SCONTENT, "Arterialstenosismeasurement")
summary(b)
ba3 <- str_detect(data$SCONTENT, "BA:")
summary(ba3)

for(i in 1:ptno){
  if (b[i] == TRUE & ba3[i] == FALSE){
    data$BA[i]<-1
  }
}

###############Identify Stenosis#################
data$Stenosis <- 0
data$DELSTENOSIS <- tolower(data$CONTENT)
data$DELSTENOSIS <- gsub("bil.", "bilateral", data$DELSTENOSIS)
for(i in 1:ptno){
delstenosis1 <- c("no significant stenosis", 
                  "without significant stenosis", 
                  "non-significant stenosis", "no stenosis", "without stenosis", 
                  "arterial stenosis measurement", "mild stenosis", 
                  "segmental stenosis", "focal stenosis", "moderate stenosis",
                  "no obvious stenosis", "no marked stenosis", 
                  "no significant arterial stenosis", 
                  "no definite evidence of arterial stenosis or occlusion",
                  "no vascular stenosis", "no significant vascular stenosis",
                  "no marked arterial stenosis or occlusion",
                  "no evidence of significant stenosis, occlusion", 
                  "no significant arterial stenosis or occlusion", 
                  "no significant stenosis or occlusion",
                  "no great vessel occlusion nor stenosis",
                  "without evidence of significant stenosis",
                  "no evidence of significant extracranial or intracranial arterial stenosis",
                  "no evidence of arterial stenosis",
                  "no definite arterial stenosis or occlusion",
                  "no significant intracranial or extracranial arterial stenosis or aneurysm",
                  "no obvious arterial stenosis, occulsion",
                  "no significant intracranial arterial stenosis",
                  "no other significant arterial stenosis or occlusion",
                  "no arterial stenosis",
                  "focal arterial stenosis",
                  "no significant carotid stenosis",
                  "no other significant focal stenosis or vessel occlusion",
                  "without significant arterial stenosis or occlusion",
                  "no discernible luminal defect or occlusion",
                  "no significant stenosis, occlusion",
                  "no critical stenosis",
                  "no visible stenosis",
                  "no evidence of aneurysm, significant stenosis",
                  "no significant extra/intracranial arterial stenosis",
                  "no definite stenosis or occlusion",
                  "no occlusion or significant stenosis",
                  "without another significant stenosis",
                  "without significant stenosis or occlusion",
                  "no obvious extra/intra-cranial arterial stenosis",
                  "no apparent stenosis",
                  "no mra evidence of neck or intracranial major arterial significant stenosis",
                  "nor significant arterial stenosis. no evidence of ich, major stroke, or significant arterial stenosis",
                  "no evidene of significant intracranial arterial stenosis. nosignificant intracranial arterial stenosis",
                  "no significant vascular lesions, such as aneurysm, stenosis, or avm",
                  "no obvious arterial stenosis, aneurysm, avm or other vascular anomaly",
                  "no evidence of intracranial aneurysm or significant proximal arterial stenosis",
                  "without major infarction or stenosis",
                  "no visible stenosis, nor occlusion, nor dissection",
                  "no evidence of aneurysm or significant arterial stenosis",
                  "no evidence of stenosis or vascular anomaly",
                  "no definite major vascular stenosis",
                  "no evidence of significant neck and intracranial major arterial stenosis",
                  "no definite intracranial arterial significant stenosis",
                  "no gross critical stenosis in intracranial and extracranial arteries",
                  "no definite stenosis of extra/intracranial arteries",
                  "no significant extracranial or intracranial arterial stenosis",
                  "no mra evidence of significant stenosis at neck carotid arteries or major intracranial arteries",
                  "no definite extra/intracranial arterial stenosis",
                  "no evidence of intracranial aneurysm, av malformation, or proximal vessel stenosis",
                  "no significant arterial stenosis/occlusion",
                  "neither stenosis nor occlusion in the intracranial arteries",
                  "no obvious arterial stenosis or occulsion")
data$DELSTENOSIS[i] <- gsub(paste(delstenosis1, collapse = '|'), "ns", data$DELSTENOSIS[i])
astenosis1 <- data$DELSTENOSIS[i]
astenosis2 <- unlist(strsplit(astenosis1, split="[\n\r.]"))
astenosis3 <- astenosis2[grep("stenosis", astenosis2)]
data$Stenosis[i] <- paste(astenosis3, collapse = ".")
}
data$DELSTENOSIS <- NULL
###############Identify Narrowing#################
data$Narrowing <- 0
data$DELNARROWING <- tolower(data$CONTENT)
data$DELNARROWING <- gsub("bil.", "bilateral", data$DELNARROWING)
for(i in 1:ptno){
  delnarrowing1 <- c("no significant narrowing", "without significant narrowing", "non-significant narrowing",
                     "no narrowing", "without narrowing", "mild narrowing", "segmental narrowing", 
                     "focal narrowing", "moderate narrowing",
                     "no obvious narrowing", "no marked narrowing", 
                     "no significant arterial narrowing",
                     "no marked arterial narrowing or occlusion", "without marked narrowing or occlusion",
                     "no evidence of significant intracranial or extracranial arterial narrowing")
  data$DELNARROWING[i] <- gsub(paste(delnarrowing1, collapse = '|'), "ns", data$DELNARROWING[i])
  anarrowing1 <- data$DELNARROWING[i]
  anarrowing2 <- unlist(strsplit(anarrowing1, split="[\n\r.]"))
  anarrowing3 <- anarrowing2[grep("narrowing", anarrowing2)]
  data$Narrowing[i] <- paste(anarrowing3, collapse = ".")
}
data$DELNARROWING <- NULL
###############Identify Occlusion#################
data$Occlusion <- 0
data$DELOCCLUSION <- tolower(data$CONTENT)
data$DELOCCLUSION <- gsub("bil.", "bilateral", data$DELOCCLUSION)
for(i in 1:ptno){
  delocclusion1 <- c("no significant occlusion", "without significant occlusion", 
                     "non-significant occlusion",
                     "no occlusion", "without occlusion", "mild occlusion", 
                     "moderate occlusion",
                     "no obvious occlusion", "no marked occlusion", 
                     "no significant arterial occlusion", 
                     "no definite evidence of arterial stenosis or occlusion",
                     "no vascular occlusion", 
                     "no marked arterial stenosis or occlusion",
                     "no evidence of significant stenosis, occlusion", 
                     "no significant arterial stenosis or occlusion",
                     "no significant stenosis or occlusion",
                     "no great vessel occlusion nor stenosis",
                     "no definite arterial stenosis or occlusion",
                     "no obvious arterial stenosis, occulsion",
                     "no other significant arterial stenosis or occlusion",
                     "no other significant focal stenosis or vessel occlusion",
                     "without significant arterial stenosis or occlusion",
                     "small vessel occlusion",
                     "no major intracranial artery occlusion",
                     "no significant stenosis, occlusion",
                     "no gross occlusion",
                     "no evidence of intracranial large arterial occlusion",
                     "no definite stenosis or occlusion",
                     "no occlusion or significant stenosis",
                     "no marked arterial narrowing or occlusion",
                     "without significant stenosis or occlusion",
                     "no definite evidence of arterial occlusion",
                     "no evidence of large artery occlusion",
                     "no large arterial occlusion",
                     "no visible stenosis, nor occlusion, nor dissection",
                     "without marked narrowing or occlusion",
                     "no evidence of large vessel occlusion",
                     "no arterial occlusion",
                     "no major arterial occlusion",
                     "no significant arterial stenosis/occlusion",
                     "no intracranial arteries occlusion or anuerysm",
                     "neither stenosis nor occlusion in the intracranial arteries",
                     "no obvious arterial stenosis or occulsion")
  data$DELOCCLUSION[i] <- gsub(paste(delocclusion1, collapse = '|'), "ns", data$DELOCCLUSION[i])
  aocclusion1 <- data$DELOCCLUSION[i]
  aocclusion2 <- unlist(strsplit(aocclusion1, split="[\n\r.]"))
  aocclusion3 <- aocclusion2[grep("occlusion", aocclusion2)]
  data$Occlusion[i] <- paste(aocclusion3, collapse = ".")
}
data$DELOCCLUSION <- NULL
###############Identify Obstruction#################
data$Obstruction <- 0
data$DELOBSTRUCTION <- tolower(data$CONTENT)
data$DELOBSTRUCTION <- gsub("bil.", "bilateral", data$DELOBSTRUCTION)
for(i in 1:ptno){
  delobstruction1 <- c("no significant obstruction", "without significant Obstruction", "non-significant Obstruction",
                     "no obstruction", "without obstruction", "mild obstruction", 
                     "segmental obstruction", "focal obstruction", "moderate obstruction",
                     "no obvious obstruction", "no marked obstruction", 
                     "no significant arterial obstruction")
  data$DELOBSTRUCTION[i] <- gsub(paste(delobstruction1, collapse = '|'), "ns", data$DELOBSTRUCTION[i])
  aobstruction1 <- data$DELOBSTRUCTION[i]
  aobstruction2 <- unlist(strsplit(aobstruction1, split="[\n\r.]"))
  aobstruction3 <- aobstruction2[grep("obstruction", aobstruction2)]
  data$Obstruction[i] <- paste(aobstruction3, collapse = ".")
}
data$DELOBSTRUCTION <- NULL
######################Codeing for Stenosis################
for(i in 1:ptno){
  if (data$Table[i]=="0" & data$Stenosis[i]=="" &  data$Narrowing[i]==""&
      data$Occlusion[i]==""&data$Obstruction[i]==""){
    data$RCCA[i]<-1; data$REICA[i]<-1; data$RIICA[i]<-1; data$RACA[i]<-1; 
    data$RMCA[i]<-1; data$RPCA[i]<-1; data$REVA[i]<-1; data$RIVA[i]<-1; 
    data$BA[i]<-1; data$LCCA[i]<-1; data$LEICA[i]<-1; data$LIICA[i]<-1; 
    data$LACA[i]<-1; data$LMCA[i]<-1; data$LPCA[i]<-1; data$LEVA[i]<-1; 
    data$LIVA[i]<-1}
  if (data$Table[i]=="1") {data$Stenosis[i] <-""; data$Narrowing[i]<-"";
         data$Occlusion[i]<-""; data$Obstruction[i]<-""}
}

##################Inclusive Key Words Finding############
data$Allkey <- 0
data$Findx <-0
data$Xkey <-0
data$FINDALLKEY <- tolower(data$CONTENT)
for(i in 1:ptno){
  delallkey1 <- c("no significant stenosis", 
                  "without significant stenosis", 
                  "non-significant stenosis", "no stenosis", "without stenosis", 
                  "arterial stenosis measurement", "mild stenosis", 
                  "segmental stenosis", "focal stenosis", "moderate stenosis",
                  "no obvious stenosis", "no marked stenosis", 
                  "no significant arterial stenosis", 
                  "no definite evidence of arterial stenosis or occlusion",
                  "no vascular stenosis", "no significant vascular stenosis",
                  "no marked arterial stenosis or occlusion",
                  "no evidence of significant stenosis, occlusion", 
                  "no significant arterial stenosis or occlusion", 
                  "no significant stenosis or occlusion",
                  "no great vessel occlusion nor stenosis",
                  "without evidence of significant stenosis",
                  "no evidence of significant extracranial or intracranial arterial stenosis",
                  "no evidence of arterial stenosis",
                  "no definite arterial stenosis or occlusion",
                  "no significant intracranial or extracranial arterial stenosis or aneurysm",
                  "no obvious arterial stenosis, occulsion",
                  "no significant intracranial arterial stenosis",
                  "no other significant arterial stenosis or occlusion",
                  "no arterial stenosis",
                  "focal arterial stenosis",
                  "no significant carotid stenosis",
                  "no other significant focal stenosis or vessel occlusion",
                  "without significant arterial stenosis or occlusion",
                  "no discernible luminal defect or occlusion",
                  "no significant stenosis, occlusion",
                  "no critical stenosis",
                  "no visible stenosis",
                  "no evidence of aneurysm, significant stenosis",
                  "no significant extra/intracranial arterial stenosis",
                  "no definite stenosis or occlusion",
                  "no occlusion or significant stenosis",
                  "without another significant stenosis",
                  "without significant stenosis or occlusion",
                  "no obvious extra/intra-cranial arterial stenosis",
                  "no apparent stenosis",
                  "no mra evidence of neck or intracranial major arterial significant stenosis",
                  "no significant narrowing", "without significant narrowing", "non-significant narrowing",
                  "no narrowing", "without narrowing", "mild narrowing", "segmental narrowing", 
                  "focal narrowing", "moderate narrowing",
                  "no obvious narrowing", "no marked narrowing", 
                  "no significant arterial narrowing",
                  "no marked arterial narrowing or occlusion",
                  "no significant occlusion", "without significant occlusion", 
                  "non-significant occlusion",
                  "no occlusion", "without occlusion", "mild occlusion", 
                  "moderate occlusion",
                  "no obvious occlusion", "no marked occlusion", 
                  "no significant arterial occlusion", 
                  "no definite evidence of arterial stenosis or occlusion",
                  "no vascular occlusion", 
                  "no marked arterial stenosis or occlusion",
                  "no evidence of significant stenosis, occlusion", 
                  "no significant arterial stenosis or occlusion",
                  "no significant stenosis or occlusion",
                  "no great vessel occlusion nor stenosis",
                  "no definite arterial stenosis or occlusion",
                  "no obvious arterial stenosis, occulsion",
                  "no other significant arterial stenosis or occlusion",
                  "no other significant focal stenosis or vessel occlusion",
                  "without significant arterial stenosis or occlusion",
                  "small vessel occlusion",
                  "no major intracranial artery occlusion",
                  "no significant stenosis, occlusion",
                  "no gross occlusion",
                  "no evidence of intracranial large arterial occlusion",
                  "no definite stenosis or occlusion",
                  "no occlusion or significant stenosis",
                  "no marked arterial narrowing or occlusion",
                  "without significant stenosis or occlusion",
                  "no definite evidence of arterial occlusion",
                  "no evidence of large artery occlusion",
                  "no large arterial occlusion",
                  "no significant obstruction", "without significant Obstruction", "non-significant Obstruction",
                  "no obstruction", "without obstruction", "mild obstruction", 
                  "segmental obstruction", "focal obstruction", "moderate obstruction",
                  "no obvious obstruction", "no marked obstruction", 
                  "no significant arterial obstruction",
                  "no defintie significants stenosis at neck carotid arteries or major intracranial arteries",
                  "no significant craniocervical arterial stenosis",
                  "no significant intracranial or extracranial arterial stenosis",
                  "no intracranial arteries occlusion or anuerysm")
  data$FINDALLKEY[i] <- gsub(paste(delallkey1, collapse = '|'), "ns", data$FINDALLKEY[i])
  data$FINDALLKEY[i] <- gsub("bil.", "bilateral", data$FINDALLKEY[i])
  data$FINDALLKEY[i] <- gsub("bilateralteral", "bilateral", data$FINDALLKEY[i])
  data$FINDALLKEY[i] <- gsub("signficiant", "significant", data$FINDALLKEY[i])
  
  aallkey1 <- data$FINDALLKEY[i]
  aallkey2 <- unlist(strsplit(aallkey1, split="[\n\r.]"))
  allkeyterm1 <- c("stenosis", "narrowing", "occlusion", "obstruction")
  aallkey3 <- aallkey2[grep(paste(allkeyterm1, collapse="|"),aallkey2)]
  data$Allkey[i] <- paste(aallkey3, collapse = ".")
  nkey <- length(aallkey3)
  
  stenosisterm1=c(">50%", "~50%","significant", ">=50% stenosis", ">=50% narrowing", 
                    ">50% stenosis", "significant stenosis", "significant narrowing", 
                    "tight stenosis", "tight narrowing", "severe stenosis", 
                    "severe narrowing", "high grade stenosis", "high grade narrowing", 
                    "high-grade stenosis", "high-grade narrowing", 
                    "high degree stenosis", "high degree narrowing", 
                    "high-degree stenosis", "high-degree narrowing", 
                    "prominent stenosis", "prominent narrowing", "critical stenosis", 
                    "critical narrowing", "no visual", "no opacification", "no opacity", 
                    "absence", "occlusion", "occluded","obstruction", "significant focal stenosis",
                    "marked narrowing", "severe segmental stenosis", "severe focal stenosis",
                    "occlusvie stenosis")
  rccakey <- c("right common carotid artery", "rt cca", "right cca", " r cca", "right distal cca",
               "right proximal cca")
  reicakey <- c("right cervical ica", "right proximal ica", "right ica cervical segment",
                "right neck internal carotid artery", 
                "right proximal cervical ica", "rt proximal ica",
                "proximal right ica", "cervical segment of right ica",
                "right proximal neck ica")
  riicakey <- c("right intracranial ica", "right distal ica",
                "right ica siphon", "rt carotid siphon",
                "right communicating ica", "right supraclinoid ica",
                "right petrous ica", "right carotid siphon",
                "right cavernous ica", "right ica lacerous segment")
  racakey <- c("right aca", "rt aca","right anterior cerebral artery", "right a1", "right a2", "right a3", "right a4", "right a5")
  rmcakey <- c("right mca", "rt mca", "right m1", "right m2", "right m3", "right m4", "right m5",
               "right middle cerebral artery", "rt proximal m2",
               "right distal m1")
  rpcakey <- c("right pca", "right posterior cerebral artery", "right p1", "right p2", "right p3", "right p4", "right p5", "rt pca")
  revakey <- c("right proximal va", "right cervical va", "right neck vertebral artery", "right extracranial va",
               "right v1", "right v2", "right v3", "proximal right vertebral artery",
               "rt va orifice")
  rivakey <- c("right distal va", "right intracranial va",  
               "rt va distal to the pica origin", "right intracranial va",
               "right v4", "right cranial vertebral artery",
               "right intracranial vertebral artery",
               "intradural segment of right va", "rt intracranial va",
               "right intradural va","v4 portion of right va",
               "right distal v4 va", "right va v4")
  bakey <- c("basilar artery", " ba ", "basilar trunk", "basilar a")
  lccakey <- c("left common carotid artery", "left cca", "lt cca", " l cca", "left distal cca",
               "left proximal cca")
  leicakey <- c("left cervical ica", "left proximal ica", "left ica cervical segment",
                "left neck internal carotid artery", 
                "left proximal cervical ica", "lt proximal ica",
                "proximal left ica", "cervical segment of left ica",
                "left proximal neck ica")
  liicakey <- c("left intracranial ica", "left distal ica",
                "left ica siphon", "lt carotid siphon",
                "left communicating ica", "left supraclinoid ica",
                "left petrous ica", "left carotid siphon",
                "left cavernous ica", "left ica lacerous segment")
  lacakey <- c("left aca", "lt aca","left anterior cerebral artery", "left a1", "left a2", "left a3", "left a4", "left a5")
  lmcakey <- c("left mca", "lt mca", "left m1", "left m2", "left m3", "left m4", "left m5",
               "left middle cerebral artery", "lt proximal m2",
               "left distal m1")
  lpcakey <- c("left pca", "left posterior cerebral artery", "left p1", "left p2", "left p3", "left p4", "left p5", "lt pca")
  levakey <- c("left proximal va", "left cervical va", "left neck vertebral artery", "left extracranial va",
               "left v1", "left v2", "left v3", "proximal left vertebral artery",
               "lt va orifice")
  livakey <- c("left distal va", "left intracranial va",
               "lt va distal to the pica origin", "left intracranial va",
               "left v4", "left cranial vertebral artery",
               "left intracranial vertebral artery",
               "intradural segment of left va", "lt intracranial va",
               "left intradural va", "v4 portion of left va",
               "left distal v4 va", "left va v4")
  bccakey <- c("bilateral cca", "bilateral common carotid artery")
  beicakey <- c("bilateral cervical ica", "bilateral proximal ica", "bilateral ica cervical segment",
                "bilateral neck internal carotid artery", "bilateral proximal cervical ica",
                "bilateral icas a the origin")
  biicakey <- c("bilateral intracranial ica", "bilateral distal ica", "siphon portion of bilateralteral ica",
                "bilateral ica siphon", 
                "bilateral communicating ica", "bilateral supraclinoid ica",
                "bilateral petrous ica", "bilateral intracranial ica",
                "bilateralteral cranial internal carotid arteries",
                "bilateral carotid siphon")
  bacakey <- c("bilateral aca", "bilateral anterior cerebral artery")
  bmcakey <- c("bilateral mca", "bilateral m1",
               "bilateral middle cerebral artery", "bilateral distal m1")
  bpcakey <- c("bilateral pca", "bilateral p1", "bilateral posterior cerebral artery")
  bevakey <- c("bilateral proximal va", "bilateral cervical va", "bilateral v1",
               "bilateral va orifice")
  bivakey <- c("bilateral distal va", "bilateral intracranial va",
               "bilateral intracranial va",
               "bilateral v4", "bilateral cranial vertebral artery", 
               "bilateral intracranial vertebral artery",
               "intradural segment of bilateral va",
               "bilateral intradural va",
               "bilateralteral cranial vertebral arteries",
               "bilateral distal va")
  reiicakey <- c("right neck and cranial internal carotid artery", "right ica", "right internal carotid artery")
  leiicakey <- c("left neck and cranial internal carotid artery", "left ica", "left internal carotid artery")
  reivakey <- c("right neck and cranial vertebral artery", "right va", "right vertebral artery")
  leivakey <- c("left neck and cranial vertebral artery", "left va", "left vertebral artery")
  
  countallx<- 0
  countx<- 0
  if (is.na(data$RCCA[i]) == FALSE |is.na(data$REICA[i]) == FALSE |is.na(data$RIICA[i]) == FALSE |
      is.na(data$RACA[i]) == FALSE |is.na(data$RMCA[i]) == FALSE |is.na(data$RPCA[i]) == FALSE |
      is.na(data$REVA[i]) == FALSE |is.na(data$RIVA[i]) == FALSE |is.na(data$BA[i]) == FALSE |
      is.na(data$LCCA[i]) == FALSE |is.na(data$LEICA[i]) == FALSE |is.na(data$LIICA[i]) == FALSE |
      is.na(data$LACA[i]) == FALSE |is.na(data$LMCA[i]) == FALSE |is.na(data$LPCA[i]) == FALSE |
      is.na(data$LEVA[i]) == FALSE |is.na(data$LIVA[i]) == FALSE)next
  
  for(nk in 1:nkey){
    sallkey1<- aallkey3[nk]
    repeat{
    stenosisdetect1 <- str_detect(sallkey1, paste(stenosisterm1, collapse = '|'))
    rccadetect1 <- str_detect(sallkey1, paste(rccakey, collapse = '|'))
    reicadetect1 <- str_detect(sallkey1, paste(reicakey, collapse = '|'))
    riicadetect1 <- str_detect(sallkey1, paste(riicakey, collapse = '|'))
    racadetect1 <- str_detect(sallkey1, paste(racakey, collapse = '|'))
    rmcadetect1 <- str_detect(sallkey1, paste(rmcakey, collapse = '|'))
    rpcadetect1 <- str_detect(sallkey1, paste(rpcakey, collapse = '|'))
    revadetect1 <- str_detect(sallkey1, paste(revakey, collapse = '|'))
    rivadetect1 <- str_detect(sallkey1, paste(rivakey, collapse = '|'))
    badetect1 <- str_detect(sallkey1, paste(bakey, collapse = '|'))
    lccadetect1 <- str_detect(sallkey1, paste(lccakey, collapse = '|'))
    leicadetect1 <- str_detect(sallkey1, paste(leicakey, collapse = '|'))
    liicadetect1 <- str_detect(sallkey1, paste(liicakey, collapse = '|'))
    lacadetect1 <- str_detect(sallkey1, paste(lacakey, collapse = '|'))
    lmcadetect1 <- str_detect(sallkey1, paste(lmcakey, collapse = '|'))
    lpcadetect1 <- str_detect(sallkey1, paste(lpcakey, collapse = '|'))
    levadetect1 <- str_detect(sallkey1, paste(levakey, collapse = '|'))
    livadetect1 <- str_detect(sallkey1, paste(livakey, collapse = '|'))
    bccadetect1 <- str_detect(sallkey1, paste(bccakey, collapse = '|'))
    beicadetect1 <- str_detect(sallkey1, paste(beicakey, collapse = '|'))
    biicadetect1 <- str_detect(sallkey1, paste(biicakey, collapse = '|'))
    bacadetect1 <- str_detect(sallkey1, paste(bacakey, collapse = '|'))
    bmcadetect1 <- str_detect(sallkey1, paste(bmcakey, collapse = '|'))
    bpcadetect1 <- str_detect(sallkey1, paste(bpcakey, collapse = '|'))
    bevadetect1 <- str_detect(sallkey1, paste(bevakey, collapse = '|'))
    bivadetect1 <- str_detect(sallkey1, paste(bivakey, collapse = '|'))
    reiicadetect1 <- str_detect(sallkey1, paste(reiicakey, collapse = '|'))
    leiicadetect1 <- str_detect(sallkey1, paste(leiicakey, collapse = '|'))
    reivadetect1 <- str_detect(sallkey1, paste(reivakey, collapse = '|'))
    leivadetect1 <- str_detect(sallkey1, paste(leivakey, collapse = '|'))
    
    data$Xkey[i]<-0
    if (nkey==0){data$Xkey[i]<-18; countx<-0; break 
    }else if (stenosisdetect1 == TRUE & rccadetect1 == TRUE){data$Xkey[i]<-1; countx<-1;
    sallkey1 <- gsub(paste(rccakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & reicadetect1 == TRUE){data$Xkey[i]<-2; countx<-1;
    sallkey1 <- gsub(paste(reicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & riicadetect1 == TRUE){data$Xkey[i]<-3; countx<-1;
    sallkey1 <- gsub(paste(riicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & racadetect1 == TRUE){data$Xkey[i]<-4; countx<-1;
    sallkey1 <- gsub(paste(racakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & rmcadetect1 == TRUE){data$Xkey[i]<-5; countx<-1;
    sallkey1 <- gsub(paste(rmcakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & rpcadetect1 == TRUE){data$Xkey[i]<-6; countx<-1;
    sallkey1 <- gsub(paste(rpcakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & revadetect1 == TRUE){data$Xkey[i]<-7; countx<-1;
    sallkey1 <- gsub(paste(revakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & rivadetect1 == TRUE){data$Xkey[i]<-8; countx<-1;
    sallkey1 <- gsub(paste(rivakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & badetect1 == TRUE){data$Xkey[i]<-9; countx<-1;
    sallkey1 <- gsub(paste(bakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & lccadetect1 == TRUE){data$Xkey[i]<-10; countx<-1;
    sallkey1 <- gsub(paste(lccakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & leicadetect1 == TRUE){data$Xkey[i]<-11; countx<-1;
    sallkey1 <- gsub(paste(leicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & liicadetect1 == TRUE){data$Xkey[i]<-12; countx<-1;
    sallkey1 <- gsub(paste(liicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & lacadetect1 == TRUE){data$Xkey[i]<-13; countx<-1;
    sallkey1 <- gsub(paste(lacakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & lmcadetect1 == TRUE){data$Xkey[i]<-14; countx<-1;
    sallkey1 <- gsub(paste(lmcakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & lpcadetect1 == TRUE){data$Xkey[i]<-15; countx<-1;
    sallkey1 <- gsub(paste(lpcakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & levadetect1 == TRUE){data$Xkey[i]<-16; countx<-1;
    sallkey1 <- gsub(paste(levakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & livadetect1 == TRUE){data$Xkey[i]<-17; countx<-1;
    sallkey1 <- gsub(paste(livakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & bccadetect1 == TRUE){data$Xkey[i]<-19; countx<-1;
    sallkey1 <- gsub(paste(bccakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & beicadetect1 == TRUE){data$Xkey[i]<-20; countx<-1;
    sallkey1 <- gsub(paste(beicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & biicadetect1 == TRUE){data$Xkey[i]<-21; countx<-1;
    sallkey1 <- gsub(paste(biicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & bacadetect1 == TRUE){data$Xkey[i]<-22; countx<-1;
    sallkey1 <- gsub(paste(bacakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & bmcadetect1 == TRUE){data$Xkey[i]<-23; countx<-1;
    sallkey1 <- gsub(paste(bmcakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & bpcadetect1 == TRUE){data$Xkey[i]<-24; countx<-1;
    sallkey1 <- gsub(paste(bpcakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & bevadetect1 == TRUE){data$Xkey[i]<-25; countx<-1;
    sallkey1 <- gsub(paste(bevakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & bivadetect1 == TRUE){data$Xkey[i]<-26; countx<-1;
    sallkey1 <- gsub(paste(bivakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & reiicadetect1 == TRUE){data$Xkey[i]<-27; countx<-1;
    sallkey1 <- gsub(paste(reiicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & leiicadetect1 == TRUE){data$Xkey[i]<-28; countx<-1;
    sallkey1 <- gsub(paste(leiicakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & reivadetect1 == TRUE){data$Xkey[i]<-29; countx<-1;
    sallkey1 <- gsub(paste(reivakey, collapse = '|'), "ns", sallkey1)
    }else if (stenosisdetect1 == TRUE & leivadetect1 == TRUE){data$Xkey[i]<-30; countx<-1;
    sallkey1 <- gsub(paste(leivakey, collapse = '|'), "ns", sallkey1)
    }else {break}
    
    countallx <- countallx+countx
    data$Findx[i]<- countallx
    switch(data$Xkey[i], data$RCCA[i] <- 2, data$REICA[i] <- 2, data$RIICA[i] <- 2, 
           data$RACA[i] <- 2, data$RMCA[i] <- 2, data$RPCA[i] <- 2, 
           data$REVA[i] <- 2, data$RIVA[i] <- 2, data$BA[i] <- 2,
           data$LCCA[i] <- 2, data$LEICA[i] <- 2, data$LIICA[i] <- 2, 
           data$LACA[i] <- 2, data$LMCA[i] <- 2, data$LPCA[i] <- 2, 
           data$LEVA[i] <- 2, data$LIVA[i] <- 2, data$Xkey[i] <- 0,
           {data$RCCA[i] <- 2;data$LCCA[i] <- 2}, {data$REICA[i] <- 2;data$LEICA[i] <- 2},
           {data$RIICA[i] <- 2;data$LIICA[i] <- 2}, {data$RACA[i] <- 2;data$LACA[i] <- 2},
           {data$RMCA[i] <- 2;data$LMCA[i] <- 2}, {data$RPCA[i] <- 2;data$LPCA[i] <- 2},
           {data$REVA[i] <- 2;data$LEVA[i] <- 2}, {data$RIVA[i] <- 2;data$LIVA[i] <- 2},
           {data$REICA[i] <- 2;data$RIICA[i] <- 2}, {data$LEICA[i] <- 2;data$LIICA[i] <- 2},
           {data$REVA[i] <- 2;data$RIVA[i] <- 2}, {data$LEVA[i] <- 2;data$LIVA[i] <- 2})    
      }
    }
}
data$Allkey<- NULL
data$FINDALLKEY<-NULL
data$Xkey<-NULL
data$nowithout<-NULL

#################Identification of NA###################
for(i in 1:ptno){
  if (is.na(data$RCCA[i]) == TRUE){data$RCCA[i]<-1} 
  if (is.na(data$REICA[i]) == TRUE){data$REICA[i]<-1}
  if (is.na(data$RIICA[i]) == TRUE){data$RIICA[i]<-1}
  if (is.na(data$RACA[i]) == TRUE){data$RACA[i]<-1}
  if (is.na(data$RMCA[i]) == TRUE){data$RMCA[i]<-1}
  if (is.na(data$RPCA[i]) == TRUE){data$RPCA[i]<-1}
  if (is.na(data$REVA[i]) == TRUE){data$REVA[i]<-1}
  if (is.na(data$RIVA[i]) == TRUE){data$RIVA[i]<-1} 
  if (is.na(data$BA[i]) == TRUE){data$BA[i]<-1}
  if (is.na(data$LCCA[i]) == TRUE){data$LCCA[i]<-1} 
  if (is.na(data$LEICA[i]) == TRUE){data$LEICA[i]<-1}
  if (is.na(data$LIICA[i]) == TRUE){data$LIICA[i]<-1}
  if (is.na(data$LACA[i]) == TRUE){data$LACA[i]<-1}
  if (is.na(data$LMCA[i]) == TRUE){data$LMCA[i]<-1}
  if (is.na(data$LPCA[i]) == TRUE){data$LPCA[i]<-1}
  if (is.na(data$LEVA[i]) == TRUE){data$LEVA[i]<-1}
  if (is.na(data$LIVA[i]) == TRUE){data$LIVA[i]<-1} 
 }

##########################################################
#Build Priliminary Data
data1 <- data
data1 <- data1[,-1:-10]
data1 <-data1[,c(1:19,24,20:23)]
write.table(data1, file = "count1.CSV", sep = ",")
##########################################################
##Calculate Classification Accuracy of Algorithm
count1_compare <- read_excel("~/R language/count1-compare.xlsx")
data <- count1_compare
rm(count1_compare)
ptno<-nrow(data)

data$RCCA2<-0
data$REICA2<-0
data$RIICA2<-0
data$RACA2<-0
data$RMCA2<-0
data$RPCA2<-0
data$REVA2<-0
data$RIVA2<-0
data$BA2<-0
data$LCCA2<-0
data$LEICA2<-0
data$LIICA2<-0
data$LACA2<-0
data$LMCA2<-0
data$LPCA2<-0
data$LEVA2<-0
data$LIVA2<-0

for(i in 1:ptno){
  if (data$RCCA[i] == data$RCCA1[i]){data$RCCA2[i]<-1}
  if (data$REICA[i] == data$REICA1[i]){data$REICA2[i]<-1}
  if (data$RIICA[i] == data$RIICA1[i]){data$RIICA2[i]<-1}
  if (data$RACA[i] == data$RACA1[i]){data$RACA2[i]<-1}
  if (data$RMCA[i] == data$RMCA1[i]){data$RMCA2[i]<-1}
  if (data$RPCA[i] == data$RPCA1[i]){data$RPCA2[i]<-1}
  if (data$REVA[i] == data$REVA1[i]){data$REVA2[i]<-1}
  if (data$RIVA[i] == data$RIVA1[i]){data$RIVA2[i]<-1}
  if (data$BA[i] == data$BA1[i]){data$BA2[i]<-1}
  if (data$LCCA[i] == data$LCCA1[i]){data$LCCA2[i]<-1}
  if (data$LEICA[i] == data$LEICA1[i]){data$LEICA2[i]<-1}
  if (data$LIICA[i] == data$LIICA1[i]){data$LIICA2[i]<-1}
  if (data$LACA[i] == data$LACA1[i]){data$LACA2[i]<-1}
  if (data$LMCA[i] == data$LMCA1[i]){data$LMCA2[i]<-1}
  if (data$LPCA[i] == data$LPCA1[i]){data$LPCA2[i]<-1}
  if (data$LEVA[i] == data$LEVA1[i]){data$LEVA2[i]<-1}
  if (data$LIVA[i] == data$LIVA1[i]){data$LIVA2[i]<-1}
  }
data1 <- data
write.table(data1, file = "count2.CSV", sep = ",")

#############################################################
