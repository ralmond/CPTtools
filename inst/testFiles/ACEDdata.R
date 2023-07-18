## Assumes working directory is the CPTtools development version.

ACED.scores <-
  read.csv("inst/testFiles/acedScores.csv",header=TRUE,row.names=1)

ACED.scores$Cond <- factor(ACED.scores$Cond_code,labels=c("Adaptive/Accuracy",
                  "Adaptive/Extended","Linear/Extended"))
ACED.scores$FB <- factor(ACED.scores$FB,labels=c("Extended","AccuracyOnly"))
ACED.scores$Seq <- factor(ACED.scores$Seq,labels=c("Linear","Adaptive"))

ACED.skillNames <- c("sgp","arg","cr","dt","exa","exp","ext","ind",
                     "mod","rec","tab","ver","pic")

## EAP scores are coded with H=0, reverse that.
for (eap in paste("EAP",ACED.skillNames,sep="")) {
  ACED.scores[[eap]] <- 3-ACED.scores[[eap]]
}
for (map in paste("MAP",ACED.skillNames,sep="")) {
  ACED.scores[[map]] <- factor(ACED.scores[[map]],level=1:3,
                               labels=c("L","M","H"))
}


ACED.items <-
  read.csv("CPTtools/inst/testFiles/acedItems.csv",header=TRUE,row.names=1)
ACED.items$Cond <- factor(ACED.items$Cond_code,labels=c("Adaptive/Accuracy",
                  "Adaptive/Extended","Linear/Extended"))
ACED.items$FB <- factor(ACED.items$FB,labels=c("Extended","AccuracyOnly"))
ACED.items$Seq <- factor(ACED.items$Seq,labels=c("Linear","Adaptive"))

for (i in 10:72) {
  ACED.items[[i]] <- factor(ACED.items[[i]],levels=1:2,
                            labels=c("Incorrect","Correct"))
}

ACED.prePost <-
  read.csv("CPTtools/inst/testFiles/acedPrePost.csv",header=TRUE,row.names=1)

ACED.prePost$Cond <- factor(ACED.prePost$Cond_code,labels=c("Adaptive/Accuracy",
                  "Adaptive/Extended","Linear/Extended","Control"))
ACED.prePost$FB <- factor(ACED.prePost$FB,levels=1:2,labels=c("Extended","AccuracyOnly"))
ACED.prePost$Seq <- factor(ACED.prePost$Seq,levels=1:2,labels=c("Linear","Adaptive"))
ACED.prePost$Form_Order <- factor(ACED.prePost$Form_Order,levels=1:2,
                                  labels=c("AB","BA"))
ACED.prePost$Level_Code <- factor(ACED.prePost$Form_Order,levels=1:6,
                                  labels=c("Honors","Academic","Regular",
                                      "Part 1","Part 2", "ELL"))
## I don't have codings for these variables handy.
ACED.prePost$Gender <- factor(ACED.prePost$Gender,levels=1:2)
ACED.prePost$Race <- factor(ACED.prePost$Race,levels=1:8)
ACED.prePost$Flagged <- as.logical(ACED.prePost$Flagged)

save(ACED.scores,ACED.items,ACED.prePost,ACED.skillNames,
     file="CPTtools/data/ACED.RData")
