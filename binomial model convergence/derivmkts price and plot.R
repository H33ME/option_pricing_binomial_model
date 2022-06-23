library(derivmkts)

x<-binomopt(s=100,k=95,v=0.2,r=0.06,tt=0.5,d=0,nstep=90,
         american=FALSE,putopt=FALSE,specifyupdn=FALSE,
         crr=TRUE,jarrowrudd=FALSE,returntrees=TRUE,
         returnparams=FALSE,returngreeks=FALSE)

binomplot(binomopt(s=100,k=95,v=0.2,r=0.06,tt=0.5,d=0,nstep=90,
                   american=FALSE,putopt=FALSE,specifyupdn=FALSE,
                   crr=TRUE,jarrowrudd=FALSE,returntrees=TRUE,
                   returnparams=FALSE,returngreeks=FALSE))
