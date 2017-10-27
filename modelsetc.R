a=read.csv("faceless_men_IIT_Guwahati_lb1.csv",header = FALSE)
b=read.csv("faceless_men_IIT_Guwahati_lb2.csv",header = FALSE)

ares=read.csv("response_lb1.csv")
bres=read.csv("response_lb2.csv")

myfun=function(x){
  return(as.numeric(x[x[6]]))
}

c=b
c$V2=as.factor(ifelse(a$V2==b$V2,as.character(a$V2),NA))

ar1=apply(cbind(ares[1:4939,],a$V2[1:4939]), 1, myfun)
br1=apply(cbind(bres[1:4939,],b$V2[1:4939]), 1, myfun)*2
wnr1=ifelse(ar1>br1,"a1","b1")
table(wnr1)

ar2=apply(cbind(ares[4940:9516,],a$V2[4940:9516]), 1, myfun)
br2=apply(cbind(bres[4940:9516,],b$V2[4940:9516]), 1, myfun)*4
wnr2=ifelse(ar2>br2,"a2","b2")
table(wnr2)

ar3=apply(cbind(ares[9517:14598,],a$V2[9517:14598]), 1, myfun)
br3=apply(cbind(bres[9517:14598,],b$V2[9517:14598]), 1, myfun)*4
wnr3=ifelse(ar3>br3,"a3","b3")
table(wnr3)

ar4=apply(cbind(ares[14599:19006,],a$V2[14599:19006]), 1, myfun)
br4=apply(cbind(bres[14599:19006,],b$V2[14599:19006]), 1, myfun)*3
wnr4=ifelse(ar4>br4,"a4","b4")
table(wnr4)

ar5=apply(cbind(ares[19007:21207,],a$V2[19007:21207]), 1, myfun)
br5=apply(cbind(bres[19007:21207,],b$V2[19007:21207]), 1, myfun)*2
wnr5=ifelse(ar5>br5,"a5","b5")
table(wnr5)

d=b
d$V2[1:4939]=ifelse(wnr1=="a1",as.character(a$V2[1:4939]),as.character(b$V2[1:4939]))
d$V2[4940:9516]=ifelse(wnr2=="a2",as.character(a$V2[4940:9516]),as.character(b$V2[4940:9516]))
d$V2[9517:14598]=ifelse(wnr3=="a3",as.character(a$V2[9517:14598]),as.character(b$V2[9517:14598]))
d$V2[14599:19006]=ifelse(wnr4=="a4",as.character(a$V2[14599:19006]),as.character(b$V2[14599:19006]))
d$V2[19007:21207]=ifelse(wnr5=="a5",as.character(a$V2[19007:21207]),as.character(b$V2[19007:21207]))


table(a$V2,d$V2)
table(b$V2,d$V2)
table(c$V2,d$V2)
write.table(d,"faceless_men_IIT_Guwahati_lbc.csv",row.names = FALSE,col.names = FALSE,sep=",")


#################################################################################################################


a=read.csv("faceless_men_IIT_Guwahati_final1.csv",header = FALSE)
b=read.csv("faceless_men_IIT_Guwahati_final2.csv",header = FALSE)

ares=read.csv("response_final1.csv")
bres=read.csv("response_final2.csv")

myfun=function(x){
  return(as.numeric(x[x[6]]))
}

c=b
c$V2=as.factor(ifelse(a$V2==b$V2,as.character(a$V2),NA))

ar1=apply(cbind(ares[1:4939,],a$V2[1:4939]), 1, myfun)
br1=apply(cbind(bres[1:4939,],b$V2[1:4939]), 1, myfun)*2
wnr1=ifelse(ar1>br1,"a1","b1")
table(wnr1)

ar2=apply(cbind(ares[4940:9516,],a$V2[4940:9516]), 1, myfun)
br2=apply(cbind(bres[4940:9516,],b$V2[4940:9516]), 1, myfun)*4
wnr2=ifelse(ar2>br2,"a2","b2")
table(wnr2)

ar3=apply(cbind(ares[9517:14598,],a$V2[9517:14598]), 1, myfun)
br3=apply(cbind(bres[9517:14598,],b$V2[9517:14598]), 1, myfun)*4
wnr3=ifelse(ar3>br3,"a3","b3")
table(wnr3)

ar4=apply(cbind(ares[14599:19006,],a$V2[14599:19006]), 1, myfun)
br4=apply(cbind(bres[14599:19006,],b$V2[14599:19006]), 1, myfun)*3
wnr4=ifelse(ar4>br4,"a4","b4")
table(wnr4)

ar5=apply(cbind(ares[19007:21207,],a$V2[19007:21207]), 1, myfun)
br5=apply(cbind(bres[19007:21207,],b$V2[19007:21207]), 1, myfun)*2
wnr5=ifelse(ar5>br5,"a5","b5")
table(wnr5)

d=b
d$V2[1:4939]=ifelse(wnr1=="a1",as.character(a$V2[1:4939]),as.character(b$V2[1:4939]))
d$V2[4940:9516]=ifelse(wnr2=="a2",as.character(a$V2[4940:9516]),as.character(b$V2[4940:9516]))
d$V2[9517:14598]=ifelse(wnr3=="a3",as.character(a$V2[9517:14598]),as.character(b$V2[9517:14598]))
d$V2[14599:19006]=ifelse(wnr4=="a4",as.character(a$V2[14599:19006]),as.character(b$V2[14599:19006]))
d$V2[19007:21207]=ifelse(wnr5=="a5",as.character(a$V2[19007:21207]),as.character(b$V2[19007:21207]))


table(a$V2,d$V2)
table(b$V2,d$V2)
table(c$V2,d$V2)
write.table(d,"faceless_men_IIT_Guwahati_finalc.csv",row.names = FALSE,col.names = FALSE,sep=",")

