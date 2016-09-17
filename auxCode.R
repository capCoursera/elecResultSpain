source('~/devel/elecResultSpain/lib/ProcessFiles.R')
source('~/devel/elecResultSpain/lib/DHont.R')

library(utils)
# merge(v3,v6[(v6$CodProv == 28) & (v6$CodMun ==79) & (v6$MunDistrict == 99),]) -> resMad
# merge(v6[(v6$CodProv == 28) & (v6$MunDistrict == 99),],v3) -> resComMad
# resMad[,c(1:3,5:8,11,9)]->resMadVotos
#
# dcast(data= resMadVotos, formula = ElType+ElYear+ElMonth+Round+CodProv+CodMun+MunDistrict~ candAcronym, value.var="CandVotos", fill=0) -> rowMadVotes
#
# resMad[,c(1:3,5:8,11,10)]->resMadEsc
# dcast(data= resMadEsc, formula = ElType+ElYear+ElMonth+Round+CodProv+CodMun+MunDistrict~ candAcronym, value.var="CandObt", fill=0) -> rowMadEsc
#

cong <- unpackElectResults(filename = "/home/Datasets/Elec/wrk/Congreso02201111_MESA.zip")
#mesaData <- addExtraInfoMesas(cong)

resAgreg <- ResultadosAcumMIR(cong)
rVotes <- ProcessMesasMIR(cong)
cands <- rVotes$candidaturas
mVotes <- rVotes$votosMesa

asAgreg <- resAgreg$asientos[resAgreg$asientos$CodProv!=99,c(cands)]
asAgreg$final <- TRUE

numEscs <- cong$municAggrData[cong$municAggrData$CodProv!=99, c("CodProv","NomAmbTerr","NumEscs")]
mVotesE <- merge(mVotes,numEscs)
mVotesE$seqM <- seq(nrow(mVotesE))
nocands <- names(mVotesE)[!names(mVotesE) %in% cands]

mVotesC <- mVotesE[,cands]

Rprof(filename='/var/tmp/massiveDHondt.Rprof',line.profiling = T,memory.profiling = T,gc.profiling = T,interval = 0.01)
mEscs<- massiveDHondt(votes <-mVotesC,parties = cands,seats = mVotesE$NumEscs,threshold = 0.03)
Rprof(NULL)
#summaryRprof(filename='/var/tmp/massiveDHondt.Rprof',lines = "both",memory = "both")

mEscsU <- unique(mEscs)
mEscs$seqM <- seq(nrow(mEscs))
mEscsU$seqC <- seq(nrow(mEscsU))
mEscsUG <- merge(mEscsU,asAgreg,all = T)
mEscsUG$final[is.na(mEscsUG$final)] <- F

mEscsM <- merge(mEscs,mEscsUG)
mEscsD <- merge(mVotesE[,nocands],mEscsM)



