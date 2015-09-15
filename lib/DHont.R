library("dplyr")
library("reshape2")

dHondt <- function (parties, votes, seats,threshold=0)
{ if (threshold != 0)
  {
    sumVotes <- sum(votes)
    votes[votes < (sumVotes*threshold)] <- 0
  }
  .temp <- data.frame(parties = rep(parties, each = seats),
                      scores = as.vector(sapply(votes, function(x) x/1:seats)))
  out <- with(.temp, (parties[order(-scores)][1:seats]))
  out <- data.frame(SciencesPo::freq(out)[, 1:3])
  out <- out %>% dplyr::arrange(dplyr::desc(freq))
  names(out) <- c("Parties", "d'Hondt", "Perc")
  return(out)
}

#From FICHEROS.doc coming from 04201105_MESA.zip
# “Tipo de elección” (columnas 1-2) que serán idénticos y se ajustarán a los siguientes:
# 01 = Referéndum
# 02 = Congreso
# 03 = Senado
# 04 = Municipales, Partidos Judiciales y Diputaciones Provinciales
# 05 = Autonómicas
# 06 = Cabildos Insulares
# 07 = Parlamento Europeo
# 15 = Juntas Generales.

ElectionThreshold <- function(eleType)
{
  if (eleType == 4)
  {
    return(0.05)
  } else if (eleType == 2)
  {
    return(0.03)
  } else
  {
    return(0)
  }
}

AllMunic <- function(datosEscrut)
{
  aux <- unique(datosEscrut$municData[,c(6,7)])

  return(aux[order(aux$CodProv,aux$CodMun),])
}

ProcessMesa <- function(datosEscrut,prov,mun)
{
  AuxDatosMesa <- datosEscrut$mesaData
  AuxResultMesa <- datosEscrut$mesaResult

  DatosMesa <-
    AuxDatosMesa[AuxDatosMesa$CodProv == prov &
                   AuxDatosMesa$CodMun == mun,]
  ResultMesa <- AuxResultMesa[AuxResultMesa$CodProv == prov &
                                AuxResultMesa$CodMun == mun,]

  ResultMesaAcr <-
    merge(ResultMesa,datosEscrut$candListaData[,c(1:5)])
  ResultMesaEscr <-
    dcast(
      data = ResultMesaAcr,formula = ElType + ElYear + ElMonth + Round +
        CodProv + CodMun + MunDistrict + CodSeccion + CodMesa ~ candAcronym,
      value.var = "CandVotos"
    )
  namesCands <- setdiff(names(ResultMesaEscr),names(ResultMesaAcr))
  DatosMesaEscr <- merge(DatosMesa,ResultMesaEscr)
  VotosCands <- DatosMesaEscr[,namesCands]

}

