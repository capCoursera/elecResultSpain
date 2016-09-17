library("dplyr")
library("reshape2")

dHondt <- function (parties, votes, seats, threshold = 0)
{
  if (threshold != 0)
  {
    sumVotes <- sum(votes)
    votes[votes < (sumVotes * threshold)] <- 0
  }
  .temp <- data.frame(parties = rep(parties, each = seats),
                      scores = as.vector(sapply(votes, function(x)
                        x / 1:seats)))
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
  aux <- unique(datosEscrut$municData[, c(5, 6, 7)])

  return(aux[order(aux$CodAut, aux$CodProv, aux$CodMun), ])
}

AllSecc <- function(datosEscrut)
{
  aux <- unique(datosEscrut$mesaData[, c(5, 6, 7, 8, 9)])
  return(aux[order(aux$CodAut,
                   aux$CodProv,
                   aux$CodMun,
                   aux$MunDistrict,
                   aux$CodSeccion), ])
}

#Añade acrónimos de la candidatura agregada provincial, autonómica y nacional.
#Sirve para normalizar (P.S.O.E., PSG-PSOE, PSOE ... -> PSOE)
AddExtraAcrons <- function(datosEscrut)
{
  aux <- datosEscrut$candListaData[, c(1:5)]
  provAcr <- aux
  names(provAcr) <-
    c("ElType", "ElYear", "ElMonth", "CandCodeProv", "ProvAcr")
  autAcr <- aux
  names(autAcr) <-
    c("ElType", "ElYear", "ElMonth", "CandCodeAut", "AutAcr")
  nacAcr <- aux
  names(nacAcr) <-
    c("ElType", "ElYear", "ElMonth", "CandCodeNat", "NatAcr")

  result <- merge(datosEscrut$candListaData, provAcr, all.x = T)
  result <- merge(result, autAcr, all.x = T)
  result <- merge(result, nacAcr, all.x = T)

  return(result)
}

#Añade a la información de cada mesa los votos obtenidos por cada candidatura
#acron: CandAcron, ProvAcr, AutAcr, NatAcr Acrónimo de la candidatura.
#aut,prov,mun: filtra para obtener sólo los resultados de autonomía, provincia o
#              municipio (codigo INE)
#exclCREA: elimina los datos de resultados CREA. AVISO: si no se eliminan no se
#              pueden hacer operaciones por columna ya que los datos CREA también
#              van incluidos en la entidad correspondiente.
ProcessMesasMIR <-
  function(datosEscrut,
           acron = "NatAcr",
           aut = NULL,
           prov = NULL,
           mun = NULL,
           exclCREA = T,
           calcAgregados = F)
  {
    Cands <- AddExtraAcrons(datosEscrut)
    AuxDatosMesa <- addExtraInfoMesas(datosEscrut)
    AuxResultMesa <- datosEscrut$mesaResult

    #Elimina la información de votantes en el extranjero (CREA) por redundante con
    #la de las secciones correspondientes.
    if (exclCREA)
    {
      AuxDatosMesa <-
        AuxDatosMesa[AuxDatosMesa$CodAut != 99 &
                       AuxDatosMesa$CodMun != 999, ]
      AuxResultMesa <-
        AuxResultMesa[AuxResultMesa$CodAut != 99 &
                        AuxResultMesa$CodMun != 999 , ]
    }
    if (!is.null(aut))
    {
      AuxDatosMesa <- AuxDatosMesa[AuxDatosMesa$CodAut == aut, ]
      AuxResultMesa <- AuxResultMesa[AuxResultMesa$CodAut == aut, ]
    }
    if (!is.null(prov))
    {
      AuxDatosMesa <- AuxDatosMesa[AuxDatosMesa$CodProv == prov, ]
      AuxResultMesa <-
        AuxResultMesa[AuxResultMesa$CodProv == prov, ]
    }
    if (!is.null(mun))
    {
      AuxDatosMesa <- AuxDatosMesa[AuxDatosMesa$CodMun == mun, ]
      AuxResultMesa <- AuxResultMesa[AuxResultMesa$CodMun == mun, ]
    }

    ResultMesaAcr <-
      merge(AuxResultMesa, Cands[, c("ElType", "ElYear", "ElMonth", "CandCode", acron)])

    formulaPat <-
      sprintf(
        "ElType + ElYear + ElMonth + Round + CodAut + CodProv + CodMun + MunDistrict + CodSeccion + CodMesa ~ %s",
        acron
      )

    ResultMesaEscr <-
      dcast(
        data = ResultMesaAcr,
        formula = as.formula(formulaPat),
        value.var = "CandVotos"
      )

    #¿Qué hacer con los sitios donde no se ha presentado? ¿NA o 0?
    #ResultMesaEscr[is.na(ResultMesaEscr)]=0

    namesCands <-
      setdiff(names(ResultMesaEscr), names(ResultMesaAcr))

    DatosMesaEscr <- merge(AuxDatosMesa, ResultMesaEscr)

    result = list(votosMesa = DatosMesaEscr, candidaturas = namesCands)

    return(result)
  }

#Añade información de Distrito Electoral, Partido Judicial, Diputación Prov, comarca
#y num de mesas en el municipio (act no se usan ni DistElect ni Comarca)
addExtraInfoMesas <- function(datosEscrut)
{
  mesaInfo <- datosEscrut$mesaData
  municInfo <-
    datosEscrut$municData[datosEscrut$municData$MunDistrict == 99, ] #Solo ciudad completa, no distritos de C. Grandes

  mergedData <- merge(municInfo[, c(1:7, 9:13, 15)], mesaInfo)

  return(mergedData)
}

ResultadosAcumMIR <-  function(datosEscrut, acron = "NatAcr")
{
  Cands <- AddExtraAcrons(datosEscrut)
  AuxDatos <- datosEscrut$municAggrData
  AuxResult <- datosEscrut$municAggrResult

  ResultAcr <-
    merge(AuxResult, Cands[, c("ElType", "ElYear", "ElMonth", "CandCode", acron)])

  formulaPat <-
    sprintf("ElType + ElYear + ElMonth + Round + CodAut + CodProv ~ %s", acron)

  ResultVotos <-
    dcast(
      data = ResultAcr,
      formula = as.formula(formulaPat),
      value.var = "CandVotos"
    )
  ResultAsientos <-
    dcast(
      data = ResultAcr,
      formula = as.formula(formulaPat),
      value.var = "CandObt"
    )

  namesCands <-
    setdiff(names(ResultVotos), names(ResultAcr))

  #¿Qué hacer con los sitios donde no se ha presentado? ¿NA o 0?
  #ResultVotos[is.na(ResultVotos)] <- 0
  #ResultAsientos[is.na(ResultAsientos)] <- 0

  VotosEscr <- merge(AuxDatos, ResultVotos)
  AsientosEscr <- merge(AuxDatos, ResultAsientos)

  result = list(votos = VotosEscr,
                asientos = AsientosEscr,
                candidaturas = namesCands)

  return(result)
}

mesaRandom <- function(resultados, n = 1)
{
  return(resMesas$votosMesa[sample(dim(resMesas$votosMesa)[1], n), ])
}

massiveDHondtRBIND <- function(votes, parties, seats, threshold = 0)
{
  if (class(votes) == "numeric")
  {
    if (length(votes) != length(parties))
    {
      stop(
        "ORROR: votes (",
        length(votes),
        ") and parties (",
        length(parties),
        ") doesn't have the same length"
      )
    }

    votesDF <-
      data.frame(matrix(
        votes,
        nrow = 1,
        dimnames = list(NULL, parties)
      ))
  } else if (class(votes) == "data.frame")
  {
    if (ncol(votes) != length(parties))
    {
      stop(
        "ORROR: votes (",
        ncol(votes),
        "(and parties (",
        length(parties),
        ") doesn't have the same length"
      )
    }

    votesDF <- votes
  }

  if (length(seats) == 1)
  {
    seatsEff <- rep(seats, nrow(votesDF))
  } else
  {
    if (length(seats) != nrow(votesDF))
    {
      stop(
        "ORROR: seats list length (",
        length(seats),
        ") different than number of cases (",
        nrow(votesDF),
        ") "
      )
    }
    seatsEff <- seats
  }
  votesEff <- votesDF

  if (threshold > 0)
  {
    sumVotes <- rowSums(votesDF)
    votesEff[votesDF < (sumVotes * threshold)] <- 0
  }

  #Is there a better way to create an empty dataframe?
  aux <- data.frame(matrix(nrow=0,ncol=length(cands),dimnames = list(NULL,parties)))

  for(x in seq(nrow(votesEff)))
  {
    aux <- rbind(aux,dHondt2(parties = parties,
                             votes=votesEff[x,],
                             seats=seatsEff[x],
                             threshold = threshold))
  }

  return(aux)
}


massiveDHondt <- function(votes, parties, seats, threshold = 0)
{
  if (class(votes) == "numeric")
  {
    if (length(votes) != length(parties))
    {
      stop(
        "ORROR: votes (",
        length(votes),
        ") and parties (",
        length(parties),
        ") doesn't have the same length"
      )
    }

    votesDF <-
      data.frame(matrix(
        votes,
        nrow = 1,
        dimnames = list(NULL, parties)
      ))
  } else if (class(votes) == "data.frame")
  {
    if (ncol(votes) != length(parties))
    {
      stop(
        "ORROR: votes (",
        ncol(votes),
        "(and parties (",
        length(parties),
        ") doesn't have the same length"
      )
    }

    votesDF <- votes
  }

  if (length(seats) == 1)
  {
    seatsEff <- rep(seats, nrow(votesDF))
  } else
  {
    if (length(seats) != nrow(votesDF))
    {
      stop(
        "ORROR: seats list length (",
        length(seats),
        ") different than number of cases (",
        nrow(votesDF),
        ") "
      )
    }
    seatsEff <- seats
  }
  votesEff <- votesDF

  if (threshold > 0)
  {
    sumVotes <- rowSums(votesDF)
    votesEff[votesDF < (sumVotes * threshold)] <- 0
  }

  #Is there a better way to create an empty dataframe?
  aux <- data.frame(matrix(nrow=0,ncol=length(cands),dimnames = list(NULL,parties)))

  auxVAP <- vapply(seq(nrow(votesEff)), FUN=function(x) {dHondt2(parties = parties,
                                                             votes=votesEff[x,],
                                                             seats=seatsEff[x],
                                                             threshold = threshold)},
                  FUN.VALUE = aux,USE.NAMES = F)
  result <- data.frame(matrix(unlist(auxVAP,use.names = F),ncol = length(parties),byrow = T,dimnames = list(NULL,parties)))
  names(result) <- parties

  # for(x in seq(nrow(votesEff)))
  # {
  #   aux <- rbind(aux,dHondt2(parties = parties,
  #                            votes=votesEff[x,],
  #                            seats=seatsEff[x],
  #                            threshold = threshold))
  # }

  return(result)
}


dHondt2 <- function (parties, votes, seats, threshold = 0)
{
  if (threshold != 0)
  {
    sumVotes <- sum(votes)
    votes[votes < (sumVotes * threshold)] <- 0
  }

  .temp <- data.frame(parties = rep(parties, each = seats),
                      scores = as.vector(sapply(votes, function(x)
                        x / 1:seats)))
  out <- with(.temp, (parties[order(-scores)][1:seats]))

  tabout <- table(out)
  tabout[is.na(votes)] <- NA
  result <- data.frame(matrix(as.vector(tabout), nrow = 1, byrow = T))
  names(result) <- names(tabout)

  return(result)
}
