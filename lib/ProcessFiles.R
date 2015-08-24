
#From FICHEROS.doc coming from 04201105_MESA.zip
# “Tipo de elección” (columnas 1-2) que serán idénticos y se ajustarán a los siguientes:
#   01 = Referéndum
# 02 = Congreso
# 03 = Senado
# 04 = Municipales, Partidos Judiciales y Diputaciones Provinciales
# 05 = Autonómicas
# 06 = Cabildos Insulares
# 07 = Parlamento Europeo
# 15 = Juntas Generales.

#Muestra de función para leer ficheros
# readXXfile <- function(filename)
# {
#   #Comentario descriptivo del fichero (viene del FICHEROS.{doc|rtf} incluido en el .ZIP)
#   # "Nombre campo"  Descripción (viene del FICHEROS.{doc|rtf} incluido en el .ZIP)
#   # ...
#
#
#   widthInfo <- c (# Longitudes de los campos (columna 4 ))
#   colNames <- c( #Nombres de los campos, EN EL ORDDEN DE LA TABLA)
#
#   data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)
#
#   return(data)
# }

readElecFile <- function(file,...)
{
  data <- read.fwf(file = file,
                   strip.white = T,
                   fileEncoding="Latin1",
                   ...)

  return(data)
}

#Fichero de CONTROL de los ficheros que componen el proceso electoral.
read01file <- function(filename)
{
  # "ElType"  Tipo de elección.
  # "ElYear"  Año del proceso electoral.
  # "ElMonth" Mes del proceso electoral.
  # "Round"  Número de vuelta. (en procesos a una sola vuelta o Referéndum = 1).
  # "Incl01"  Siempre 1 (se adjunta fichero 01xxaamm.dat).
  # "Incl02"  1/0 (se adjunta/no se adjunta el fichero 02xxaamm.dat).
  # "Incl03"  1/0 (se adjunta/no se adjunta el fichero 03xxaamm.dat).
  # "Incl04"  1/0 (se adjunta/no se adjunta el fichero 04xxaamm.dat).
  # "Incl05"  1/0 (se adjunta/no se adjunta el fichero 05xxaamm.dat).
  # "Incl06"  1/0 (se adjunta/no se adjunta el fichero 06xxaamm.dat).
  # "Incl07"  1/0 (se adjunta/no se adjunta el fichero 07xxaamm.dat).
  # "Incl08"  1/0 (se adjunta/no se adjunta el fichero 08xxaamm.dat).
  # "Incl09"  1/0 (se adjunta/no se adjunta el fichero 09xxaamm.dat).
  # "Incl10"  1/0 (se adjunta/no se adjunta el fichero 10xxaamm.dat).
  # "Incl1104"  1/0 (se adjunta/no se adjunta el fichero 1104aamm.dat).
  # "Incl1204"  1/0 (se adjunta/no se adjunta el fichero 1204aamm.dat).
  # "Incl0510"  1/0 (se adjunta/no se adjunta el fichero 0510aamm.dat).
  # "Incl0610"  1/0 (se adjunta/no se adjunta el fichero 0610aamm.dat).
  # "Incl0710"  1/0 (se adjunta/no se adjunta el fichero 0710aamm.dat).
  # "Incl0810"  1/0 (se adjunta/no se adjunta el fichero 0810aamm.dat).

  widthInfo <- c (2,4,2,1,rep_len(1,16))
  colNames <- c("ElType","ElYear","ElMonth","Round",
                "Incl01","Incl02","Incl03","Incl04",
                "Incl05","Incl06","Incl07","Incl08","Incl09",
                "Incl10","Incl1104","Incl1204","Incl0510",
                "Incl0610","Incl0710","Incl0810")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de IDENTIFICACION del proceso electoral.
read02file <- function(filename)
{

#   "ElType"  Tipo de elección.
#   "ElYear" Año del proceso electoral.
#   "ElMonth" Mes del proceso electoral.
#   "Round" Número de vuelta. (en procesos a una sola vuelta o Referéndum = 1).
#   "ScopeType" Tipo de ámbito (N=nacional, A=autonómico).
#   "Scope" Ámbito territorial del proceso electoral.
#   "ElDay" Fecha de celebración del proceso electoral (DIA).
#   "ElMonthB" Fecha de celebración del proceso electoral (MES).
#   "ElYearB" Fecha de celebración del proceso electoral (AÑO).
#   (en formato ‘HH:MM’ de 24 horas)
#   "boxOpenTime" Hora de apertura de los ‘Colegios Electorales’ .
#   "boxCloseTime" Hora de cierre de los ‘Colegios Electorales’.
#   "partAdv1Time" Hora del primer ‘Avance de Participación’.
#   "partAdv2Time" Hora del segundo ‘Avance de Participación’.

  widthInfo <- c (2,4,2,1,1,2,
                  2,2,4,
                  5,5,5,5)
  colNames <- c("ElType","ElYear","ElMonth","Round","ScopeType","Scope",
                "ElDay","ElMonthB","ElYearB",
                "boxOpenTime","boxCloseTime","partAdv1Time","partAdv2Time")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#3.- Fichero de CANDIDATURAS.
read03file <- function(filename)
{
#"ElType"   Tipo de elección.
#"ElYear"   Año del proceso electoral.
#"ElMonth"   Mes del proceso electoral.
#"candCode"   Código de la candidatura.
#"candAcronym"   Siglas de la candidatura.
#"candName"   Denominación de la candidatura.
#"candCodeProv"   Código de la candidatura cabecera de acumulación a nivel provincial.
#"candCodeAut"   Código de la candidatura cabecera de acumulación a nivel autonómico.
#"candCodeNat"   Código de la candidatura cabecera de acumulación a nivel nacional.

  widthInfo <- c (2,4,2,6,
                  50,150,
                  6,6,6)
  colNames <- c("ElType","ElYear","ElMonth",
                "candCode","candAcronym","candName",
                "candCodeProv","candCodeAut","candCodeNat"
                )
  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#4.- Fichero de RELACION DE CANDIDATOS.
read04file <- function(filename)
{
  #"ElType" Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1).
  #"CodProv" Código I.N.E.de la provincia (99 en elecciones al Parlamento Europeo).
  #"District" Distrito electoral cuando corresponda o 9 en elecciones que no tienen este tipo de circunscripción.
  #"CodMun" Código I.N.E. del municipio (elecciones municipales) o del Senador (elecciones al Senado). En el resto de procesos electorales llevará siempre 999.
  #"CodListCand" Código de la candidatura.
  #"CandListOrder" Número de orden del candidato.
  #"CandType" Tipo de candidato (T = Titular, S = Suplente).
  #"CandFirstName" Nombre del candidato.
  #"Cand1LastName" Primer apellido del candidato.
  #"Cand2LastName" Segundo apellido del candidato.
  #"CandSex" Sexo del candidato (Masculino/Femenino).
  #"CandBirthDay" Fecha de nacimiento del candidato (DIA).
  #"CandBirthMonth" Fecha de nacimiento del candidato (MES).
  #"CandBirthYear" Fecha de nacimiento del candidato (AÑO).
  #"CandDNI" D.N.I. del candidato.
  #"CandElected" Candidato elegido (Si/No).


  widthInfo <- c (2,4,2,1,
                  2,1,3,
                  6,3,1,
                  25,25,25,
                  1,2,2,4,
                  10,1
                  )
  colNames <- c("ElType","ElYear","ElMonth","Round",
                "CodProv","District","CodMun",
                "CodListCand","CandListOrder","CandType",
                "CandFirstName","Cand1LastName","Cand2LastName",
                "CandSex","CandBirthDay","CandBirthMonth","CandBirthYear",
                "CandDNI","CandElected"
                )
  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de DATOS COMUNES DE MUNICIPIOS.
read05file <- function(filename)
{

  #"ElType"   Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1).
  #"CodAut"   Código de la Comunidad Autónoma.
  #"CodProv" Código I.N.E. de la provincia.
  #"CodMun" Código I.N.E. del municipio.
  #"MunDistrict" Número de distrito municipal en su caso o 99 si es el total municipal.
  #"MunName" Nombre del municipio o del distrito municipal.
  #"EleDistrict" Código del Distrito Electoral cuando corresponda o 0 en elecciones que no tienen este tipo de circunscripción.
  #"CodPJ" Código del Partido Judicial.
  #"CodDP" Código de la Diputación Provincial.
  #"CodCom" Código de la comarca.
  #"PobDerecho"   Población de derecho.
  #"NumMesas" Número de mesas.
  #"CensINE"  Censo del I.N.E.
  #"CensEscr" Censo de escrutinio.
  #"CensCERE" Censo C.E.R.E. en escrutinio (Residentes Extranjeros).
  #"VotsCERE" Total votantes C.E.R.E. (Residentes Extranjeros).
  #"Vots1Av" Votantes del primer avance de participación.
  #"Vots2Av" Votantes del segundo avance de participación.
  #"VotsBlanco" Votos en blanco.
  #"VotsNulo" Votos nulos.
  #"VotCands" Votos a candidaturas.
  #"NumEscs" Número de ‘Escaños’ a distribuir cuando el municipio es la circunscripción electoral. Ceros en otros casos.
  #"VotsSI" Votos afirmativos en Referéndum o ceros en otros procesos electorales.
  #"VotsNO" Votos negativos en Referéndum o ceros en otros procesos electorales.
  #"DatOfic" Datos oficiales (Si/No).


  widthInfo <- c (2,4,2,1,
                  2,2,3,2,
                  100,1,3,3,3,8,5,8,8,8,8,8,8,8,8,8,3,8,8,1)

  colNames <- c( "ElType","ElYear","ElMonth","Round",
                 "CodAut","CodProv","CodMun","MunDistrict","MunName",
                 "EleDistrict","CodPJ","CodDP","CodCom",
                 "PobDerecho","NumMesas","CensINE","CensEscr","CensCERE",
                 "VotsCERE","Vots1Av","Vots2Av","VotsBlanco","VotsNulo",
                 "VotCands","NumEscs","VotsSI","VotsNO","DatOfic")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

# Fichero de DATOS DE CANDIDATURAS DE MUNICIPIOS
read06file <- function(filename)
{

  #"ElType"   Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1).
  #"CodProv" Código I.N.E. de la provincia.
  #"CodMun" Código I.N.E. del municipio.
  #"MunDistrict" Número de distrito municipal en su caso o 99 si es el total municipal.
  #"CodCand" Código de la candidatura o del Senador.
  #"VotCand" Votos obtenidos por la candidatura.
  #"ObtCand" Número de candidatos obtenidos por la candidatura.


  widthInfo <- c (2,4,2,1,
                  2,3,2,6,
                  8,3)

  colNames <- c("ElType","ElYear","ElMonth","Round",
                "CodProv","CodMun","MunDistrict","CodCand",
                "VotCand","ObtCand"
                )

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de DATOS COMUNES DE AMBITO SUPERIOR AL MUNICIPIO.
read07file <- function(filename)
{

  #"ElType"   Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1) o Número de pregunta en Referéndum.
  #"CodAut" Código de la Comunidad Autónoma. En el caso de Total Nacional, llevará 99.
  #"CodProv" Código I.N.E. de la provincia o 99 si se trata de datos a nivel Total Comunidad o Total Nacional.
  #"EleDistrict" Código del Distrito Electoral que corresponda o 9 en datos a nivel Total Provincial, Comunidad o Nacional.
  #"NomAmbTerr" Nombre del ámbito territorial.
  #"PobDerecho"   Población de derecho.
  #"NumMesas" Número de mesas.
  #"CensINE"  Censo del I.N.E.
  #"CensEscr" Censo de escrutinio.
  #"CensCERE" Censo C.E.R.E. en escrutinio (Residentes Extranjeros).
  #"VotsCERE" Total votantes C.E.R.E. (Residentes Extranjeros).
  #"Vots1Av" Votantes del primer avance de participación.
  #"Vots2Av" Votantes del segundo avance de participación.
  #"VotsBlanco" Votos en blanco.
  #"VotsNulo" Votos nulos.
  #"VotCands" Votos a candidaturas.
  #"NumEscs" Número de ‘Escaños’ a distribuir cuando el ámbito coincida con la circunscripción electoral o Total de ‘Escaños’ distribuidos en el ámbito. Ceros en el caso de que el ámbito sea inferior a la circunscripción electoral.
  #"VotsSI" Votos afirmativos en Referéndum o ceros en otros procesos electorales.
  #"VotsNO" Votos negativos en Referéndum o ceros en otros procesos electorales.
  #"DatOfic" Datos oficiales (Si/No).

  widthInfo <- c (2,4,2,1,2,2,1,50,8,5,8,8,8,8,8,8,8,8,8,6,8,8,1)
  colNames <- c("ElType","ElYear","ElMonth","Round",
                "CodAut","CodProv","EleDistrict","NomAmbTerr",
                "PobDerecho","NumMesas","CensINE","CensEscr",
                "CensCERE","VotsCERE",
                "Vots1Av","Vots2Av","VotsBlanco","VotsNulo","VotCands",
                "NumEscs","VotsSI","VotsNO","DatOfic")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de DATOS DE CANDIDATURAS DE AMBITO SUPERIOR AL MUNICIPIO.
read08file <- function(filename)
{
  #"ElType"   Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1).
  #"CodAut" Código de la Comunidad Autónoma. En el caso de Total Nacional, llevará 99.
  #"CodProv" Código I.N.E. de la provincia o 99 si se trata de datos a nivel Total Comunidad o Total Nacional.
  #"EleDistrict" Código del Distrito Electoral que corresponda o 9 en datos a nivel Total Provincial, Comunidad o Nacional.
  #"CodCand" Código de la candidatura o del Senador.
  #"VotCand" Votos obtenidos por la candidatura.
  #"ObtCand" Número de candidatos obtenidos por la candidatura.

  widthInfo <- c (2,4,2,1,2,2,1,6,8,5)
  colNames <- c("ElType","ElYear","ElMonth","Round",
                "CodAut","CodProv","EleDistrict",
                "CodCand","VotCand","ObtCand")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de DATOS COMUNES DE MESAS y del C.E.R.A.
read09file <- function(filename)
{
  #"ElType"   Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1) o Número de pregunta en Referéndum.

  #"CodAut" Código de la Comunidad Autónoma o 99 si se trata del Total Nacional del C.E.R.A.
  #"CodProv" Código I.N.E. de la provincia o 99 si se trata del Total Nacional o Autonómico del C.E.R.A.
  #"CodMun" Código I.N.E. del municipio (999 = C.E.R.A.).
  #"MunDistrict" Número de distrito municipal en su caso o 01 si el municipio no tiene distritos (distrito único). En el caso de datos procedentes del C.E.R.A., llevará el número del ‘Distrito Electoral’ a que correspondan o 09 si el ámbito de dicho distrito coincide con el de la provincia.
  #"CodSeccion" Código de la sección (tres dígitos seguidos de un espacio, letra mayúscula u otro dígito).
  #"CodMesa" Código de la mesa (una letra mayúscula identificando la mesa o una ‘U’ en caso de mesa única).
  #"CensINE"  Censo del I.N.E.
  #"CensEscr" Censo de escrutinio o censo C.E.R.A.
  #"CensCERE" Censo C.E.R.E. en escrutinio (Residentes Extranjeros).
  #"VotsCERE" Total votantes C.E.R.E. (Residentes Extranjeros).
  #"Vots1Av" Votantes del primer avance de participación.
  #"Vots2Av" Votantes del segundo avance de participación.
  #"VotsBlanco" Votos en blanco.
  #"VotsNulo" Votos nulos.
  #"VotCands" Votos a candidaturas.
  #"VotsSI" Votos afirmativos en Referéndum o ceros en otros procesos electorales.
  #"VotsNO" Votos negativos en Referéndum o ceros en otros procesos electorales.
  #"DatOfic" Datos oficiales (Si/No).

  widthInfo <- c (2,4,2,1,2,2,3,2,4,1,7,7,7,7,7,7,7,7,7,7,7,1)
  colNames <- c("ElType","ElYear","ElMonth","Round",
                "CodAut","CodProv","CodMun","MunDistrict","CodSeccion","CodMesa",
                "CensINE","CensEscr","CensCERE","VotsCERE","Vots1Av","Vots2Av",
                "VotsBlanco","VotsNulo","VotCands","VotsSI","VotsNO","DatOfic"
                )

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de DATOS DE CANDIDATURAS DE MESAS y del C.E.R.A.
read10file <- function(filename)
{
  #"ElType"   Tipo de elección.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1) o Número de pregunta en Referéndum.
  #"CodAut" Código de la Comunidad Autónoma o 99 si se trata del Total Nacional del C.E.R.A.
  #"CodProv" Código I.N.E. de la provincia o 99 si se trata del Total Nacional o Autonómico del C.E.R.A.
  #"CodMun" Código I.N.E. del municipio (999 = C.E.R.A.).
  #"MunDistrict" Número de distrito municipal en su caso o 01 si el municipio no tiene distritos (distrito único). En el caso de datos procedentes del C.E.R.A., llevará el número del ‘Distrito Electoral’ a que correspondan o 09 si el ámbito de dicho distrito coincide con el de la provincia.
  #"CodSeccion" Código de la sección (tres dígitos seguidos de un espacio, letra mayúscula u otro dígito).
  #"CodMesa" Código de la mesa (una letra mayúscula identificando la mesa o una ‘U’ en caso de mesa única).
  #"CodCand" Código de la candidatura o del Senador en elecciones al Senado.
  #"VotCand" Votos obtenidos por la candidatura o el Senador.

  widthInfo <- c (2,4,2,1,2,2,3,2,4,1,6,7)
  colNames <- c("ElType","ElYear","ElMonth","Round",
                "CodAut","CodProv","CodMun","MunDistrict",
                "CodSeccion","CodMesa","CodCand","VotCand")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

#Fichero de DATOS COMUNES DE MUNICIPIOS menores de 250 habitantes. (Solo en Elecciones Municipales)
read1104file <- function(filename)
{
  #"TipoMun" Tipo de municipio: 08 = entre 100 y 250 habitantes 09 = menores de 100 habitantes.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1).
  #"CodAut"   Código de la Comunidad Autónoma.
  #"CodProv" Código I.N.E. de la provincia.
  #"CodMun" Código I.N.E. del municipio.
  #"MunName" Nombre del municipio o del distrito municipal.
  # Código del Partido Judicial.
  #"CodPJ" Código del Partido Judicial.
  #"CodDP" Código de la Diputación Provincial.
  #"CodCom" Código de la comarca.
  #"PobDerecho"   Población de derecho.
  #"NumMesas" Número de mesas.
  #"CensINE"  Censo del I.N.E.
  #"CensEscr" Censo de escrutinio.
  #"CensCERE" Censo C.E.R.E. en escrutinio (Residentes Extranjeros).
  #"VotsCERE" Total votantes C.E.R.E. (Residentes Extranjeros).
  #"Vots1Av" Votantes del primer avance de participación.
  #"Vots2Av" Votantes del segundo avance de participación.
  #"VotsBlanco" Votos en blanco.
  #"VotsNulo" Votos nulos.
  #"VotCands" Votos a candidaturas.
  #"NumEscs" Número de ‘Escaños’ a distribuir.
  #"DatOfic" Datos oficiales (Si/No).

  widthInfo <- c (2,4,2,1,2,2,3,100,3,3,3,3,2,3,3,3,3,3,3,3,3,3,2,1)
  colNames <- c("TipoMun","ElYear","ElMonth","Round",
                "CodAut","CodProv","CodMun","MunName","CodPJ","CodDP","CodCom",
                "PobDerecho","NumMesas","CensINE","CensEscr","CensCERE",
                "VotsCERE","Vots1Av","Vots2Av","VotsBlanco","VotsNulo",
                "VotCands","NumEscs","DatOfic")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}

# Fichero de DATOS DE CANDIDATURAS DE MUNICIPIOS menores de 250 hab. (Solo en Elecciones Municipales)
read1204file <- function(filename)
{
  #"TipoMun" Tipo de municipio: 08 = entre 100 y 250 habitantes 09 = menores de 100 habitantes.
  #"ElYear"   Año del proceso electoral.
  #"ElMonth"   Mes del proceso electoral.
  #"Round" Número de vuelta (en procesos a una sola vuelta = 1).
  #"CodProv" Código I.N.E. de la provincia.
  #"CodMun" Código I.N.E. del municipio.
  #"CandCode"   Código de la candidatura.  #"VotCand" Votos obtenidos por la candidatura.
  #"VotsCand" Votos obtenidos por la candidatura.
  #"CandElect" Número de candidatos obtenidos por la candidatura.
  #"CandFirstName" Nombre del candidato.
  #"Cand1LastName" Primer apellido del candidato.
  #"Cand2LastName" Segundo apellido del candidato.
  #"CandSex" Sexo del candidato (Masculino/Femenino).
  #"CandBirthDay" Fecha de nacimiento del candidato (DIA).
  #"CandBirthMonth" Fecha de nacimiento del candidato (MES).
  #"CandBirthYear" Fecha de nacimiento del candidato (AÑO).
  #"CandDNI" D.N.I. del candidato.
  #"VotsCandidato" Votos obtenidos por el candidato.
  #"CandElected" Candidato elegido (Si/No).




  widthInfo <- c (2,4,2,1,2,3,6,3,2,25,25,25,1,2,2,4,10,3,1)
  colNames <- c("TipoMun","ElYear","ElMonth","Round","CodProv","CodMun",
                "CandCode","VotsCand","CandElect",
                "CandFirstName","Cand1LastName","Cand2LastName",
                "CandSex","CandBirthDay","CandBirthMonth","CandBirthYear",
                "CandDNI","VotsCandidato","CandElected")

  data <- readElecFile(file = filename,widths = widthInfo,col.names =colNames)

  return(data)
}
