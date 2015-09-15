source('~/devel/elecResultSpain/lib/ProcessFiles.R')

#1.- Fichero de CONTROL de los ficheros que componen el proceso electoral.
v1 <- read01file("data/01041105.DAT")

#2.- Fichero de IDENTIFICACION del proceso electoral.
v2 <- read02file("data/02041105.DAT")

#3.- Fichero de CANDIDATURAS.
v3 <- read03file("data/03041105.DAT")

#4.- Fichero de RELACION DE CANDIDATOS.
v4 <- read04file("data/04041105.DAT")

#5.- Fichero de DATOS COMUNES DE MUNICIPIOS.
v5 <- rbind(read05file("data/05041105.DAT"),read05file("data/05101105.DAT"))

#6.- Fichero de DATOS DE CANDIDATURAS DE MUNICIPIOS
v6 <- rbind(read06file("data/06041105.DAT"),read06file("data/06101105.DAT"))

#7.- Fichero de DATOS COMUNES DE AMBITO SUPERIOR AL MUNICIPIO.
v7 <- rbind(read07file("data/07041105.DAT"),read07file("data/07101105.DAT"))

#8.- Fichero de DATOS DE CANDIDATURAS DE AMBITO SUPERIOR AL MUNICIPIO.
v8 <- rbind(read08file("data/08041105.DAT"),read08file("data/08101105.DAT"))

#9.- Fichero de DATOS COMUNES DE MESAS y del C.E.R.A.
v9 <- read09file("data/09041105.DAT")

#10.- Fichero de DATOS DE CANDIDATURAS DE MESAS y del C.E.R.A.
v10 <- read10file("data/10041105.DAT")

#1104.- Fichero de DATOS COMUNES DE MUNICIPIOS menores de 250 habitantes. (Solo en Elecciones Municipales)
v1104 <- read1104file("data/11041105.DAT")

#1204.- Fichero de DATOS DE CANDIDATURAS DE MUNICIPIOS menores de 250 hab. (Solo en Elecciones Municipales)
v1204 <- read1204file("data/12041105.DAT")

merge(v3,v6[(v6$CodProv == 28) & (v6$CodMun ==79) & (v6$MunDistrict == 99),]) -> resMad
merge(v6[(v6$CodProv == 28) & (v6$MunDistrict == 99),],v3) -> resComMad
resMad[,c(1:3,5:8,11,9)]->resMadVotos

dcast(data= resMadVotos, formula = ElType+ElYear+ElMonth+Round+CodProv+CodMun+MunDistrict~ candAcronym, value.var="CandVotos", fill=0) -> rowMadVotes

resMad[,c(1:3,5:8,11,10)]->resMadEsc
dcast(data= resMadEsc, formula = ElType+ElYear+ElMonth+Round+CodProv+CodMun+MunDistrict~ candAcronym, value.var="CandObt", fill=0) -> rowMadEsc


