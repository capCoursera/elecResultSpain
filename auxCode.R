v6 <- read06file("data/06041105.DAT")
v5 <- read05file("data/05041105.DAT")
v4 <- read04file("data/04041105.DAT")
v3 <- read03file("data/03041105.DAT")
v2 <- read02file("data/02041105.DAT")
v1 <- read01file(filename = "data/01041105.DAT")
merge(v3,v6[(v6$CodProv == 28) & (v6$CodMun ==79) & (v6$MunDistrict == 99),]) -> resMad
