devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

setwd("C:/Users/lro2402unn/RegistreGIT/deformitet")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/deformitet11a91614a.sql__20260617_144241.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")

# source c://Users/lro2402unn/RegistreGIT/data/deformitet11a91614a.sql;

library(deformitet)
source("C:/Users/lro2402unn/RegistreGIT/deformitet/dev/sysSetenv.R")
deformitet::kjorDeformApp(browser = TRUE)


#Linting:
styler::style_pkg()

# Få andelGrVar opp å kjøre:

raw_regdata <- alleRegData()
surgeon_form <- hentDataTabell("surgeonform", egneVarNavn = 0)

RegDataRaa <- alleRegData(egneVarNavn = 0)
RegData <- preprosData(RegData=RegDataRaa, egneVarNavn = 0)

length(unique(RegData))

#### Clean and tidy data:
regData <- preprosData(raw_regdata)

RegData <- alleRegData(egneVarNavn = 0)
RegData <- preprosData(RegData=RegData, egneVarNavn = 0)
kvalInd <- c('liggetidPostOp', 'fornoydBeh2aar', 'reOp')
for (var in kvalInd) {
  valgtVar <- var
  figAndelerGrVar(RegData=RegData, hentData=0, preprosess=0,
                valgtVar=valgtVar,
                datoFra='2023-01-01', datoTil=Sys.Date(),
                Ngrense=10, reshID=0, outfile=paste0(valgtVar, '.pdf'))
}
