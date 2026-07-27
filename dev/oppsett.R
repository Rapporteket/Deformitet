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
# resh: 102467  103240  111961
# Få fordelingsfigur opp å kjøre:
raw_regdata <- alleRegData(egneVarNavn = 0)
RegDataPre <- preprosData(raw_regdata)
RegData <- RegDataPre
DefFordeling(RegData, valgtVar='alder', datoFra = '2023-01-01', datoTil = Sys.Date(),
                          aar = 0, minald = 0, maxald = 110, erMann = '',
                          outfile = '', reshID = 111961, enhetsUtvalg = 1)

data_sykeh <- RegDataUtv
data_alle <- RegDataUtv

tab <- lagFordelingstabell(RegData=data_sykeh) #, var_reshid, visning = 'Hele landet')

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
