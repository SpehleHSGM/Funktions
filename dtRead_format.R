dt <- fread("https://raw.githubusercontent.com/SpehleHSGM/Datasets/main/datatable.csv",sep = ";",dec = ",",check.names=T)

dt[,Variante:=as.factor(Variante)][,Wdh:=as.factor(Wdh)][,CO2_Stufe:=as.factor(CO2_Stufe)][,N_Form:=as.factor(N_Form)]
dt[,Knollenvolumen:=as.numeric(Knollenvolumen)][,Frischmasse.Knolle:=as.numeric(Frischmasse.Knolle)]
