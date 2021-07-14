# read
dt <- fread("https://raw.githubusercontent.com/SpehleHSGM/Datasets/main/datatable.csv",sep = ";",dec = ",",check.names=T)

# format
dt[,Variante:=as.factor(Variante)][,Wdh:=as.factor(Wdh)][,CO2_Stufe:=as.factor(CO2_Stufe)][,N_Form:=as.factor(N_Form)]
dt[,Knollenvolumen:=as.numeric(Knollenvolumen)][,Frischmasse.Knolle:=as.numeric(Frischmasse.Knolle)]

# define outlier function: ggplot+geom_boxplot method
outlier <- function(dp){
  qt <- quantile(dp,probs=c(0.25,0.75),na.rm = TRUE)
  iqr <- IQR(dp,na.rm = TRUE)
  out <- ifelse(dp>(qt[1]-1.5*iqr) & dp<(qt[2]+1.5*iqr),FALSE,TRUE)
  return(out)
}

dt[,Flag_out:=outlier(trait), by = Variante]
