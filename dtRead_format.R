# read data
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

# add column with boolean "Flag_outlier" (TRUE/FALSE)
dt[,Flag_outlier:=outlier(eval(traitName)),by=interaction(CO2_Stufe,N_Form)]

# data.table aggregate of trait means by Wdh*CO2_Stufe*N_Form
dt_means <- dt[ Flag_outlier==FALSE,.(Knollenvolumen=mean(Knollenvolumen),
                  Frischmasse.Knolle=mean(Frischmasse.Knolle),
                  count=.N,
                  mainPlots=factor(Wdh:CO2_Stufe)),
               by=list(Wdh,CO2_Stufe,N_Form)]

# add column with boolean "Flag_outlier" (TRUE/FALSE)
dt_means[,Flag_outlier:=outlier(eval(traitName)),by=interaction(CO2_Stufe,N_Form)]
