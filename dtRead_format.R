# read data
dt <- fread("https://raw.githubusercontent.com/SpehleHSGM/Datasets/main/datatable.csv",sep = ";",dec = ",",check.names=T)

# format
dt[,Wdh:=as.factor(Wdh)][,CO2_Stufe:=as.factor(CO2_Stufe)][,N_Form:=as.factor(N_Form)]
dt[,Knollenvolumen:=as.numeric(Knollenvolumen)][,Frischmasse.Knolle:=as.numeric(Frischmasse.Knolle)]

# define outlier function: Whisker by John W. Tukey 
outlier.iqr <- function(dp){
  qt <- quantile(dp,probs=c(0.25,0.75),na.rm = TRUE)
  iqr <- IQR(dp,na.rm = TRUE)
  out <- ifelse(dp>(qt[1]-1.5*iqr) & dp<(qt[2]+1.5*iqr),FALSE,TRUE)
  return(out)
}
# define outlier function: median absolute deviation (MAD)
outlier.mad <- function(dp){
  med <- median(dp)
  mad <- mad(dp, constant=1)
  out <- ifelse((dp > med-5.2*mad) & (dp < med + 5.2*mad),FALSE,TRUE)
  return(out)
}

# add column Flag_outlier.iqr
dt[,Flag_outlier.iqr:=outlier.iqr(eval(traitName)),by=interaction(CO2_Stufe,N_Form)]

# add column Flag_outlier.mad
dt[,Flag_outlier.mad:=outlier.mad(eval(traitName)),by=interaction(CO2_Stufe,N_Form)]

# add column Flag_outlier.bonf (mean shift outlier)
dt[,Flag_outlier.bonf:=outlierTest(lm(eval(traitName)~CO2_Stufe*N_Form),cutoff=Inf, n.max=Inf,order=FALSE)$bonf.p < 0.05]

# data.table aggregate of trait means by Wdh*CO2_Stufe*N_Form
dt_means <- dt[ ,.(Knollenvolumen=mean(Knollenvolumen),
                  Frischmasse.Knolle=mean(Frischmasse.Knolle),
                  count=.N,
                  mainPlots=factor(Wdh:CO2_Stufe)),
               by=list(Wdh,CO2_Stufe,N_Form)]

# add column with boolean "Flag_outlier" (TRUE/FALSE)
dt_means[,Flag_outlier.iqr:=outlier.iqr(eval(traitName)),by=interaction(CO2_Stufe,N_Form)]
