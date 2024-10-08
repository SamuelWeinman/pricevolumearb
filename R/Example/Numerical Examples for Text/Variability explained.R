library(parallel)
library(snow)
NrPC.cand = c(15,20,25)


VarExplianed.Day = function(Returns, StandardisedVolume, t, H, NrPC.cand) {
  R = Returns[,(t-H):(t-1)]
  rho.R = ConstructRho(R)
  E.R = eigen(rho.R)

  Prop.Explained.R = sapply(1:length(NrPC.cand), function(i){
    sum(E.R$values [1:NrPC.cand[i]]) / sum(E.R$values)
  })
 
  
  V = StandardisedVolume[,(t-H):(t-1)]
  rho.V = ConstructRho(V)
  E.V = eigen(rho.V)
  
  
  Prop.Explained.V = sapply(1:length(NrPC.cand), function(i){
    sum(E.V$values [1:NrPC.cand[i]]) / sum(E.V$values )
  })
  
  return(list(
    Prop.Explained.R = Prop.Explained.R,
    Prop.Explained.V = Prop.Explained.V
  ))
}






VarExplained.TS = function(Returns, Volume, Start, End, H, NrPC.cand,d) {
  StandardisedVolume = Volume / t(roll_mean(t(as.matrix(Volume)), width = d))
  
  
  
  #VARIABLES TO SEND TO CORES FROM GLOBAL ENVIRONMENT
  Globalvarlist = c("VarExplianed.Day", "ConstructRho")
  
  #VARIABLES TO SEND TO CORES FROM FUNCTION ENVIRONMENT
  Localvarlist = c("Returns","H","NrPC.cand", "StandardisedVolume")
  
  #OPEN CORES AND TRANSFER
  cl = snow::makeCluster(detectCores()-1)
  snow::clusterExport(cl, Globalvarlist) 
  snow::clusterExport(cl, Localvarlist, envir = environment()) 
  
  
  
  Prop = snow::parSapply(cl,Start:End, function(t){
    VarExplianed.Day(Returns = Returns, StandardisedVolume = StandardisedVolume,
                     t = t, H = H, NrPC.cand = NrPC.cand)
  })
  
  snow::stopCluster(cl)
  
  Prop.R = ldply(Prop[1,])
  Prop.V = ldply(Prop[2,])
  
  return(list(
    Prop.R = Prop.R,
    Prop.V = Prop.V
  ))
}


Start = 500
End = ncol(Returns)



# 
# P = VarExplained.TS(Returns, Volume,
#                  Start = 500, End = ncol(Returns), H = 252,
#                  NrPC.cand=c(15,20,25),d = 20)

# 
# 
# write.csv(P, file = "./Results/Predictions/VarExplained.csv")


P = read.csv("./Results/Predictions/VarExplained.csv")

P = list(Prop.R = P[,2:4], Prop.V = P[5:7])

dates = colnames(Returns)[Start:End]
dates = as.Date(dates, format = "%Y%m%d")
dates = rep(dates, 3)



NrPC = c(rep(15,dim(as.matrix(P$Prop.R))[1]), rep(20,dim(as.matrix(P$Prop.R))[1]), rep(25,dim(as.matrix(P$Prop.R))[1]))

VarExplained.R = as.numeric(as.matrix(P$Prop.R))
VarExplained.V = as.numeric(as.matrix(P$Prop.V))         

data = data.frame(dates = dates,
                  NrPC = as.factor(NrPC),
                  VarExplained.R =VarExplained.R,
                  VarExplained.V = VarExplained.V)



plot.R = ggplot(data, aes(x = dates, y = VarExplained.R)) +
  geom_line(aes(colour = NrPC)) + ylim(0.25,0.8) +
  xlab("Date") + ylab("Variability (return)") + 
  theme(legend.position = "top")  +
  labs(colour = "Number Principal Components")

plot.V = ggplot(data, aes(x = dates, y = VarExplained.V)) +
  geom_line(aes(colour = NrPC)) +  ylim(0.25,0.8) +
  xlab("Date") + ylab("Variability (volume)") +
  theme(legend.position = "none") #legend 

plot = grid.arrange(plot.R, plot.V, nrow = 2)



###SAVE
ggsave(filename = "VariabilityExplained.png", 
       path = "C:\\Users\\Samuel Weinman\\OneDrive - Nexus365\\Documents\\MSc Statistical Science\\Dissertation\\Results\\Plots",
       plot = plot,
       width = 5, height = 6)

