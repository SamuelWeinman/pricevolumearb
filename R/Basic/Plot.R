#CREATES TIME SERIES PLOT OVER CUMSUM PNL, SEPARATED BY QUANTILE AND WHETHER STANDARDISED OR NOT.
#INPUTS:
# SCORES: LIST OF SCORES (OUTPUT FROM "ANALYSIS")
# LABELS: NAME OF METHODS USE (E.G. C("CROSS-SECTIONAL", "CROSS-TEMPORAL"))
# TYPE: ONE OF "CS", "CT" OR BASE (ONE CS, ONE CT)
# BASEMODELS: WHICH OF THE METHODS IS THE "BASELINE" MODEL, I.E. THE BENCHMARK. HAS TO BE A NUMBER, WHERE E.G. BaseModels=2 CORRESPONDS TO THE SECOND SCORE IN "SCORES" BEING THE BASELINE.
CreatePlot.CumSumPnL = function(Scores, Labels, Title, Type, BaseModels) {

  #NR OF METHODS
  r=length(Scores)
  
  #NR OF DAYS
  T = length(Scores[[1]]$Regular$PnL.Cumsum[,1])
  
  
  #FIND CORRESPONDING DATES
  ColNr = as.integer(names(Scores[[1]]$Standard$PnL.Cumsum[,1]))
  dates = colnames(Returns)[ColNr]
  dates = as.Date(dates, format = "%Y%m%d")
  dates = rep(dates, 8*r)
  
  
  #CUMSUM PNL AS VECTOR
  CumPnL = sapply(1:r, function(i){
    c(Scores[[i]]$Regular$PnL.Cumsum, Scores[[i]]$Regular$PnL.Cumsum)
  })
  CumPnL = as.numeric(unlist(CumPnL))
  
  
  #IF TYPE IS CS OR CT, NEED TO PUT IN WHICH ARE BASELINE MODELS TO CHANGE THE LABELLING IN METHODS ACCORDINGLY.
  #IF TYPE = "BASE", CAN JUST KEEP THE MODEL NAMES
  if (Type != "BASE") {
    #BASELINE AS VECTOR (USED ONLY TO GET METHOD ORDER CORRECT)
    BaseLine = rep(0, 8*r*T)
    BaseLine[(BaseModels-1)*8*T + 1:(8*T)] = 1
    
    
    #METHOD AS A VECTOR
    #TO GET THE MODELS IN RIGHT ORDER, WE CHANGE THE NAME OF THE BASEMODEL TO "AA" TO ENSURE THAT THIS MODEL COMES OUT FIRST. THE LABEL OF THIS MODEL WILL BE DIFFERENT.
    Method = sapply(1:r, function(i){
      rep(Labels[i],8*T)
    })
    Method = as.character(Method)
    Method[BaseLine==1] = "AA"
  } else{
  #METHOD AS VECTOR
  Method = sapply(1:r, function(i){
    rep(Labels[i],8*T)
  })
  Method = as.factor(Method)
  }
  
  
  
  #STANDARDISED AS VECTOR
  Standardised = rep(c(rep("Not Standardised", 4*T), rep("Standardised", 4*T)),r)
  
  
  #QUANTILE AS VECTOR
  Quantile = rep(c(rep(1/4,T), rep(2/4,T), rep(3/4,T), rep(4/4,T)),2*r)
  
  
  #PUT IN DATA FRAME
  data = data.frame(dates=dates,
                    CumPnL=CumPnL,
                    Method=Method,
                    Standardised=Standardised,
                    Quantile=Quantile)
  
  
  
  #IF TYPE IS NOT "BASE", WE RE-ORDER THE ROWS OF THE DATA FRAME TO MAKE SURE THAT THE COLOURS MATCH SHARPE PPT PLOT.
  #IE WHEN USING SIDE-BY-SIDE IN BARPLOT, IT WILL SORT THE COLOURS IN ALPHABETICAL ORDER. NEED TO MAKE SURE IT DOES THE SAME HERE!
  
  
  if (Type != "BASE") {
  data = data[order(Method),]
  Labels = Labels[order(unique(Method))]
  }
  
  
  
  
  
  
  
  #CHOOSE PALETTE ACCORDINGLY
    if (Type=="CS") {
      p=brewer.pal(11, "PiYG")[c(1:3,9:11)]
    }
    
    if (Type=="CT") {
      p = brewer.pal(11, "BrBG")[c(1:3,9:11)]
    }
    
    if (Type == "BASE") {
      p = c(brewer.pal(11, "PiYG")[1], brewer.pal(11, "BrBG")[1])
    }
    
  
  
  
  #CREATE PLOT
  plot = ggplot(data, aes(x=dates, y=CumPnL)) + #x,y axis 
    geom_line(aes(colour=Method)) + #method as colour
    scale_colour_manual(values = p, labels=Labels) + #palette
    scale_x_date(date_breaks = "4 years", date_labels="%Y") + xlab("")+ #format x-axis
    ylab("CumSum PnL") + scale_y_continuous(breaks = c(0,500,1000), limits = c(0,max(data$CumPnL*1.1))) + #format y-axis 
    theme_minimal() #theme
  
  
  #SPLIT ON SUBPLOTS
  plot = plot + facet_grid(rows = vars(Quantile), cols = vars(Standardised)) + #sub-plots
    theme(legend.position="top",  legend.title = element_blank()) #format legend
  
  
  #ADD TITLE IF IN THE INPUTR
  if (!missing(Title)) {
    plot = plot + ggtitle(Title)
  }
  
  #RETURN
  return(plot)
}






#CREATES BARPLOT OVER PPT AND SHARPE, SEPARATED BY QUANTILE AND WHETHER STANDARDISED OR NOT.
#INPUTS:
# SCORES: LIST OF SCORES (OUTPUT FROM "ANALYSIS")
# LABELS: NAME OF METHODS USE (E.G. C("CROSS-SECTIONAL", "CROSS-TEMPORAL"))
# BASEMODELS: INDICATES WHICH OF THE SCORES BELONG TO A BASE MODEL. E.G. IF BASEMODELS = C(1,2), THE FIRST TWO ARE BASE MODELS AND THE OTHERS NOT
# TYPE: ONE OF "CS", "CT" OR "BASE" (ONE CS, ONE CT)
#IF TYPE=="BASE", MUST GIVE CS BEFORE CT!
CreatePlot.SharpePPT = function(Scores,Labels,Title, BaseModels, Type, LegendTitle) {

  #NR OF METHODS
  r = length(Scores) 
  
  
  #SHARPE RATIO AS VECTOR
  Sharpe = sapply(1:r, function(i){
    c(Scores[[i]]$Regular$Sharpe, Scores[[i]]$Regular$Sharpe)
  })
  Sharpe = as.numeric(Sharpe)
  
  
  #PPT AS VECTOR
  PPT = sapply(1:r, function(i){
    c(Scores[[i]]$Regular$PPT, Scores[[i]]$Regular$PPT)
  })
  PPT = as.numeric(PPT)
  

  
  
  #BASELINE
  BaseLine = sapply(1:r, function(i) {
    rep(i %in% BaseModels ,8)
  })
  BaseLine = as.numeric(BaseLine)
  
  
  #METHOD AND LABEL AS VECTORS. IN THE CASE WHEN TYPE="BASE", THERE IS NO RE-ORDERING SO LABELS = METHOD
  Method = sapply(1:r, function(i) {
    rep(Labels[i],8)
  })
  Method = as.character(Method)

  
  #IF TYPE="CS" OR "CW", THE BASELINE METHOD IS GIVEN NAME "AA" TO ENSURE THAT IT SHOWS UP IN THE LEFTERMOST OF THE GRAPH. THE NAME ON THE LABEL IS STILL THE SAME AS WHAT IS GIVEN IN THE INPUT "LAVELS".
  #ALSO NEED TO REORDER LABELS AS THE BARS WILL BE SORTED IN ALPHABETICAL ORDER
  if (Type != "BASE") {
    Method[BaseLine==1] = "AA"
    Labels = c(Labels[BaseModels],sort(Labels[-BaseModels]))
  }

  #FINALLY,CHANGE METHOD TO FACTOR
  Method = as.factor(Method)
  
  
  #QUANTILE AS VECTOR
  Quantile = rep((1:4)/4, r*2)
  
  #STANDARDISED AS VECTOR
  Standardised = rep(c(rep("Not Standardised",4),rep("Standardised",4)),r)
  
  
  #CREATE DATA FRAME
  data = data.frame(Sharpe=Sharpe,
                    PPT=PPT,
                    Method=Method,
                    BaseLine = BaseLine,
                    Quantile= Quantile,
                    Standardised=Standardised)
  
  
  #CHOOSE PALETTE DEPENDING ON TYPE
  if (Type=="CS") {
    p=brewer.pal(11, "PiYG")[c(1:3,9:11)]
    if (length(Scores) == 7) {
      p=brewer.pal(11, "PiYG")[c(1:4,9:11)]
    }
    
    if (length(Scores) == 8) {
      p=brewer.pal(11, "PiYG")[c(1:5,9:11)]
    }
  }
  
  if (Type=="CT") {
    p = brewer.pal(11, "BrBG")[c(1:3,9:11)]
    if (length(Scores) == 8) {
      p=brewer.pal(11, "BrBG")[c(1:5,9:11)]
    }
  }
  
  
  if (Type == "BASE") {
    p = c(brewer.pal(11, "PiYG")[1], brewer.pal(11, "BrBG")[1])
  }

  
  #CREATE SHARPE RATIO PLOT
  bar1 = ggplot(data=data, aes(x=Quantile,y=Sharpe, fill=Method,width=0.2,
                               color=BaseLine)) + 
    geom_bar(stat="identity", position="dodge")+ #create barplot, method as colour
    scale_fill_manual(values =p, labels=Labels) + #palette, labels
    scale_x_continuous(breaks=(1:4)/4, labels = c("25%", "50%", "75%", "100%")) +
    #x-axis 
    ylab("Sharpe Ratio") +scale_y_continuous(limits = c(0,max(data$Sharpe+0.5))) + #y-axis label
    facet_grid(cols = vars(Standardised)) + #subplots
    theme_minimal() + #theme
    guides(col=FALSE) +
    theme(legend.position = "top") 
  
  if (missing(LegendTitle)) {
    bar1 = bar1 + theme(legend.title =  element_blank()) #legend
  } else {
    bar1 = bar1 + guides(fill=guide_legend(title=LegendTitle))
  }
  
  
  #ADD TITLE ABOVE SHARPE RATIO IF IN THE INPUT
  if (!missing(Title)) {
    bar1 = bar1 + ggtitle(Title)
  }
  
  

  
  
  
  
  ###CREATE BARPLOT OVER PPT
  bar2 = ggplot(data=data, aes(x=Quantile,y=PPT, fill=Method,color=BaseLine, width=0.2)) +
    geom_bar(stat="identity", position="dodge")+ #create barplot, method as colour
    scale_fill_manual(values =p) + #palette
    scale_x_continuous(breaks=(1:4)/4, labels = c("25%", "50%", "75%", "100%"))+ 
    #format x-axis
    scale_y_continuous(breaks = (0:4)*2e-4, labels = seq(0,8,by=2),limits = c(0,max(data$PPT+0.0002))) + #y-axis
    ylab("PPT (bps)") + #y-axis label
    facet_grid(cols = vars(Standardised)) + #subplots
    theme_minimal() + #theme
    theme(legend.position = "none") #legend 
    
  
  #MERGE
  barplot = grid.arrange(bar1, bar2, nrow=2)
  
  #RETURN
  return(barplot)
}

