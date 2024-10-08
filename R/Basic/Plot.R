#CREATES TIME SERIES PLOT OVER CUMSUM PNL, SEPARATED BY QUANTILE AND WHETHER STANDARDISED OR NOT.
#INPUTS:
# scores: LIST OF scores (OUTPUT FROM "ANALYSIS")
# labels: NAME OF methodS USE (E.G. C("CROSS-SECTIONAL", "CROSS-TEMPORAL"))
# TYPE: ONE OF "CS", "CT" OR BASE (ONE CS, ONE CT)
# base_models: WHICH OF THE methodS IS THE "base_line" MODEL, I.E. THE BENCHMARK. HAS TO BE A NUMBER, WHERE E.G. base_models = 2 CORRESPONDS TO THE SECOND SCORE IN "scores" BEING THE base_line.
createCumSumPnLPlot = function(scores, labels, title, type, base_models) {

  #NR OF methodS
  r <- length(scores)
  
  #NR OF DAYS
  t <- length(scores[[1]]$regular$pnl_cum_sum[,1])
  
  
  #FIND CORRESPONDING DATES
  col_nr <- as.integer(names(scores[[1]]$standard$pnl_cum_sum[,1]))
  dates <- colnames(returns)[col_nr]
  dates <- as.Date(dates, format = "%Y%m%d")
  dates <- rep(dates, 8*r)
  
  
  #CUMSUM PNL AS VECTOR
  cum_pnl <- sapply(1:r, function(i){
    c(scores[[i]]$regular$pnl_cum_sum, scores[[i]]$regular$pnl_cum_sum)
  })
  cum_pnl <- as.numeric(unlist(cum_pnl))
  
  
  #IF TYPE IS CS OR CT, NEED TO PUT IN WHICH ARE base_line MODELS TO CHANGE THE LABELLING IN methodS ACCORDINGLY.
  #IF TYPE = "BASE", CAN JUST KEEP THE MODEL NAMES
  if (type != "BASE") {
    #base_line AS VECTOR (USED ONLY TO GET method ORDER CORRECT)
    base_line <- rep(0, 8*r*t)
    base_line[(base_models-1)*8*t + 1:(8*t)] = 1
    
    
    #method AS A VECTOR
    #TO GET THE MODELS IN RIGHT ORDER, WE CHANGE THE NAME OF THE BASEMODEL TO "AA" TO ENSURE THAT THIS MODEL COMES OUT FIRST. THE LABEL OF THIS MODEL WILL BE DIFFERENT.
    method <- sapply(1:r, function(i){
      rep(labels[i],8*t)
    })
    method <- as.character(method)
    method[base_line == 1] <- "AA"
  } else{
  #method AS VECTOR
  method <- sapply(1:r, function(i){
    rep(labels[i],8*t)
  })
  method <- as.factor(method)
  }
  
  
  
  #STANDARDISED AS VECTOR
  standardised <- rep(c(rep("Not Standardised", 4*t), rep("Standardised", 4*t)),r)
  
  
  #QUANTILE AS VECTOR
  quantile <- rep(c(rep(1/4,T), rep(2/4,T), rep(3/4,T), rep(4/4,T)),2*r)
  
  
  #PUT IN DATA FRAME
  data <- data.frame(dates = dates,
                    cum_pnl = cum_pnl,
                    method = method,
                    standardised = standardised,
                    quantile = quantile)
  
  
  
  #IF TYPE IS NOT "BASE", WE RE-ORDER THE ROWS OF THE DATA FRAME TO MAKE SURE THAT THE COLOURS MATCH SHARPE PPT PLOT.
  #IE WHEN USING SIDE-BY-SIDE IN BARPLOT, IT WILL SORT THE COLOURS IN ALPHABETICAL ORDER. NEED TO MAKE SURE IT DOES THE SAME HERE!
  
  
  if (type != "BASE") {
    data = data[order(method),]
    labels = labels[order(unique(method))]
  }
  
  
  
  
  
  
  
  #CHOOSE PALETTE ACCORDINGLY
    if (type == "CS") {
      p = brewer.pal(11, "PiYG")[c(1:3,9:11)]
    }
    
    if (type == "CT") {
      p = brewer.pal(11, "BrBG")[c(1:3,9:11)]
    }
    
    if (type == "BASE") {
      p = c(brewer.pal(11, "PiYG")[1], brewer.pal(11, "BrBG")[1])
    }
    
  
  
  
  #CREATE PLOT
  plot = ggplot(data, aes(x = dates, y = CumPnL)) + #x,y axis 
    geom_line(aes(colour = method)) + #method as colour
    scale_colour_manual(values = p, labels = labels) + #palette
    scale_x_date(date_breaks = "4 years", date_labels = "%Y") + xlab("")+ #format x-axis
    ylab("CumSum PnL") + scale_y_continuous(breaks = c(0,500,1000), limits = c(0,max(data$CumPnL*1.1))) + #format y-axis 
    theme_minimal() #theme
  
  
  #SPLIT ON SUBPLOTS
  plot = plot + facet_grid(rows = vars(Quantile), cols = vars(Standardised)) + #sub-plots
    theme(legend.position = "top",  legend.title = element_blank()) #format legend
  
  
  #ADD TITLE IF IN THE INPUTR
  if (!missing(title)) {
    plot = plot + ggtitle(title)
  }
  
  #RETURN
  return(plot)
}






#CREATES BARPLOT OVER PPT AND SHARPE, SEPARATED BY QUANTILE AND WHETHER STANDARDISED OR NOT.
#INPUTS:
# scores: LIST OF scores (OUTPUT FROM "ANALYSIS")
# labels: NAME OF methodS USE (E.G. C("CROSS-SECTIONAL", "CROSS-TEMPORAL"))
# base_models: INDICATES WHICH OF THE scores BELONG TO A BASE MODEL. E.G. IF base_models = C(1,2), THE FIRST TWO ARE BASE MODELS AND THE OTHERS NOT
# TYPE: ONE OF "CS", "CT" OR "BASE" (ONE CS, ONE CT)
#IF TYPE == "BASE", MUST GIVE CS BEFORE CT!
createSharpePPTPlot = function(scores, labels, title, base_models, type, legendTitle) {

  #NR OF methodS
  r = length(scores) 
  
  
  #SHARPE RATIO AS VECTOR
  share = sapply(1:r, function(i){
    c(scores[[i]]$regular$share, scores[[i]]$regular$share)
  })
  share = as.numeric(share)
  
  
  #PPT AS VECTOR
  PPT = sapply(1:r, function(i){
    c(scores[[i]]$regular$PPT, scores[[i]]$regular$PPT)
  })
  PPT = as.numeric(PPT)
  

  
  
  #base_line
  base_line = sapply(1:r, function(i) {
    rep(i %in% base_models ,8)
  })
  base_line = as.numeric(base_line)
  
  
  #method AND LABEL AS VECTORS. IN THE CASE WHEN TYPE = "BASE", THERE IS NO RE-ORDERING SO labels = method
  method = sapply(1:r, function(i) {
    rep(labels[i],8)
  })
  method = as.character(method)

  
  #IF TYPE = "CS" OR "CW", THE base_line method IS GIVEN NAME "AA" TO ENSURE THAT IT SHOWS UP IN THE LEFTERMOST OF THE GRAPH. THE NAME ON THE LABEL IS STILL THE SAME AS WHAT IS GIVEN IN THE INPUT "LAVELS".
  #ALSO NEED TO REORDER labels AS THE BARS WILL BE SORTED IN ALPHABETICAL ORDER
  if (type != "BASE") {
    method[base_line == 1] = "AA"
    labels = c(labels[base_models],sort(labels[-base_models]))
  }

  #FINALLY,CHANGE method TO FACTOR
  method = as.factor(method)
  
  
  #QUANTILE AS VECTOR
  Quantile = rep((1:4)/4, r*2)
  
  #STANDARDISED AS VECTOR
  Standardised = rep(c(rep("Not Standardised",4),rep("Standardised",4)),r)
  
  
  #CREATE DATA FRAME
  data = data.frame(share = share,
                    PPT = PPT,
                    method = method,
                    base_line = base_line,
                    Quantile= Quantile,
                    Standardised = Standardised)
  
  
  #CHOOSE PALETTE DEPENDING ON TYPE
  if (type == "CS") {
    p = brewer.pal(11, "PiYG")[c(1:3,9:11)]
    if (length(scores) == 7) {
      p = brewer.pal(11, "PiYG")[c(1:4,9:11)]
    }
    
    if (length(scores) == 8) {
      p = brewer.pal(11, "PiYG")[c(1:5,9:11)]
    }
  }
  
  if (type == "CT") {
    p = brewer.pal(11, "BrBG")[c(1:3,9:11)]
    if (length(scores) == 8) {
      p = brewer.pal(11, "BrBG")[c(1:5,9:11)]
    }
  }
  
  
  if (type == "BASE") {
    p = c(brewer.pal(11, "PiYG")[1], brewer.pal(11, "BrBG")[1])
  }

  
  #CREATE SHARPE RATIO PLOT
  bar1 = ggplot(data = data, aes(x = Quantile,y = share, fill = method,width = 0.2,
                               color = base_line)) + 
    geom_bar(stat = "identity", position = "dodge")+ #create barplot, method as colour
    scale_fill_manual(values =p, labels = labels) + #palette, labels
    scale_x_continuous(breaks = (1:4)/4, labels = c("25%", "50%", "75%", "100%")) +
    #x-axis 
    ylab("Sharpe Ratio") +scale_y_continuous(limits = c(0,max(data$share+0.5))) + #y-axis label
    facet_grid(cols = vars(Standardised)) + #subplots
    theme_minimal() + #theme
    guides(col = FALSE) +
    theme(legend.position = "top") 
  
  if (missing(legendTitle)) {
    bar1 = bar1 + theme(legend.title =  element_blank()) #legend
  } else {
    bar1 = bar1 + guides(fill = guide_legend(title = legendTitle))
  }
  
  
  #ADD TITLE ABOVE SHARPE RATIO IF IN THE INPUT
  if (!missing(Title)) {
    bar1 = bar1 + ggtitle(Title)
  }
  
  

  
  
  
  
  ###CREATE BARPLOT OVER PPT
  bar2 = ggplot(data = data, aes(x = Quantile,y = PPT, fill = method,color = base_line, width = 0.2)) +
    geom_bar(stat = "identity", position = "dodge")+ #create barplot, method as colour
    scale_fill_manual(values =p) + #palette
    scale_x_continuous(breaks = (1:4)/4, labels = c("25%", "50%", "75%", "100%"))+ 
    #format x-axis
    scale_y_continuous(breaks = (0:4)*2e-4, labels = seq(0,8,by = 2),limits = c(0,max(data$PPT+0.0002))) + #y-axis
    ylab("PPT (bps)") + #y-axis label
    facet_grid(cols = vars(Standardised)) + #subplots
    theme_minimal() + #theme
    theme(legend.position = "none") #legend 
    
  
  #MERGE
  barplot = grid.arrange(bar1, bar2, nrow = 2)
  
  #RETURN
  return(barplot)
}

