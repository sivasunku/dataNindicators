
efficiencyRatio <- function(x,n=3){
  ########################################################
  # efficiency Ration = (net change)/ sum(abs(each change))
  # if x is xts, calculates eR for each with prev 3 bars into considreation
  ########################################################
  if (is.xts(x) && (ncol(x) >1) ){
    stop("efficiency Ratio can be calculated on univariate variables - cols to be single")
  }
  
  if (is.xts(x)){
    r <- x
    r[,1] <- NA
    series <- as.numeric(x[,1])
    dS <- diff(series)
    
    for(i in (n+1):nrow(x)){
      nC <- as.numeric(x[i]) - as.numeric(x[(i-n)])
      cS <- sum( abs(dS[(i-n):(i-1)]) )
      r[i,1] <- nC/cS
    }
    colnames(r) <- c("effRatio")
  } else {
    r <- x
    series <- x
    dS <- diff(series)
    for(i in (n+1):length(x)){
      nC <- as.numeric(x[i]) - as.numeric(x[(i-n)])
      cS <- sum( abs(dS[(i-n):(i-1)]) )
      r[i] <- nC/cS
    }
    
  }#end of is.xts
  r
}
elasticity <- function(m,n=13){
  ########################################################
  # elasticity  = (net change pct)/ log(Volume)
  # if m is xts, calculates elasticity for each of n prev bars
  #  return 3 cols - bullishE, bearishE, netE
  #  bullishE - elasticity of all bullish candles( where chgpct is +ve)
  #  similarly bearish , & net
  ########################################################
  if (!is.xts(m) ){
    stop("elasticity can be calculated on xts variables")
  }
  if (!is.OHLCV(m) ){
    stop("elasticity : m should be OHLCV")
  }
  
  res <- xts(order.by = index(m))
  res <- merge(res,bullE=NA,bearE=NA,netE=NA)
  l <- nrow(m)
  a <- m
  a$prevC <- lag(Cl(m))
  a$chgPct <- (a$Close/a$prevC) - 1
  
  for (i in n:l){
    t <- a[(i-n+1):i]
    bull <- t[(t$chgPct >= 0),]
    bear <- t[(t$chgPct < 0),]
    res[i,]$bullE <- sum(bull$chgPct/log(Vo(bull)))
    res[i,]$bearE <- sum(bear$chgPct/log(Vo(bear)))
    res[i,]$netE <- sum(t$chgPct/log(Vo(t)))
  }
  return(res)
}
findRangeFlag <- function(p,r, t = 0){
  ###############################################################################
  # For a given xts finds the pct of periods within the range xts
  # p - price , range - ranges, t - ticks that can be applied to see if in range
  # Returns : xts to say if it is within range or not
  ###############################################################################
  tickP <- 0
  if (t != 0 ) {
    tickP <- getTickedPrice(p,t)
  }
  if (!is.xts(p)){
    stop("In findRange p is not xts")
  }
  if (ncol(p) > 1){
    stop("In findRange p col is more than 1")
  }
  if (ncol(r) != 2){
    stop("In findRange r col is not 2")
  }
  if (nrow(p) != nrow(r)){
    stop("In findRange p,r have different rows")
  }
  
  colnames(p) <- c("Price")
  #Find which is ulimit/llimit in r
  if ( max(r[,1] > r[,2]) ){
    colnames(r) <- c("Ulimit","Llimit")
  }else{
    colnames(r) <- c("Llimit","Ulimit")
  }
  
  #Increase the ulimit by ticks & reduces the llimit by ticks
  r$Ulimit  <- r$Ulimit + tickP
  r$Llimit  <- r$Llimit - tickP
  
  temp <- merge.xts(r,p)
  temp$rangeFlag <- NA
  temp$rangeFlag <- ifelse( (temp$Price <= temp$Ulimit) & (temp$Price >= temp$Llimit),0,NA)
  temp$rangeFlag <- ifelse( is.na(temp$rangeFlag) & (temp$Price < temp$Llimit), -1, temp$rangeFlag)
  temp$rangeFlag <- ifelse( is.na(temp$rangeFlag) & (temp$Price > temp$Ulimit),  1, temp$rangeFlag)
  
  return (temp$rangeFlag)
}
findRangePct <- function(r,v = 0,n=13){
  ###############################################################################
  # For a given range r, finds the pct of v alue in n periods
  # Returns : xts to say if it is within range or not
  ###############################################################################
  if (!is.xts(r)){
    stop("In findRangePct r is not xts")
  }
  if (ncol(r) > 1){
    stop("In findRange p col is more than 1")
  }
  res <- xts(order.by = index(r))
  res <- merge.xts(res,count = NA)
  for ( i in n:nrow(r)){
    res[i,]$count <- sum(ifelse(temp[(i-n+1):i,]$rangeFlag == v,1,0)) / n
  }
  return(res * 100) 
}

bullBearStrength <- function(m){
  ########################################################
  # Returns bull bear strength xts object
  #   Bull Strength : prev Close - High, 0 which ever is max
  #   Bear Strength : Prev Close - Low, 0 which ever is min
  ########################################################
  if (!is.xts(m) ){
    stop("bullBearStrength can be calculated on xts variables")
  }
  if (!is.OHLC(m) ){
    stop("bullBearStrength : m should be OHLCV")
  }
  m$pC <- lag.xts(Cl(m))
  m <- na.trim(m)
  
  bull <- Hi(m) - m$pC
  bull <- ifelse(bull < 0,0,bull)
  
  bear <- Lo(m) - m$pC
  bear <- ifelse(bear > 0,0,abs(bear))
  
  res <- merge.xts(bull,bear)
  colnames(res) <- c("bullStrength","bearStrength")
  return(res)
}

