#' Plots the elder ray similar to the volume chart (histograms with green&red colours or defined by up.col & down.col)
#'
#' The function returns the elders Ray for a given OHLCV
#' @param m - The two columns, elderH & elderL.
#' @param log.scale - scaling parm taken as is from addVo
#' @return same as addVo
#' @author Siva Sunku
#' @keywords elders Ray
#' @export
#' 
`addElder` <- function(d,log.scale=FALSE) {
  lchob <- quantmod:::get.current.chob() 
  x <- as.matrix(lchob@xdata)

  if ( !( any(grepl("elderH",colnames(d)) == TRUE) && any(grepl("elderL",colnames(d)) == TRUE)  ) ){
    stop("In addElder d doesnot contain elderH/elderL")
  }
  elderS <- d[,c("elderH","elderL")]
  max.eld <- max(abs(elderS),na.rm=TRUE)
  
  eld.scale <- list(5, "5s")
  if ((max.eld) > 05)
    eld.scale <- list(1,"1s")
  if ((max.eld) > 10)
    eld.scale <- list(2,"2s")
  if ((max.eld) > 25)
    eld.scale <- list(5,"5s")
  if ((max.eld) > 50)
    eld.scale <- list(10,"10s")
  if ((max.eld) > 100)
    eld.scale <- list(20,"20s")
  if ((max.eld) > 250)
    eld.scale <- list(50,"50s")
  if (max.eld > 500)
    eld.scale <- list(100, "100s")
  
  bar.col    <- ifelse(elderS[,"elderL"] >0 , lchob@colors$up.up.col,lchob@colors$dn.dn.col)
  border.col <- ifelse(is.null(lchob@colors$border),bar.col,lchob@colors$border)

  chobTA <- new("chobTA")
  chobTA@new <- TRUE
  
  chobTA@TA.values <- (elderS/eld.scale[[1]])[lchob@xsubset]
  chobTA@name <- "chartElder"
  chobTA@params <- list(xrange=lchob@xrange,
                        colors=lchob@colors,
                        color.vol=lchob@color.vol,
                        multi.col=lchob@multi.col,
                        spacing=lchob@spacing,
                        width=lchob@width,
                        bp=lchob@bp,
                        eld.scale=eld.scale,
                        x.labels=lchob@x.labels,
                        log.scale=log.scale,
                        bar.col=bar.col,border.col=border.col,
                        time.scale=lchob@time.scale)
  chobTA@call <- match.call()
  chobTA@params$thin <- ifelse(lchob@type %in% c('bars','matchsticks'),TRUE,FALSE)
  
  if(is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA,chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new,1,0)
    #do.call('quantmod:::chartSeries.chob',list(lchob))
    do.call(quantmod:::chartSeries.chob,list(lchob))
    invisible(chobTA)
  } else {
    return(chobTA)
  } 
} # }}}


# chartElder {{{
`chartElder` <-
  function(x) {
    # if volume is to be plotted, do so here
    # scale volume - vol.divisor
    
    if(class(x) != "chobTA") stop("chartElder requires a suitable chobTA object")
    Elders <- x@TA.values
    spacing <- x@params$spacing
    width <- x@params$width
    
    x.range <- x@params$xrange
    x.range <- seq(x.range[1],x.range[2]*spacing)
    
    #    multi.col <- x@params$multi.col
    color.vol <- x@params$color.vol
    log.scale <- ifelse(x@params$log.scale,"y","")
    
    eld.scale <- x@params$eld.scale
    
    if(x@new) {
      plot.new()
      plot.window(xlim=c(1, x@params$xrange[2] * spacing),
                  ylim=c(min(Elders,na.rm=TRUE),max(Elders,na.rm=TRUE)),
                  log=log.scale)
      coords <- par('usr')
      rect(coords[1],coords[3],coords[2],coords[4],col=x@params$colors$area)
      abline(h=axTicks(2), col=x@params$colors$grid.col, lty='dotted')
    }
    
    x.pos <- 1 + spacing * (1:nrow(Elders) - 1)
    
    bar.col <- if(x@params$color.vol) {
      x@params$bar.col
    } else x@params$border.col
    
    border.col <- x@params$border.col
    #At anytime elderH will be bigger in value than elderL. 
    #If both are above zero, first draw elderH rectangle & then elderL.
    #If both are below zero, first draw elderL rectangle & then elderH
    #If elderH >0 & elderL <0 draw them independently (as if they are above zero)
    if(x@params$thin) {
      # plot thin volume bars if appropriate
      segments(x.pos,0,x.pos,Elders,col=bar.col)
    } else {
      #rect(x.pos-spacing/3,0,x.pos+spacing/3,Elders,col=bar.col,border=border.col)
      #lchob@colors$up.up.col,lchob@colors$dn.dn.col
      up.H <- ifelse(Elders$elderL>0,Elders$elderH,0)
      up.L <- ifelse(Elders$elderL>0,Elders$elderL,0)
      
      dn.H <- ifelse(Elders$elderL<0,Elders$elderH,0)
      dn.L <- ifelse(Elders$elderL<0,Elders$elderL,0) 
      rect(x.pos-spacing/3,0,x.pos+spacing/3,up.H,col=x@params$colors$up.col,border=border.col)
      rect(x.pos-spacing/3,0,x.pos+spacing/3,dn.L,col=x@params$colors$dn.col,border=border.col)
      
      rect(x.pos-spacing/3,0,x.pos+spacing/3,up.L,col=x@params$colors$dn.col,border=border.col)
      rect(x.pos-spacing/3,0,x.pos+spacing/3,dn.H,col=x@params$colors$up.col,border=border.col)

      #Siva -
    }
    legend.text <- list(list(
      legend=c(paste("Volume (",eld.scale[[2]],"):",sep=''),format(last(Elders)*eld.scale[[1]],big.mark=',')),
      text.col=c(x@params$colors$fg.col, last(bar.col))
    ))
    legend("topleft",
           legend=c(paste("Volume (",eld.scale[[2]],"):",sep=''),format(last(Elders)*eld.scale[[1]],big.mark=',')),
           text.col=c(x@params$colors$fg.col, last(bar.col)), bty="n", y.intersp=0.95)
    axis(2)
    box(col=x@params$colors$fg.col)
    invisible(vector('list',2))
  } # }}}
