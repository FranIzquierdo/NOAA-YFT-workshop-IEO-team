plotspict.data.adp=function (inpin, MSY = NULL, one.index = NULL, qlegend = TRUE, 
          stamp = get.version()) {
  isrep <- ifelse(inherits(inpin, "spictcls") && "opt" %in% 
                    names(inpin), 1, 0)
  if (isrep) {
    inpin <- inpin$inp
  }
  inp <- check.inp(inpin)
  nseries <- inp$nseries + as.numeric(inp$logmcovflag)
  if ("true" %in% names(inp)) {
    nseries <- nseries + 2
    if (inp$timevaryinggrowth) {
      nseries <- nseries + 1
    }
  }


  xlim <- range(inp$timeC, unlist(inp$timeI), inp$timeE)
  if ("true" %in% names(inp)) {
    if (inp$timevaryinggrowth) {
      plot(inp$time, inp$true$mre, typ = "l", xlim = xlim, 
           xlab = "Time", ylab = "m", lwd = 1.5, 
           col = true.col(), main = "True MSY")
      box(lwd = 1.5)
    }
    plot(inp$time, inp$true$F, typ = "l", col = true.col(), 
         xlim = xlim, xlab = "Time", ylab = expression(F[t]), 
         lwd = 1.5, main = "True F")
    box(lwd = 1.5)
    ylab <- add.catchunit(expression(B[t]), inp$catchunit)
    plot(inp$time, inp$true$B, typ = "l", xlim = xlim, 
         xlab = "Time", ylab = ylab, lwd = 1.5, col = true.col(), 
         main = "True biomass")
    box(lwd = 1.5)
  }
  main <- paste0("Nobs C: ", inp$nobsC)
  ylab <- "Catch"
  ylab <- add.catchunit(ylab, inp$catchunit)
  plot(inp$timeC, inp$obsC, typ = "l", ylab = ylab, xlab = "Time", 
       main = main, xlim = xlim)
  grid()
  plot.col(inp$timeC, inp$obsC, do.line = FALSE, cex = 0.6, 
           add = TRUE, add.legend = qlegend)
  if (!is.null(MSY)) {
    abline(h = MSY, lty = 2)
  }
  box(lwd = 1.5)
  if (inp$nindex > 0) {
    i <- 1
    main <- paste0("Nobs I: ", inp$nobsI[i])
    plot(inp$timeI[[i]], inp$obsI[[i]], typ = "l", 
         ylab = paste("Index", i), xlab = "Time", 
         main = main, xlim = xlim)
    grid()
    plot.col(inp$timeI[[i]], inp$obsI[[i]], pch = i, do.line = FALSE, 
             cex = 0.6, add = TRUE, add.legend = FALSE)
    if (inp$nindex > 1 & is.null(one.index)) {
      for (i in 2:inp$nindex) {
        main <- paste0("Nobs I: ", inp$nobsI[i])
        plot(inp$timeI[[i]], inp$obsI[[i]], typ = "l", 
             ylab = paste("Index", i), xlab = "Time", 
             main = main, xlim = xlim)
        grid()
        plot.col(inp$timeI[[i]], inp$obsI[[i]], pch = i, 
                 do.line = FALSE, cex = 0.6, add = TRUE, add.legend = FALSE)
      }
    }
    box(lwd = 1.5)
  }
  if (inp$nobsE) {
    main <- paste0("Nobs E: ", inp$nobsE)
    ylab <- "Effort"
    plot(inp$timeE, inp$obsE, typ = "l", ylab = ylab, 
         xlab = "Time", main = main, xlim = xlim)
    grid()
    plot.col(inp$timeE, inp$obsE, do.line = FALSE, cex = 0.6, 
             add = TRUE, add.legend = FALSE)
    box(lwd = 1.5)
  }
  if (inp$logmcovflag) {
    main <- paste0("Nobs logmcovariate: ", length(inp$logmcovariate))
    ylab <- "logm covariate"
    plot(inp$time, inp$logmcovariatein, typ = "l", 
         ylab = ylab, xlab = "Time", main = main, xlim = xlim, 
         col = "blue", lwd = 1.5)
    grid()
    plot.col(inp$logmcovariatetime, inp$logmcovariate, do.line = FALSE, 
             cex = 0.6, add = TRUE, add.legend = FALSE)
    box(lwd = 1.5)
  }
  txt.stamp(stamp, do.flag = TRUE)
}