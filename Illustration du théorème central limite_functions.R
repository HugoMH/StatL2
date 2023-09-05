PlotPop = function(){
  par(mfrow = c(3,1), mar=c(4,2,0.5,2))
  hist(Pop,breaks = 50,main='',xlab = 'X')
  legend(x = 'topleft',legend = paste('σ² = ',round((σ^2),7)), text.col = 'red'
    )
  
  plot(1:N ~ Pop, xlab='           X                                                                                                µ^',ylab='',xlim = c(0,2), yaxt = 'n', xaxt = 'n')
  axis(side = 1 ,at = (0:4)/2 ,labels = c('0.0','0.5','1.0  0.0','0.5','1.0')    )
}



sampleNech = function(){
  samp_ = 1:N
  M_ = NULL
  # i = 0
  # for(speed in 1:500){
    for(ech in 1:Nech){ # i=i+1
      samp = sample(samp_,replace = F,size = n)
      if(ech < 5){
        for(rep in 1:3){points(samp ~ Pop[samp],col=ech+1)}
      Sys.sleep(2)}
      M_[length(M_)+1] = mean(Pop[samp])
      i = runif(1,1,N)
      points(M_[length(M_)] + 1, i , col=ech+1)
      if(ech < 5){Sys.sleep(5)
        for(rep in 1:3){ points(samp ~ Pop[samp]) }
        }
    }
  return(M_)
}


PlotHistogramSamples = function(){
  H = hist(M_, xlab = 'Population of µ^',breaks = round(Nech/50),main = '')
  H <<- H 
  
  abline(v=µ,col='blue',lwd = 1.5)
  legend(x = 'topleft',legend = c(paste('n = ',n)
                                 ,paste('var(µ^) = ',round(var(M_),6))
                                 ,paste('σ²/n = ',round((σ^2)/n,6))
                                 )
    , text.col = c('red','red','red')
    ,col=c('red','red','red'))
  
  legend(x = 'topright',legend = c('µ'
                                 ,'N(µ, σ²/n)'
                                 ,'µ ± 1.96 * σ²/n'
                                 )
    , text.col = c('blue','green','red')
    ,lty=c(1,1,2),col=c('blue','green','red'))
  
  x = seq(min(M_),max(M_),length = 500)
  x <<- x
  D. = dnorm(x,mean = µ,sd = σ/sqrt(n))
  D. <<- D.
  
  H$mid = (H$breaks[-1]+H$breaks[-length(H$breaks)])/2
  D.mid = D.[sapply(H$mid,function(h)which.min(abs(h-x)))]
  
  fac = D.mid / H$counts
  fac[is.infinite(fac)] = NA
  fac = median(fac,na.rm = T)
  fac <<- fac
  
  lines(x,D./fac,col=3,lwd = 1.5)
  
  σ_1.96 <<- c(µ - 1.96*σ/sqrt(n)  ,  µ + 1.96*σ/sqrt(n))
  
  segments(x0 = σ_1.96,x1 = σ_1.96, y0 = 0
           , y1 = dnorm(σ_1.96,mean = µ,sd = σ/sqrt(n))/fac
           , col = 2,lty=2,lwd = 1.5)
  text('95%',x=0.65,y=45,cex = 2,col = 3)
  text('2.5%',x=0.52,y=25,cex = 1,col = 2)
  text('2.5%',x=0.83,y=25,cex = 1,col = 2)
}




CheckSymmetry = function(){
  par(mfrow = c(3,1), mar=c(4,2,0.5,2))
  PlotHistogramSamples()
  
  Xlim = range(c(    -(M_[M_<mean(M_)]-mean(M_))+mean(M_)    ,M_[ M_>mean(M_)]  ))
  hist(-(M_[M_<mean(M_)]-mean(M_))+mean(M_), xlab = 'Histogram of -µ^ < mean(M_)',breaks = round(Nech/50),main = '',xlim = Xlim)
  hist( M_[ M_>mean(M_)], xlab = 'Histogram of  µ^ > mean(M_)',breaks = round(Nech/50),main = '',xlim = Xlim)
}



ComparePop_SampleConf = function(step){
  if(step == 0){
    par(mfrow = c(2,1))
    PlotHistogramSamples()
    
  }else if(step == 1){
    plot(H$mids, -H$counts, type = 'n', xaxt = 'n', yaxt = 'n',xlab = '')
    mtext('Échantillons et intervalle de confiance',side = 1,line = 1.5)
    abline(v = µ, col = 'blue',lwd = 3,lty=2)
    
  }else if(step == 2){
    abline(v = σ_1.96[1], col = 5,lwd = 1.5)
    
  }else if(step == 3){
    lines(x-µ+σ_1.96[1] , -D./fac,col=5,lwd = 1.5)
    segments(x0 = σ_1.96-µ+σ_1.96[1], x1 = σ_1.96-µ+σ_1.96[1], y0 = 0         , y1 = -dnorm(σ_1.96,mean = µ,sd = σ/sqrt(n))/fac         , col = 5,lwd = 1.5)

  }else if(step == 4){
    abline(v = σ_1.96[2], col = 6,lwd = 1.5)
    
  }else if(step == 5){
    lines(x-µ+σ_1.96[2] , -D./fac,col=6,lwd = 1.5)
    segments(x0 = σ_1.96-µ+σ_1.96[2],x1 = σ_1.96-µ+σ_1.96[2], y0 = 0         , y1 = -dnorm(σ_1.96,mean = µ,sd = σ/sqrt(n))/fac         , col = 6,lwd = 1.5)

  }else if(step == 6){
    text('σ -> σ^', x=0.63, y = -100)
    σ1 = σ + -0.11*σ
    σ1_1.96 = c(µ - 1.96*σ1/sqrt(n)  ,  µ + 1.96*σ1/sqrt(n))
    D1 = dnorm(x,mean = µ,sd = σ1/sqrt(n))
    lines(x-µ+σ_1.96[1] , -D1/fac,col=5,lwd = 1.5,lty = 2)
    segments(x0 = σ1_1.96-µ+σ_1.96[1],x1 = σ1_1.96-µ+σ_1.96[1], y0 = 0         , y1 = -dnorm(σ1_1.96,mean = µ,sd = σ1/sqrt(n))/fac         , col = 5,lty=2,lwd = 1.5)
    
  }else if(step == 7){
    σ1 = σ + 0.08*σ
    σ1_1.96 = c(µ - 1.96*σ1/sqrt(n)  ,  µ + 1.96*σ1/sqrt(n))
    D1 = dnorm(x,mean = µ,sd = σ1/sqrt(n))
    lines(x-µ+σ_1.96[2] , -D1/fac,col=6,lwd = 1.5,lty = 2)
    segments(x0 = σ1_1.96-µ+σ_1.96[2],x1 = σ1_1.96-µ+σ_1.96[2], y0 = 0         , y1 = -dnorm(σ1_1.96,mean = µ,sd = σ1/sqrt(n))/fac         , col = 6,lty=2,lwd = 1.5)

  }
  
}




















