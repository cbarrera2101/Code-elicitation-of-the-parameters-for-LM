readinteger <- function()
{
  n <- readline(prompt="Enter an integer: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}

print(readinteger())


# CODE FOR ELICITING THE PARAMETERS OF A LINEAR MODEL
# Only two points of the explanatory variable $x_0$ and $x_1$

lectura <- function()
{
  x <- readline(prompt="Enter the value: ")
  return(as.numeric(x))
}


elicita.modelo.lineal<-function(){ print('Choose a point from the
explanatory variable and hit enter ..') 
  x0<-lectura() 
  print(x0)
  
  print('Pick a point on the dependent variable and press enter ..') 
  y0<-lectura() 
  print(y0)
  
  cat('From a sample of 100 subjects with a value of x=',x0,' how 
many do you think\n they will be below the value ',y0,'?\n')
  
  n0<-lectura()
  print(n0)
  
  cat('What would be the minimum number of subjects out of the 100 
that you would accept as valid less than ',y0,'?\n')
  
  n0L<-lectura()
  print(n0L)
  
  n.eq1<-(1.96/(n0-n0L))^2*n0*(100-n0)
  
  cat('What would be the maximum number of subjects out of 100 that 
would accept as valid greater than ',y0,'?\n')
  
  n0U<-lectura()
  print(n0U)
  
  n.eq2<-(1.96/(n0U-n0))^2*n0*(100-n0)
  
  print('Pick another point on the dependent variable and press
enter..')
  y1<-lectura() 
  print(y1)
  
  cat('From a sample of 100 subjects with a value of x=',x0,' 
how many do you think\n they will be below the value ',y1,'?\n')
  
  n1<-lectura()
  print(n1)
  
  cat('What would be the minimum number of subjects out of the 100 
that you would accept as valid less than ',y1,'?\n')
  
  n1L<-lectura()
  print(n1L)
  
  n.eq3<-(1.96/(n1-n1L))^2*n1*(100-n1)
  
  cat('What would be the maximum number of subjects of the 100 that 
would accept as valid greater than ',y1,'?\n')
  
  n1U<-lectura()
  print(n1U)
  
  n.eq4<-(1.96/(n1U-n1))^2*n1*(100-n1)
  
  # Calculations
  s1<-(y0-y1)/(qnorm(n0/100)-qnorm(n1/100))
  m1<-y0-s1*qnorm(n0/100)
  
  print(c(m1,s1))
  
  
  n.eq2<-(1.96/(n1U-n1))*n1*(100-n1)
  
  print('Pick ANOTHER point of the explanatory variable and press
enter ..') 
  x0a<-lectura() 
  print(x0a)
  
  print('Pick a point on the dependent variable and press
enter ..') 
  y0a<-lectura() 
  print(y0a)
  
  cat('From a sample of 100 subjects with a value of x=',x0a,' how
many do you think\n they will be below the value ',y0a,'?\n')
  
  n0a<-lectura()
  print(n0a)
  
  cat('What would be the minimum number of subjects out of the 100 
that you would accept as valid less than ',y0a,'?\n')
  
  n0La<-lectura()
  print(n0La)
  
  cat('What would be the maximum number of subjects of the 100 that
would accept as valid greater than ',y0a,'?\n')
  
  n0Ua<-lectura()
  print(n0Ua)
  
  print('Pick another point on the dependent variable and press 
enter ..')
  y1a<-lectura()
  print(y1a)
  
  cat('From a sample of 100 subjects with a value of x=',x0a,' 
how many do you believe\n that they will be below the value 
',y1a,'?\n')
  
  n1a<-lectura()
  print(n1a)
  
  cat('What would be the minimum number of subjects out of 100 who
I would accept as valid less than ',y1a,'?\n')
  
  n1La<-lectura()
  print(n1La)
  
  cat('What would be the maximum number of subjects of the 100 that 
would accept as valid greater than ',y1a,'?\n')
  
  n1Ua<-lectura()
  print(n1Ua)
  
  # Calculations
  s2<-(y0a-y1a)/(qnorm(n0a/100)-qnorm(n1a/100))
  m2<-y0a-s2*qnorm(n0a/100)
  
  print(c(m2,s2))
  
  
} #END

elicita.modelo.lineal()