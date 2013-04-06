
is_mandel <- function(x0,y0) {
  max_it <- 1000
  x <- 0
  y <- 0
  i <- 1
  while(i<=max_it && x*x+y*y<4) {
    xtemp <- x*x-y*y+x0
    y <- 2*x*y+y0
    x <- xtemp
    i <- i + 1
  }
  return(i)
}

# old_fn <- function(x0,y0) {}
#   z <- matrix(0,dim(X)[1],dim(X)[2])
#   for(i in 1:max_it) {
#     xtemp <- x*x-y*y+x0
#     y <- 2*x*y+y0
#     x <- xtemp
#     x[x*x+y*y>4] <- 2
#     y[x*x+y*y>4] <- 2
#     z[(x*x+y*y>4) & (z==0)] <- i
#   }
#   z*15
# } 


do_almost_everything <- function(h) {
  x <- seq(-2.5,1,by=h)
  y <- seq(-1,1,by=h)
  X <- outer(x,y,function(a,b) a)
  Y <- outer(x,y,function(a,b) b)
  L <- list(X=X,Y=Y)
  return(L)
}

do_everything <- function(h) {
  x <- seq(-2.5,1,by=h)
  y <- seq(-1,1,by=h)
  X <- outer(x,y,function(a,b) a)
  Y <- outer(x,y,function(a,b) b)
  
  M <- is_mandel(X,Y)
  image(M,x=x,y=y)
  return(M)
}

complex_mandel <- function(m, max_it=20) {
  jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", 
                                  "yellow", "#FF7F00", "red", "#7F0000")) 
  C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), 
               imag=rep(seq(-1.2,1.2, length.out=m), m ) ) 
  C = matrix(C,m,m)  # reshape as square matrix of complex numbers
  Z = 0     # initialize Z to zero
  #X = array(0, c(m,m,20)) # initialize output 3D array
  for (k in 1:max_it) {  # loop with 20 iterations
    Z = Z^2+C    # the central difference equation 
    #X[,,k] = exp(-abs(Z)) # capture results
  } 
  X = exp(-abs(Z))
  L <- list(X=X,x=seq(-1.8,0.6,length.out=m),y=seq(-1.2,1.2,length.out=m),col=jet.colors)
  return(L)
}

init_mandel <- function(m) {
  jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", 
                                  "yellow", "#FF7F00", "red", "#7F0000")) 
  C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), 
               imag=rep(seq(-1.2,1.2, length.out=m), m ) ) 
  C = matrix(C,m,m)  # reshape as square matrix of complex numbers
  L <- list(C=C,x=seq(-1.8,0.6,length.out=m),y=seq(-1.2,1.2,length.out=m),col=jet.colors)
  return(L)
}

it_mandel <- function(C) {
  #Naive vectorized method
  max_it <- 1000
  Z = 0     # initialize Z to zero
  #X = array(0, c(m,m,20)) # initialize output 3D array
  for (k in 1:max_it) {  # loop with 20 iterations
    Z = Z^2+C    # the central difference equation 
    #X[,,k] = exp(-abs(Z)) # capture results
  } 
  X = exp(-abs(Z))
}

it_filter_mandel_v1 <- function(C) {
  #Vectorized, with cutoff 
  max_it <- 1000
  J <- 1:length(C)
  Z <- rep(0,length(C))
  R <- rep(-1,length(C))
  dim(R) <- dim(C)
  W <- rep(0,length(C))
  #Z <- matrix(0,nrow=dim(C)[1],ncol=dim(C)[2])
  #R <- matrix(-1,nrow=dim(C)[1],ncol=dim(C)[2])
  #W <- matrix(0,nrow=dim(C)[1],ncol=dim(C)[2])
  
  #X = array(0, c(m,m,20)) # initialize output 3D array
  for (k in 1:max_it) {  # loop with 20 iterations
    L <- J[J>0]
    Z[L] <- Z[L]^2+C[L]# the central difference equation 
    W[L] <- abs(Z[L])
    M <- L[W[L]>2]
    R[M] <- k
    J[M] <- 0
    #X[,,k] = exp(-abs(Z)) # capture results
  } 
  return(R)
}

it_filter_mandel_v2 <- function(C) {
  #Vectorized with cutoff, better.
  #Two indices used so vector size reduces at each iteration
  #i.e. local indices (L) + global indices (J)
  max_it <- 1000
  
  J <- 1:length(C)
  L <- 1:length(C)
  Z <- rep(0,length(C))
  R <- rep(-1,length(C))
  K <- list()
  dim(R) <- dim(C)
  #W <- rep(0,length(C))
  #Z <- matrix(0,nrow=dim(C)[1],ncol=dim(C)[2])
  #R <- matrix(-1,nrow=dim(C)[1],ncol=dim(C)[2])
  #W <- matrix(0,nrow=dim(C)[1],ncol=dim(C)[2])
  
  #X = array(0, c(m,m,20)) # initialize output 3D array
  for (k in 1:max_it) {  # loop with 20 iterations
    L <- L[J>0]
    J <- J[L]
    
    C <- C[L]
    Z  <- Z[L]
    Z <- Z^2+C# the central difference equation 
    
    L <- 1:length(J)
    M <- L[abs(Z)>2]
    
    K[[k]] <- J[M]
    #R[J[M]] <- k
    J[M] <- 0
    #X[,,k] = exp(-abs(Z)) # capture results
  } 
  
  for (k in 1:max_it) {
    R[K[[k]]] <- k
  }
  
  return(R)
}

it_filter_mandel_v3 <- function(C) {
  #Vectorized with cutoff, better.
  #Two indices used so vector size reduces at each iteration
  #i.e. local indices (L) + global indices (J)
  max_it <- 1000
  l_init <- length(C)
  d_init <- dim(C)
  
  J <- 1:length(C)
  #L <- 1:length(C)
  Z <- rep(0,length(C))
  
  K <- list()
  
  for (k in 1:max_it) {  # loop with 20 iterations 
    Z <- Z^2+C# the central difference equation 
    
    #Termination Condition
    M <- abs(Z)>2
    
    #Storing results
    K[[k]] <- J[M]
    
    #Updating indices
    L <- 1:length(J)
    L <- L[!M]
    J <- J[L]
    #Contracting arrays
    C <- C[L]
    Z  <- Z[L]
  } 
  
  R <- rep(-1,l_init)
  dim(R) <- d_init
  for (k in 1:max_it) {
    R[K[[k]]] <- k
  }
  
  return(R)
}

it_filter_mandel_v4 <- function(C) {
  #Vectorized with cutoff, better.
  #Two indices used so vector size reduces at each iteration
  #i.e. local indices (L) + global indices (J)
  #Tried to avoid additional copys/allocation by subindexing, but this 
  #seems to not work.
  max_it <- 1000
  l_init <- length(C)
  d_init <- dim(C)
  
  J <- 1:length(C)
  L <- 1:length(C)
  Z <- rep(0,length(C))
  M <- abs(Z)>2
  
  K <- list()
  
  for (k in 1:max_it) {  # loop with 20 iterations 
    Z[L] <- Z[L]^2+C[L]# the central difference equation 
    
    #Termination Condition
    M[L] <- abs(Z[L])>2
    
    #Storing results
    K[[k]] <- J[M[L]]
    
    #Updating indices
    L <- L[!M[L]]
    J <- J[L]
    LN <- 1:length(J)
    #Contracting arrays
    C[LN] <- C[L]
    Z[LN]  <- Z[L]
    L <- LN
  } 
  
  R <- rep(-1,l_init)
  dim(R) <- d_init
  for (k in 1:max_it) {
    R[K[[k]]] <- k
  }
  
  return(R)
}



run_seq_mandel <- function(L,mandel=it_mandel) {
  Sys.time()->start
  L$X <- mandel(L$C)
  dim(L$X) <- dim(L$C)
  L$time <- Sys.time()-start
  return(L)
}

run_par_mandel <- function(L,mandel=it_mandel) {
  L$cl <- makeCluster(getOption("cl.cores", detectCores()))
  Sys.time()->start
  L$X <- parSapply(L$cl,L$C,mandel)
  dim(L$X) <- dim(L$C)
  L$time <- Sys.time()-start
  return(L)
}

test_all <- function(m) {
  L <- init_mandel(m)
  print("Sequential naive")
  S1 <- run_seq_mandel(L,it_mandel)
  print(S1$time)
  print("Sequential filtered")
  S2 <- run_seq_mandel(L,it_filter_mandel)
  print(S2$time)
}


evolution_mandel <- function() {
  #http://blog.revolutionanalytics.com/2010/09/mandelbrot-set.html
  library(caTools)  # external package providing write.gif function
  jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", 
                                  "yellow", "#FF7F00", "red", "#7F0000")) 
  m = 600     # define size
  C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), 
               imag=rep(seq(-1.2,1.2, length.out=m), m ) ) 
  C = matrix(C,m,m)  # reshape as square matrix of complex numbers
  Z = 0     # initialize Z to zero
  X = array(0, c(m,m,20)) # initialize output 3D array
  for (k in 1:20) {  # loop with 20 iterations
    Z = Z^2+C    # the central difference equation 
    X[,,k] = exp(-abs(Z)) # capture results
  } 
  write.gif(X, "Mandelbrot.gif", col=jet.colors, delay=100)
}


