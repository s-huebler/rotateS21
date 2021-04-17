propTest <- function(x){
  xbar <- mean(x)
  #print(xbar)
  sD <- sd(x)

  #print(sD)
  n <- length(x)

  count1 <- 0
  for (i in 1:n){
    if (x[i] < xbar - sD ) {count1 = count1 + 1}
    #print("a")}
    if (x[i] > xbar + sD) {count1 = count1 +1}
    #print("b")}
    count1 = count1
  }
  #print(count1)


  count2 <- 0
  for (i in 1:n){
    if (x[i] < xbar - 2*sD) {count2 = count2 + 1}
    if (x[i] > xbar + 2*sD) {count2 = count2 +1}
    count2 = count2
  }
  #print(count2)

  oneSD <- ifelse(abs((1-count1/n)-.683) > 1.396/sqrt(n), "Fail", "Pass")
  twoSD <- ifelse(abs((1-count2/n)-.954) > .628/sqrt(n), "Fail", "Pass")


  if(identical(oneSD, twoSD)){
    ret <- oneSD
  }else {
    ret <- "Fail"}

  final <- list("Result" = ret,
                "PropOne" = 1-count1/n,
                "PropTwo" = 1-count2/n)

}

propPlot <- function(vec){
  mu <- mean(vec)
  dev <- sd(vec)
  a <- mu + dev
  b <- mu - dev
  c <- mu + 2 * dev
  d <- mu - 2 * dev


  p<-ggplot(data=data.frame("data"=vec), aes(data))+
    geom_freqpoly(bins=length(vec)/3)+
    theme_classic()+
    geom_vline(xintercept = a,
               color="blue",
               lty=2)+
    geom_vline(xintercept = b,
               color="blue",
               lty=2)+
    geom_vline(xintercept = c,
               color="red",
               lty=2)+
    geom_vline(xintercept = d,
               color="red",
               lty=2)+
    geom_vline(xintercept = mu,
               color= "black",
               lty=2)

  suppressWarnings(p)
}


rQ <- function (vec){

  vec <- matrix(unclass(vec), ncol=1)

  n <- nrow(vec)

  xbar <- mean(vec)

  vec <- vec[order(vec)]

  q <-c()
  for (i in 1:n){
    q[i] <- qnorm((i-0.5)/n)
  }


  qbar <- mean(q)



  top <- 0
  d1 <- 0
  d2 <- 0
  for (j in 1:n){
    top <- top + (vec[j]-xbar)*(q[j])
    d1 <- d1 +  (vec[j]-xbar)^2
    d2 <- d2 + (q[j]-qbar)^2
  }


  ret <- top/(sqrt(d1)*sqrt(d2))

}




qqplot <- function(vec){

  vec <- matrix(unclass(vec), ncol=1)
  n <- nrow(vec)
  xbar <- mean(vec)
  vec <- vec[order(vec)]

  shap <- shapiro.test(vec)

  q <-c()
  for (i in 1:n){
    q[i] <- qnorm((i-0.5)/n)
  }


  df <- data.frame("obs" = vec, "q"=q)

  plot <- ggplot(data=df, aes(x=q, y=obs))+
    geom_point()+
    xlab("Normal Distribution")+
    ylab("Observed Distribution")+
    ggtitle("QQ-Plot")+
    theme_classic()+
    labs(caption=paste("Shapiro test has test statistic", round(shap$statistic, 4),
                       "with an associated p-value of", round(shap$p.value, 3),
                       sep=" "))+
    theme(plot.caption = element_text(size=20))

  suppressWarnings(plot)

}


ellipsePlot <- function(x,y, alpha=.05){

  df <- data.frame("x"=x, "y"=y)
  mat <- as.matrix(df, ncol=2)

  df$x <- unlist(df$x)
  df$y <- unlist(df$y)

  mu <- colMeans(df)

  s <- cov(mat)

  eig <- eigen(s)


  chi <- as.numeric(qchisq(alpha, 2, lower.tail = FALSE))




  rmaj <- as.numeric(sqrt(eig$values[1]*chi))


  rmin <- as.numeric(sqrt(eig$values[2]*chi))

  ang <- 90-atan(eig$vectors[1,1]/eig$vectors[2,1])*180/pi

  ell <- Ellipse$new(center = mu,
                     rmajor = rmaj,
                     rminor = rmin,
                     alpha= ang)

  ellpath <- ell$path()
  ellpath <- rbind(ellpath, ellpath[1,])

  p <- ggplot(as.data.frame(ellpath), aes(x=x, y=y))+
    geom_path(color="red")+
    theme_classic()+
    geom_point(data=df, aes(x=df[,1], y=df[,2]))+
    ggtitle("Bivariate Scatter Plot")



  suppressWarnings(p)


}

chiPlot <- function(x, y) {

  df <- data.frame("x"=unlist(x), "y"=unlist(y))
  n <- nrow(df)

  s <- cov(df[,c(1,2)])
  sInv <- solve(s)

  means<-data.frame("x1"=df$x-mean(df$x), "x2"=df$y-mean(df$y))

  multi<-c()
  for(i in 1:n){
    multi[i]=as.matrix(means[i,]) %*% sInv %*% t(as.matrix(means[i,]))
  }

  multi<-sort(multi)

  chi_plot<-data.frame("j"=seq(1:n), "d2"=multi) %>%
    mutate("q"=qchisq((j-0.5)/n, 2))

  p <- ggplot(chi_plot, aes(x=q, y=d2))+
    geom_point()+
    theme_classic()+
    xlab("Chi")+
    ylab("Dist")+
    ggtitle("Chi-Square Plot for Bivariate Data")

  suppressWarnings(p)
}

outDet <- function(df) {

  n <- nrow(df)
  name <- names(df)
  s <- cov(df)
  sInv <- solve(s)

  #means<-matrix(colMeans(df), ncol=1)
  meanCorrect <- as.data.frame(apply(df, 2, FUN= function(x){scale(x, center=TRUE, scale=FALSE)}))

  multi<-c()
  for(i in 1:n){
    multi[i]=as.matrix(meanCorrect[i,]) %*% sInv %*% t(as.matrix(meanCorrect[i,]))
  }


  d2 <- as.data.frame(multi)
  names(d2) <- "d2"


  z <- as.data.frame(apply(df, 2, FUN= function(x){scale(x)}))
  temp<-c()
  for(j in 1:ncol(df)){
    temp[j]<-paste("z", name[j], sep="")
  }

  names(z) <- temp


  final <- cbind(df,z)
  final <- cbind(final, d2)
  final
}
