f2 <- function(df) {
  # check the input based on the criteria
  stopifnot(ncol(df)==4,is.numeric(df[[1]]),is.numeric(df[[2]]),is.numeric(df[[3]]),all(df[[4]]%%1==0),all(df[[4]]>0))

  # make a plot based on the df with x is the n value and y is the result of the function 1
  y<- vector(mode="numeric",length = nrow(df))
  x<-vector(mode="numeric",length = nrow(df))
  for(i in 1:nrow(df))
  {
    myseq_n(x=c(df[[1]][i],df[[2]][i],df[[3]][i]),df[[4]][i])->y_value
    y[[i]]<-y_value
    x[[i]]<-df[[4]][i]
  }
  df<-data.frame(x=x,y=y)
  print(df)
  ggplot(df,mapping=aes(x=x,y=y))+
    geom_line()->plot
  return(plot)
}
