myseq_n<-function(x,n){
  stopifnot(n>0 |length(x)>=3 | typeof(x)=="numeric")
  myseq_n<-vector(mode = "numeric",length = n)
  myseq_n[1]=x[1]
  myseq_n[2]=x[2]
  myseq_n[3]=x[3]
  if (n<=3){
    return(myseq_n[n])
  }
  if (n>3){
    for(i in 4:n){
      myseq_n[i]=myseq_n[i-1]+(myseq_n[i-3]-  myseq_n[i-2])/4
    }
    return(myseq_n[n])
  }
}
