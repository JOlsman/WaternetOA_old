#' Reads Sobek his output files into Large Array's
#' @export
readhis<-function(filename){
  library(stringr)
  zz <- file(filename, "rb")
  readChar(zz,40)
  readChar(zz,40)
  readChar(zz,40)
  readChar(zz,40)
  afm<-readBin(zz,integer(),n=2)
  syname<-vector("character",afm[1])
  idump<-vector("integer",afm[2])
  duname<-vector("integer",afm[2])
  for(i in 1:afm[1]){
    syname[i]<-readChar(zz,20)
  }
  for(i in 1:afm[2]){
    idump[i]<-readBin(zz,integer(),n=1)
    duname[i]<-readChar(zz,20)
  }

  it=-1
  itn<-vector("integer",0)
  tel<-0
  conc<-vector("numeric",0)
  while(length(it)>0){
    tel<-tel+1
    it<-readBin(zz,integer(),n=1)
    if (length(it)>0){
      itn<-c(itn,it)
      conc<-readBin(zz,"numeric",n=afm[1]*afm[2],size=4)
    }
  }

  close(zz)
  zz <- file(filename, "rb")
  readChar(zz,40)
  readChar(zz,40)
  readChar(zz,40)
  readChar(zz,40)
  afm<-readBin(zz,integer(),n=2)
  syname<-vector("character",afm[1])
  idump<-vector("integer",afm[2])
  duname<-vector("integer",afm[2])
  for(i in 1:afm[1]){
    syname[i]<-readChar(zz,20)
  }
  for(i in 1:afm[2]){
    idump[i]<-readBin(zz,integer(),n=1)
    duname[i]<-readChar(zz,20)
  }
  concar<-array(dim=c(length(itn),afm[2],afm[1]))
  for(i in 1:length(itn)){
    it<-readBin(zz,integer(),n=1)
    concar[i,,]<-matrix(readBin(zz,"numeric",n=afm[1]*afm[2],size=4),nrow=afm[2],ncol=afm[1],byrow=T)
  }

  close(zz)
  dimnames(concar)<-list(itn,str_trim(duname),str_trim(syname))

  return(concar)
}
