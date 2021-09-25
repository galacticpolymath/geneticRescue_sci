require(clipr)
set.seed(155)
seq1<-sample(c("C","G","T","A"),300,T)
set.seed(157)
seq2<-sample(c("C","G","T","A"),300,T)

write_clip(paste0(seq1,collapse=""))
write_clip(paste0(seq2,collapse=""))
