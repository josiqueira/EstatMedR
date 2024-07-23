# eiras.findcommonchars.R

findcommonchars <- function(strings)
{
  minlen <- min(nchar(strings))
  m <- matrix(ncol=minlen,nrow=length(strings)+1)
  for (r in 1:length(strings))
  {
    for (c in 1:minlen)
    {
      m[r,c] <- substr(strings[r],c,c)
    }
  }
  m <- toupper(m)
  m[nrow(m),] <- " "
  chars <- LETTERS
  chars <- c(chars,0:9)
  for (c in 1:minlen)
  {
    cntgm <- rep(0,length(chars))
    for (r in 1:length(strings))
    {
      for (l in 1:length(chars))
      {
        if (m[r,c]==chars[l])
        {
          cntgm[l] <- cntgm[l]+1
        }
      }
    }
    maxcnt <- max(cntgm)
    if (maxcnt == length(strings))
    {
      m[nrow(m),c] <- chars[which(cntgm==maxcnt)]
    }
  }
  txt <- ""
  for (c in 1:minlen)
  {
    if (m[nrow(m),c] != " ")
    {
      txt <- paste(txt, m[nrow(m),c], sep="")
    }
  }
  return (txt)
}

# x <- c("abcde1","abcdf2","abcd3")
# nomecomum <- findcommonchars(x)
# cat(nomecomum,"\n")