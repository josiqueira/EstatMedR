# eiras.plotlasRStudio.R

eiras.plotlasRStudio <- function(filename)
{
  # preserve old 
  newfile <- paste("./",filename,sep="")
  oldfile <- paste("./",filename,".old",sep="")
  if (file.exists(newfile))
  {
    file.rename(from=newfile, to=oldfile)
  }
  # find temporary images and save last image
  plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
  plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
  infoimg <- file.info(plots.png.paths)
  df_tmp <- data.frame(plots.png.paths,infoimg$atime)
  df_tmp <- df_tmp[order(df_tmp$infoimg.atime),]
  lastimg <- as.character(df_tmp$plots.png.paths[nrow(df_tmp)])
  file.copy(from=lastimg, to=newfile)
  cat("copy from",lastimg,"to",newfile,"\n")
}

# # https://stackoverflow.com/questions/35321775/save-all-plots-already-present-in-the-panel-of-rstudio
# # from Taher Ahmed Ghaleb 20181217
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="./teste.png")
# 
# # from Mr. Duhart 20180214
# for(i in 1:lastplot) {
#   png('teste.png')
#   print(ph[i])
#   dev.off()
# }
