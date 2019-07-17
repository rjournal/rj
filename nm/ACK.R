

###############  acknowledge receipt of submissions #####################

# msNums: char vec of ms numbers to be ACKed

ack <- function(msNums) {
   # get in-memory DESCRIPTION file
   if (!exists('desFiles'))  {
      getAll()
      print('desFiles regenerated')
   }
   # get formletter (global; no need, should change this)
   source('../../rj/nm/ACKTmplt.R')
   for (msNum in msNums) {
      cat('ACKing',msNum,'\n')
      des <- desFiles[[msNum]]
      cat('\ncheck correct ms!\n')
      print(des)
      ans <- readline('correct file (yes or no) ')
      if (ans != 'yes') stop('wrong file')
      autinfo <- getAutInfo(des)
      frml <- formletter
      frml[1] <- sub('GREET',autinfo[1],frml[1])
      title <- des[1]
      title <- substr(title,8,nchar(title))
      frml[1] <- sub('TITLE',title,frml[1])
      print(frml)
      mailIt(autinfo[2],'"R Journal manuscript received"',NULL,frml)
   }
}

