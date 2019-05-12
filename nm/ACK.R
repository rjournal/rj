

###############  acknowledge receipt of submissions #####################

ack <- function(msNum) {
   # get in-memory DESCRIPTION file
   if (!exists('desFiles'))  {
      getAll()
      print('desFiles regenerated')
   }
   des <- desFiles[[msNum]]
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')
   source('../../rj/nm/ACKTmplt.R')
   autinfo <- getAutInfo(des)
   formletter[1] <- sub('GREET',autinfo[1],formletter[1])
   title <- des[1]
   title <- substr(title,8,nchar(title))
   formletter[1] <- sub('TITLE',title,formletter[1])
   mailIt(autinfo[2],'"R Journal manuscript received"',NULL,formletter)
}

