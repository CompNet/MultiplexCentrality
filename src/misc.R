#############################################################################################
# Various secondary functions.
#############################################################################################



#############################################################################################
# Loads an Rdata file supposed to contain a single object, then returns it.
# The source code was taken from StackOVerflow user Hong Ooi, see the following URL: 
# http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
# 
# filename: name of the file to load.
# returns: the single object contained in the specified file.
#############################################################################################
retrieve.rdata.object <- function(filename)
{	env <- new.env()
	nm <- load(filename, env)[1]
	return(env[[nm]])
}
