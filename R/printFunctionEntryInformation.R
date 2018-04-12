# PRINTFUNCTIONENTRYINFORMATION
# 
#	Print function name inclusive parameter values (if verbose = TRUE)
#
# Author: stefanhaunsberger
###############################################################################


printFunctionEntryInformation = function (fun, match_call) {
	
	writeLines("================================================");
	writeLines(sprintf("%s(): Start ...", fun[1]));
	writeLines("Call details:");
	print(match_call);
	writeLines("------------------------------------------");
	
	return(as.character(fun[1]));
	
}	