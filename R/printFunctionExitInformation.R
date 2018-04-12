# PRINTFUNCTIONEXITINFORMATION
# 
#	Print function name
#
# Author: stefanhaunsberger
###############################################################################


printFunctionExitInformation = function (fun) {
	
	writeLines("------------------------------------------");
	writeLines(sprintf("%s(): Done", fun[1]));
	writeLines("===========================================");
	
}	