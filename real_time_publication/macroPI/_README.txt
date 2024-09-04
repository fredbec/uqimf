If you only want to run check: 
- either save locally first:
	- run data_loadsave.R, will save to MacroPI repo (don't commit yet) 
	- change URLs in app.R to local paths
- or push to other (unofficial) MacroPI repo
	- run data_loadsave_MacroPI_check.R
	- commit to that repo 
	- change URLs in app.R to corresponding URLs in other repo (simply change "MacroPrediction/MacroPI" to "MacroPII/MacroPI")