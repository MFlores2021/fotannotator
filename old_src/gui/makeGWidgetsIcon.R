###############################################################################
#
# TODO: Add comment
#
# May 25, 2011
# 4:38:30 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################



makeGWidgetsIcon <- function(w, giffile) {
	
	makeIconTcltk <- function(w, giffile) {
		
#	if(as.character(tkwinfo("class", w)) == "Toplevel" &&
#			file.exists(giffile)) {
		tkimage.create("photo","::icon::name", file=giffile)
		tcl("wm","iconphoto", w, "::icon::name")
#	}
	}
	
	W <- getToolkitWidget(w)
	if(is(w@widget, "gWindowRGtk"))
		makeIconRGtk2(W, giffile)
	if(is(w@widget, "gWindowtcltk"))
		makeIconTcltk(W, giffile)
}
