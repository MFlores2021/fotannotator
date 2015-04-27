###############################################################################
#
# TODO: Add comment
#
# May 25, 2011
# 4:36:35 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


xgwindow = function(title = "Window", visible = TRUE, name=title,
		width = NULL, height= NULL, parent=NULL,
		handler = NULL, action = NULL, icon="bin/icon.gif",
		..., toolkit = guiToolkit()){
	w = gwindow(title = title, visible = visible, name=title,
			width = width, height= height, parent=parent,
			handler = handler, action = action,
			..., toolkit = toolkit)
	makeGWidgetsIcon(w, icon)
	w
}
