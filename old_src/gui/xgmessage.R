###############################################################################
#
# TODO: Add comment
#
# Oct 3, 2011
# 2:04:47 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


xgmessage = function(message, title="Confirm", icon = c("info", "warning", "error",
				"question"), parent=NULL,
		handler = NULL, action = NULL)
{
	w = gmessage(message, title = title, icon = icon, parent=parent,
			handler = handler, action = action) 
	makeGWidgetsIcon(w, icon)
	w
}
