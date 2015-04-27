###############################################################################
#
# TODO: Add comment
#
# Oct 3, 2011
# 2:01:52 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


xgconfirm = function(message, title="Confirm", icon = c("info", "warning", "error",
				"question"), parent=NULL,
		handler = NULL, action = NULL)
	{
	w = gconfirm(message, title=title, icon = icon, parent=parent,
			handler = handler, action = action) 
	makeGWidgetsIcon(w, icon)
	w
}
