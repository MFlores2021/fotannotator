###############################################################################
#
# 
#
# Aug 27, 2011
# 3:39:34 PM
# Author: Mirella Flores (mflores)
# (c) International Potato Center
#
###############################################################################


title= paste("AnnotatorTool",sep="")

w <- xgwindow(title, visible=F)

m.rename.images <-function(w){
	get.images(w)			
	}
	
gui <- function(title=title, w=w){
	
	visible(w) <- FALSE
	
	actions <- list(Rename=gaction("Rename photos", icon="save", handler=m.rename.images),
				 quit=gaction("Quit", icon="quit", handler=function(...) dispose(w)))
	action2 <- list(quit=gaction("Quit", icon="quit", handler=function(...) dispose(w)))
	 
	 menubarlist <- list(Start=actions,Close=action2)
	 
	 gmenu(menubarlist, cont = w)
		
		# rightArrow <- system.file("banner.JPG",package="gWidgets")
		print(getwd())
		
		gimage(filename ="potato.gif", dir =getwd() ,container =w, toolkit = guiToolkit())
		glabel("Annotator Tool", container = w) 
	visible(w) <- TRUE
	w
}


w=gui(title,w)

while(isExtant(w)) Sys.sleep(1)



	


 
 
 
 