	###############################################################################
	#
	#
	# August 27th, 2014
    #
	# 02:25 PM
	# Author: Mirella Flores (mflores)
	# (c) International Potato Center
	#
	###############################################################################
	setwd("D:\\AnnotatorTool")
	
	#############################
	# setup the reference directory for source code
	# switch to bin in distributed version
	##########################
	
	library("utils") #Necessary for version upgrade to R 2.14.1; otherwise will stop for not finding progressbar
					 #GTDM-52
	app.dir = "src"
	if(!file.exists(app.dir)) app.dir="bin"
	
	startup = function(){
		
		xmin=1
		i=1
		xmax=100
		pb <- winProgressBar("Loading application ...", "Loading module ... %",
				xmin, xmax, xmin)	
		
		update.prog <- function(i, package) {
			#info <- sprintf(paste("%d%%",translate("_MSG_DONE_")), round(i/xmax*100))
			info <- sprintf(paste("%d%%","done"), round(i/xmax*100))
			#setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_LOADING_"),"(%s)"), info), package)
			setWinProgressBar(pb, i, sprintf(paste("Loading ","(%s)"), info), package)
		}
		
		
		#############################
		# setup helper functions
		#############################
		
		
		import <-function(package,i){
			update.prog(i = i, package = package)
			if(!require(package, character.only=TRUE)) 
				install.packages(package,  depend=TRUE)
			library(package, character.only=TRUE)
		}
		
		
		include <- function(module,i){
			update.prog(i = i, package = module)
			source(file.path(app.dir,module))
		}
		
		
	#############################
	# setup the progress bar
	#############################

	update.prog(i = i, package = "AnnotatorTool")
	
	#############################
	# external libraries
	#############################
	
	import("gWidgetstcltk",1)
	options(guiToolkit="tcltk")
	#import("RSQLite",2)
	import("stringr",3)
	#import("testthat",4)
	#import("xlsx",5)
	#import("agricolae",6)
	#import("date",7)
	
	#import("gWidgetsWWW",8)
	#import("randomForest",9)
	#import("lme4",10)
	import("abind",11)
	#import("MASS",12)
	import("EBImage",13)
	#import("biomaRt",11)
	#############################
	# modules
	#############################
	include(file.path("gui","makeGWidgetsIcon.R"),19)

	#include(file.path("utils","get.version.nr.R"),25)
	#include(file.path("utils","utils.R"),25)

	include(file.path("gui","xgwindow.R"),27)
	include(file.path("gui","xgconfirm.R"),28)
	include(file.path("gui","xgmessage.R"),29)
	
	#include("create.Pref.Dlg.R",43)
	
	include(file.path("image","cut.R"),64)
	#include(file.path("image","AnnotatorTool.R"),65)				########descomentar fotos normales
	include(file.path("image","AnnotatorTool.R"),65)		########solo semillas
	#############################
	# load user interface
	#############################
	#import("gWidgets2",8)
	#include(file.path("gui","gui.R"),xmax)
	#source(file.path(app.dir,"gui.R"))
	close(pb)
	source(file.path(app.dir,file.path("gui","gui.R")))
	#############################
	# finish progress bar
	#############################

	}
	
	#initialize()
	startup()
	
	
	
