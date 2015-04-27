###############################################################################
# June 08, 2013
# 7:59:34 AM
# Author: Mirella Flores
# (c) International Potato Center
#
###############################################################################

# tesseract


read.txt.from.image <- function(fp){
	
	tmpfile <- file.path('temp','unsharped.jpg.txt')
	
	if(file.exists(tmpfile)) file.remove(tmpfile)
	
	#call tesseract via command line from R
	tfn = file.path ("bin","tesseract","tesseract")
	
	cmd = paste(tfn,fp,fp)
	
	system(cmd,ignore.stdout = TRUE,show.output.on.console = FALSE)
	
	#read archive (several lines)
	
	txt = readLines(paste(fp,".txt",sep=""))
	
	#just make one string out of it separating former lines with ;
	
	txt = paste(txt,collapse=";")
	
	file.remove(paste(fp,".txt", sep = ""))
#print(txt)
	return(txt)
	
}

get.cipnumber.from.txt <-function(txt){
	
	cpn = "(CIP)?[1-9]{1}[0-9@yDZO/]{5}(\\.[0-9@yDZO]{1,4})?" 
	
	#make sure to preced cip number with CIP 
	
	txt = str_replace_all(txt," ","")
	txt = str_replace_all(txt,"@","0")
	txt = str_replace_all(txt,"y","1")
	txt = str_replace_all(txt,"D","0")
	txt = str_replace_all(txt,"Z","2")
	txt = str_replace_all(txt,"A","4")
	txt = str_replace_all(txt,"/","7")
	txt = str_replace_all(txt,"B","8")
	txt = str_replace_all(txt,"O","0")
	txt = str_replace_all(txt,"é","6")
	txt = str_replace_all(txt,"ó","6")
	txt = str_replace_all(txt,"®","0")
	txt = str_replace_all(txt,"I","1")
	txt = str_replace_all(txt,"Â","")
	txt = str_replace_all(txt,"\\]","1")
	txt = str_replace_all(txt,"\\[","1")
	txt = str_replace_all(txt,",",".")
	txt = str_replace_all(txt,"U","0")
	txt = str_replace_all(txt,"©","0")
	txt = str_replace_all(txt,"Â®","0")
	
	cip = str_extract(txt,cpn)
	
	if(is.na(cip)) cip = ''
	
	return(cip)
}

read.cipnumber.from.image <- function(fp){
	
	#CIP number pattern/regular expression

	
	txt = read.txt.from.image(fp)
	
	cip = get.cipnumber.from.txt(txt)
	
	if (cip == '111111' || cip == '777777' || str_detect(cip,'12345')) cip = ''
	#cip =  NULL
	return(cip)
	
}

image.zbar <- function (imagei){
	
	cpn = "(CIP)?[0-9@yDZO/]{6}(\\.[0-9@yDZO]{1,4})?" 

	tfn = file.path ("bin","zxing")
	#tfn = file.path ("bin","zbar","zbarimg")
	tfna = ("java -cp bin\\zxing\\javase-3.1.0.jar;bin\\zxing\\core-3.1.0.jar com.google.zxing.client.j2se.CommandLineRunner ")
	#try(cip <- system2(tfna, args=paste("--multi --dump_results ",imagei), stdout = TRUE, stderr = FALSE), silent = TRUE)
	cip <- system(paste(tfna,imagei), intern = TRUE) #, stdout = TRUE, stderr = FALSE) , show.output.on.console=FALSE
	#try(A <- system2("C:\\Program Files\\ZBar\\bin\\zbarimg.exe", args=paste("-q --raw",imagei, sep = " "), stdout = TRUE, stderr = FALSE), silent = TRUE)
	cip = str_extract(cip,cpn)
	cip = cip[!is.na(cip)]
	cip = moda.cipnumber(cip)
	
	return(cip)
}

image.unsharp <- function (imagei,params){
	
	tmpfile <- file.path('temp',"unsharped.jpg")
	
	tfn = file.path ("bin","ImageMagick","convert")
	
	system2(tfn, args=paste(params,imagei, tmpfile, sep = " "), stdout = TRUE, stderr = FALSE)

}

image.typefile <- function (namefile){
	typefile = ""
	typefile <- do.call(rbind, strsplit(namefile,"\\."))
	typefile <- typefile[,length(typefile)]	
	return(typefile)
}

file.verify.name <- function (namefile){

	N			=	str_replace_all(string=namefile, pattern=" ", repl="")
	
	file.rename(namefile, N)
	
	namefile	=	N
	
	return(namefile)
}

compare.result <- function(cipz,cipt,list.cip) {
	
	cip = NULL
	strcip = NULL
	cipnumber = NULL
	#list.cip = str_replace_all(list.cip,"CIP","")
	ciptmp=c(cipt,cipz)
	ciptmp=ciptmp[ciptmp != ""]

	# if (length(ciptmp)){
		# for (i in 1:length(ciptmp)) {
		# if (ciptmp[i] %in% list.cip) cip = c(cip,ciptmp[i])
	# }}

	#if (!is.null(cipz)) if (cipz %in% list.cip) cip = cipz
	#if (!is.null(cipt)) if (cipt %in% list.cip) cip = c(cipt,cip)

	if(is.null(cip)) {
		
		cip = moda.cipnumber(ciptmp)
		
		if (!is.null(cip)){
			if (length(cip)>1) strcip = 'CIP' #'Please-check-CIP'
			else {
				if (cip != '') strcip = 'CIP' #'Please-check-CIP'
			}}
	}
	else {
		cip = moda.cipnumber(cip)
		strcip = 'CIP'
	}	

	if(!is.null(strcip)) {
		if (length(cip)>1) {
				cip = unique(cip)
		}
			
	 cipnumber = paste(strcip,paste(cip, collapse='-'), sep = "")
	}
	return(cipnumber)
}

image.rename <- function(oldfilesi,filecip){
	
	tmpfile <- file.path('temp','unsharped.jpg')
	
	file_exists = list.files(".", pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")

	tryCatch(image.crop(oldfilesi), error = function(err) {return(NULL)})

	labelfiles=file.path('temp',list.files('temp', pattern = ".new.jpg"))
		
		if(!length(labelfiles)) labelfiles=oldfilesi
		
		x <- 1
		
		CIPN=NULL
		CIPT=NULL
		CIPZ=NULL
		CIPN1=NULL
		cipnumber=NULL
		
		for(x in 1:length(labelfiles)){
			
			tryCatch({		
			
				if(is.null(CIPZ)){
	
					Z <- image.zbar(labelfiles[x])
					
					if (Z != '' && length(Z)) {
						CIPZ = c(Z,CIPZ) 					
						file.remove(list.files("temp", pattern = ".new.jpg", full.names = TRUE))
						break
					}
					T <- read.cipnumber.from.image(labelfiles[x])
								
					if (T != '' && length(T)){
						CIPT = c(CIPT,T) 	
						
					}					
					
					if(is.null(CIPZ)){
						params = "-unsharp 6x3"
						image.unsharp(labelfiles[x],params)
						
						if(file.exists(tmpfile)){						
							Z <- image.zbar(tmpfile)
						
							if (Z != '' && length(Z)) {
								CIPZ = c(Z,CIPZ) 							
								file.remove(list.files("temp", pattern = ".new.jpg", full.names = TRUE))
								break
							}
							T <- read.cipnumber.from.image(tmpfile)
									
							if (T != '' && length(T))	CIPT = c(CIPT,T) 									
							file.remove(tmpfile)
						}				
					
						if(is.null(CIPZ)){
							params = "-unsharp 3x1+7"
							image.unsharp(labelfiles[x],params)
						
							if(file.exists(tmpfile)){						
								
								Z <- image.zbar(tmpfile)	
								
								if (Z != '' && length(Z)) {
									CIPZ = c(Z,CIPZ) 									
									file.remove(list.files("temp", pattern = ".new.jpg", full.names = TRUE))
									break
								} 							
								
								T <- read.cipnumber.from.image(tmpfile)
								
								if (T != '' && length(T))	CIPT = c(CIPT,T) 		
										
								file.remove(tmpfile)
							}
					}}
				}						
			}, error=function(cond) NA )
			
			if(labelfiles[x]!=oldfilesi)	file.remove(labelfiles[x])
			
		} 
				

		tryCatch({
		
			CIPN = CIPZ
		
			if (!length(CIPN) || is.null(CIPN)){
				
				Z <- image.zbar(oldfilesi)				
				if (Z != '' && length(Z)){
					CIPN = Z 				
				}
					
				if (!length(CIPN) || is.null(CIPN)){				
					T <- read.cipnumber.from.image(oldfilesi)		
							
					if (T != '' && length(T)){
						CIPT = c(CIPT,T)							
					}
					
					CIPN = compare.result(CIPZ,CIPT,filecip)						
						
				}
			}
					
			if(length(CIPN) || !(is.null(CIPN))){
							
				CIPN = str_replace_all(CIPN,"CIP","")
				cipnumber <-paste("CIP", CIPN, sep="")
				#if(cipnumber %in% file_exists) cipnumber <- paste(CIPN,'-1',sep = "")
				
			}
			
		}, error=function(cond) NA )
		
		if(file.exists(tmpfile)) file.remove(tmpfile)
		file.remove(list.files("temp", pattern = ".jpg", full.names = TRUE))
		
		cipnumber = str_replace_all(cipnumber,"CIP","")
		
		return (cipnumber)
		
}

moda.cipnumber <- function(x) {
	z <- table(x)
	n <- names(z)[z == max(z)]
	
	return(n)
}

menu.rename <- function(dir_initial,fbp,befCIP,ppl,yeardate,autor){

	part_plant = part.plant(ppl)
	# create folder
	dir_final <- file.path(fbp,part_plant) 

	if(!file.exists(dir_final)) dir.create(dir_final, recursive = TRUE)
		
	# copy files to new directory
	
	oldfiles=list.files(dir_initial, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
	
	contador <- 0
	
	dput("List",file=paste(dir_final,"log.txt"))
	
	if (length(oldfiles)){
		
		pb <- winProgressBar("Renaming", "Progress in %",0, 100, 1)
		
		file.copy(file.path(dir_initial,oldfiles), dir_final)
				
		listcipn = file.path(dir_initial,'cipnumber.txt')
		
		if(file.exists(listcipn)) listcipn = readLines(listcipn) else listcipn = ''
		
		# list into new directory
		oldfiles = list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
		
		oldfiles = file.path(dir_final,oldfiles)
			
		keys <- paste(ppl,';',"Renaming")
				
		for(i in 1:length(oldfiles)) {
		
			# main function: recursive rename
			oldfiles[i] = file.verify.name(oldfiles[i])
			
			print(oldfiles[i])		
			
			write(basename(oldfiles[i]),file=paste(dir_final,"log.txt"),append=TRUE)
			
			CIPN <- image.rename(oldfiles[i],listcipn)
		
			if (length(CIPN)){				
		
				#CIPN	= image.name(CIPN)
				file_exists = file.path(dir_final,list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp"))
				
				count=2
				new_name = file.path(dir_final,paste(befCIP,CIPN,yeardate,part_plant,'.jpg',sep=''))
								
				while(new_name %in% file_exists){ 		
				
					new_name = file.path(dir_final,paste(befCIP,CIPN,yeardate,part_plant,count,'.jpg',sep=''))
					
					count=count+1
				}
					
				file.rename(oldfiles[i],new_name)
				
				write(basename(new_name),file=paste(dir_final,"log.txt"),append=TRUE)
				
				#add.legend(yeardate,autor,new_name)
				
				contador= contador+1
			} 
				
				write("--------*--------",file=paste(dir_final,"log.txt"),append=TRUE)
				
				setWinProgressBar(pb, round(i/length(oldfiles)*100, 0), label=paste(round(i/length(oldfiles)*100, 0),"% done"))
		}
		
		close(pb)
}
	msg = paste("Renamed:",contador,"files", sep = " ")
	
	gmessage(msg,ico="info")
}

part.plant <- function (p){
	if (p == 'plant') return('p') 
	if (p == 'flower') return('fl') 
	if (p == 'flower dissection') return('df') 
	if (p == 'fruit') return('b')
	if (p == 'seeds') return('s')
	if (p == 'tuber') return('t')
	if (p == 'root') return('r')
	if (p == 'sprout') return('sp')
	if (p == 'herbarium') return('hr')
	if (p == 'habitat') return('hb')
	if (p == 'foliage') return('f') 
	if (p == 'Other') return('') 
}
localities.list <- function (l){
	if (l == 'San Ramon') return('CIPSRM-1') 
	if (l == 'La Victoria') return('LAVICT') 
	if (l == 'La Molina') return('CIPHQ') 
	if (l == 'Cañete') return('CAÑETE')
}

get.org.list= function(){

	org = c('plant','flower','flower dissection','fruit','seeds','tuber','root','sprout','herbarium','habitat','foliage','chips','Other')
	return(org) 
}
get.localities= function(){

	localities = c('San Ramon','La Victoria','La Molina','Cañete')
	return(localities) 
}
#w <- gwindow("Annotator", visible=TRUE)
create.img.Dlg = function(w){
	
	dput("cancel",file="bin/temp.txt")
	
	win <- gbasicdialog(title='Add images to experiment', handler = function(h,...) {
				
			if(!is.na(svalue(gl[8,1])) && !is.na(svalue(gl[1,1]))){
				
					result = c(svalue(gl[1,2]),svalue(gl[2,2]),svalue(gl[8,1]),svalue(gl[3,2]),svalue(gl[5,2]),svalue(gl[6,2]))
					
					dput(result,file="bin/temp.txt")
			}
			}, parent=w)
	
	gl = glayout(cont=win)
	gl[1,1]=glabel("Experiment:", cont=gl)
	gl[1,2]=gedit("ExperimentName", cont=gl)
	gl[2,1]=glabel("Types of picture:", cont=gl)
	gl[2,2]=gcombobox(get.org.list(), cont=gl)
	gl[3,1]=glabel("Author(s):", cont=gl)
	gl[3,2]=gedit("Author", cont=gl)
	gl[5,1]=glabel("Before:", cont=gl)
	gl[5,2]=gedit("CIP", cont=gl) 
	gl[6,1]=glabel("After:", cont=gl)
	gl[6,2]=gedit("", cont=gl)
	gl[7,1]=gbutton("Select ...", container=gl,
			 handler = function(h,...) {
				 image.dir = choose.dir()
				 
				 if(!is.na(image.dir)){
					 
					 gl[8,1]=glabel(image.dir, cont=gl)
					 svalue(gl[8,1])= image.dir
					 gl[9,1]=glabel("                                    ", cont=gl)
				 }
				 else gl[9,1]=glabel("Enter images directory", cont=gl)
				 
				 })
	
	visible(win, set=TRUE) ## show dialog
	fn=dget("bin/temp.txt")
	dput("cancel",file="bin/temp.txt")
	fn
}

get.images <- function (w){
	
	#tryCatch({
				
		res = create.img.Dlg(w)
		
		if ( length(res) == '6'){
		
			fbp   = file.path("Renamed",res[1])
			local = res[5] #localities.list(res[5])
			menu.rename(res[3],fbp,local,res[2],res[6],res[4])
		}
	#}, error=function(cond) NA )
} 

read.list.cipnumber <- function(xls,t_sheet){
	
	wb  	<- loadWorkbook(xls)
	sheets	<- getSheets(wb)
	
	sheet 	<- sheets[[t_sheet]]
	if (t_sheet=='Material List'){
		n = sheet$getLastRowNum()+1
		m = 10
		v = readColumns(sheet,1,m,1,n)
		#df = as.data.frame(v[,3],stringsAsFa=F)
		df =as.character(v[,"Institutional.number"])
	}
	return(df)
}

add.legend <- function (year,autor,im){
	
	
	h=as.numeric(image.identify(im,'%h')) 

	w=as.numeric(image.identify(im,'%w')) 
	numberautor <- do.call(rbind, strsplit(autor,"\\,"))

	if ((length(numberautor))>1){
		str_autor = "Authors:"
	}	
	else str_autor = "Author:"
	if(w>h){
		point_size = round(as.numeric(w*0.01))
	}
	else{
		point_size = round(as.numeric(h*0.01))
	}
	
	point = h*0.96
	tfn = file.path ("bin","ImageMagick","convert")
	txt = paste('" ©Copyright 2014 International Potato Center.\n ',str_autor,autor,' "',sep='')
	tryCatch(system2(tfn,args=paste("-pointsize ", point_size," -annotate +1+",point," ",txt," ",im," ",im, sep = ""), stdout = TRUE), error = function(cond)NA ) 

}

