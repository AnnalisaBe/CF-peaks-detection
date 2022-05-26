
// Author: Annalisa Bellandi, Faulkner Group
// 29/04/2022
// Scan images along vertical and horizontal lines of a grid, saves the profiles along thise lines
// Images are one-channel and one-slice .czi files

// Place all your images in a folder (input folder), make sure only the images to be processed are in the folder and nothing else 
// Select the input folder when prompted
// Results of the analysis will be saved in the input folder


//============================================================================================
//======================= select and run the following =======================================

//------------------- adjustable parameters, adjust before you start, if you wish --------------

chosenlinesN=50 //how many vertical lines and how many horizontal lines do you want? We will have an equal numebr of vertical and horizontal lines, chosing 50 here will give a total of 100 lines
chosen_um_thickness=50 //how thick do you want each line to be? I set it at 50 um, may change if you have images of different size

//------------------- actual analysis ---------------------------------------------------------

//prompts you to select the folder that you want
input_folder = getDirectory("Choose input folder, files with images to be processed") 

setBatchMode(true);

//gets a list of all your files in the chosen input folder
list = getFileList(input_folder);
		for (i=0; i<list.length; i++) {
            print(list[i]);
            path = input_folder+list[i];

//wipes ROI manager and results tab clean to get started           
n = roiManager("count");
if (n>0) {
    roiManager("Delete");
}

run("Clear Results");

//open the first image and retrieves informations
run("Bio-Formats Importer", "open=[" + path + "] autoscale color_mode=Default open_files rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");

dir = getInfo("image.directory");// get the image directory
print(dir);
imageTitle=getTitle; //get the image name
run("Duplicate...", "duplicate"); //duplicate the entire stack for measurements
measuredImageTitle="dup"+imageTitle; //add the starting "dup" to the original image title
rename(measuredImageTitle);//rename it
selectWindow(measuredImageTitle); //select the duplicate image to measure

//get pixel size
getPixelSize(unit, pixelWidth, pixelHeight);
print("Current image pixel width = " + pixelWidth + " " + unit +".");
pxw = pixelWidth;
print(pxw);

   widthlines = chosen_um_thickness/pxw;
   nLines = chosenlinesN;
   width = getWidth; //image size
   height = getHeight; //image size
   tileWidth = width/(nLines+1);
   tileHeight = height/(nLines+1);
   xoff=tileWidth;
   yoff=tileHeight;
   
   for (v=0; v<=nLines-1; v++){ //vertial lines
      makeLine(xoff, 1, xoff, height-1, widthlines);
   	profile = getProfile();
    roiManager("add");

  for(j=0; j<profile.length; j++){
  		setResult(v, j, profile[j]);
        setResult("d", j, j*pxw);        
        }
   }
   
   for (h=nLines; h<=(2*nLines); h++){ //horizontal lines
      makeLine(1, yoff, width-1, yoff, widthlines);
  profile = getProfile();
  roiManager("add");
  for(j=0; j<profile.length; j++){
    	 setResult(h, j, profile[j]);
    	 setResult("d", j, j*pxw);        
        }
        
  yoff += tileHeight;
   }
   
title_without_extension = substring(imageTitle, 0, lengthOf(imageTitle)-31);
saveAs("Results", dir + title_without_extension + ".csv");

print(title_without_extension, " - all done");
close();
close();
		}
		
//saves ROIs, each line is numbered, you can reopen them by dragging and dropping the zip folder on Fiji interface		
roiManager("save", dir + "ROIs.zip");

print("analysis finished");

//=======================================================  end of the analysis ==================
