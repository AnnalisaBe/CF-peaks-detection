
// Author: Annalisa Bellandi, Faulkner Group
// 29/04/2022
// Scan images along vertical and horizontal lines of a grid, saves the profiles along thise lines

input_folder = getDirectory("Choose input folder, files with images to be processed") //script designed for one-slice images, results will be saved in teh same folder

setBatchMode(true);

list = getFileList(input_folder);
		for (i=0; i<list.length; i++) {
            print(list[i]);
            path = input_folder+list[i];
            
n = roiManager("count");
if (n>0) {
    roiManager("Delete");
}

run("Clear Results");

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

   widthlines = 50/pxw;
   nLines = 50;
   width = getWidth;
   S=width-1;
   height = getHeight;
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
		
//saves ROIs		
roiManager("save", dir + "ROIs.zip");
