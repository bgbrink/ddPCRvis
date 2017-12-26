# ddPCRvis

ddPCRvis is an interactive, web based GUI for the [ddPCRclust](https://github.com/bgbrink/ddPCRclust) algorithm. It was developed using the web application framework [Shiny](https://shiny.rstudio.com/). 

## Installation
An online version of the app is available under https://bibiserv.cebitec.uni-bielefeld.de/ddPCRvis/

If you want to use it locally, you can simply clone this repository and run the app using RStudio. Please note, that the [ddPCRclust](https://github.com/bgbrink/ddPCRclust) package needs to be installed on your machine or the [submodule](https://github.com/blog/2104-working-with-submodules) in this repo needs to be cloned as well using ```git clone --recursive https://github.com/bgbrink/ddPCRvis.git```

You also need to install the following packages.

```R
install.packages(c('shiny', 'shinyjs', 'rhandsontable', 'jsonlite', 'devtools', 'R.utils', 'roxygen2', 'plotrix', 'clue', 'parallel', 'ggplot2', 'openxlsx', 'plotly'), repos='https://cran.rstudio.com/')
```

If you use ddPCRclust as a submodule, you also need the following Bioconductor packages.
```R
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite(c('flowDensity', 'SamSPECTRAL', 'flowPeaks'))
```

## Usage
Since you found your way here, I think it's safe to assume that you are familiar with websites and how to navigate them. ddPCRvis works exactly the same. On the top, you have the navigation bar:
<p align="center">
<img 
src="https://user-images.githubusercontent.com/11661112/30427716-20a7b36a-9951-11e7-9d85-a8e25a84abd5.PNG"  
alt="ddPCRvis navigation">
</p>

Notice the help button on the right side. Click it and it will explain everything you need to know on the current page. On the left side, a sidebar contains all available input fields for the current view. Here is an example for the 'Upload files' view:
<p align="left">
<img 
src="https://user-images.githubusercontent.com/11661112/28871441-da3a5e70-7784-11e7-93c6-1cfe41dd7880.PNG"  
alt="ddPCRvis upload example">
</p>

To get started, you have to upload your data. Since one experiment most likely consists of many different files, naming them apropriately is important in order to keep things organized. We chose to use a unique identifier in each filename of the form `"^[[:upper:]][[:digit:]][[:digit:]]$"` (A01, A02, A03, B01, B02, ...), which is usually included automatically by the ddPCR machine.
In order to get the best results, it is recommended to set up a template file for your experiment. The template has to be a *csv* file with a header, which contains information about each of the raw data files according to their unique identifier, as explained above. 

*> Name of your experiment, channel1=HEX, channel2=FAM, annotations(date, experimentor, etc)*

Well|Sample type|No of markers|Marker 1|Marker 2|Marker 3|Marker 4
---|---|---|---|---|---|---
B01|Blood|4|a|b|c|d
G01|FFPE|4|a|b|c|d
F02|Blood|3|a||c|d
D03|FFPE|3|a||c|d
A04|FFPE|4|a|b|c|d
G07|Cell line|3|a||c|d
G08|Cell line|3|a||c|d
E09|FFPE|2|||c|d

The raw data should be *csv* files. Each file represents a two-dimensional data frame. Each row within the data frame represents a single droplet, each column the respective intensities per colour channel (please make sure your decimal symbol is a point):

Ch1 Amplitude | Ch2 Amplitude 
--- | --- 
2360.098 |	6119.26953
2396.3916 |	1415.31665
2445.838 |	6740.79639
2451.63867 |	1381.74683
2492.55884 |	1478.19617
2519.6355 |	7082.25049
&#8942; | &#8942;

After you hit 'Start Analysis', the [ddPCRclust](https://github.com/bgbrink/ddPCRclust) algorithm will run in the background. Once the computation is completed (allow ~5 seconds per file), you will be redirected to the next page, where you can inspect the result and spot any errors or other irregularities. Depending on your internet connection and the number of files you submitted, loading the plots in the browser can take a little while, so please be patient.
<p align="center">
<img 
src="https://user-images.githubusercontent.com/11661112/28871944-1f21e484-7787-11e7-908f-e932dfa9132a.PNG"  
alt="ddPCRvis plots example">
</p>

You can manually edit the clustering result for each well individually. Once you are satisfied, click on 'Count droplets' to continue. You can then inspect and download the droplet counts for each cluster, as well as the individual copies per droplet (CPDs) per marker, if you provided this information in the template.
<p align="center">
<img 
src="https://user-images.githubusercontent.com/11661112/28872118-0b843494-7788-11e7-9102-f4177999e97d.PNG"  
alt="ddPCRvis counts example">
</p>

Lastly, ddPCRvis provides you an interactive visualization to explore the results and compare markers vs stable controls.
<p align="center">
<img 
src="https://user-images.githubusercontent.com/11661112/30428577-6dae989c-9954-11e7-8651-09af23342ba7.PNG"  
alt="ddPCRvis results vis">
</p>


## License

    MIT License

    Copyright (c) 2017 Benedikt G. Brink

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
