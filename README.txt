This file gives the code that was used in this project to generate the plots and results that are documented in my thesis. The descriptions of the files are given below as well as how to run the documents. A note about R files: to run a certain line of code, highlight what you want to run and press command + enter and it will be sent to the command line. 

predictW.r
This file is the code that finds the W shapes within the data. To do this, run the part of the code that imports the csv files and reorders the data (because data is in backwards order when downloaded from the internet). After this, just execute all the function definitions so that you only have to do it once. After that, there are two different predictions you can do. The slopes/lengths prediction is at the top of the file and the directional vector prediction is commented out at the end of the file. 

predictM.r
The same as predictW.r, except that the code finds the M shape.

predictHS.r
Same as predictW.r, except that the code finds the head and shoulders shape.

3d plot.r
This file mostly deals with plotting 3D data. The initial plot is just the plot of time, price, volume of the stock data that is imported. At the end of the file, there are commands to determine the distributions at every 10 points and plot the means and standard deviations at each step. To make this work, just run the code the order it is written.