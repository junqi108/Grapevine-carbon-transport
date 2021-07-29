# Grapevine-carbon-transport
This code corresponding to the paper published in In Silico Plants: Simulating organ biomass variability and carbohydrate distribution in perennial fruit crops: a comparison between the common assimilate pool and phloem carbohydrate transport models


Simple instruction would be: 
Specify the main path for finding the scenario file and parameter file, click save to initialize the inputs, and click run to run the model step by step. That’s it ^-^

Preparation: 
First you need to install GroIMP.exe or the linux version (https://sourceforge.net/projects/groimp/) and Java run environment 8.0 (http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html). 

After install those two things, you should be able to open the GroIMP and see some examples in the File tab – show examples. You can then open the GrapeXL code through File-open option. 

When you open the code, first thing to do is go to the File Explorer, global parameters, then go down to find the definition of MainPath. Then you change the MainPath into the folder where you put the model input and model scenario file. 
Use the Boolean variable useCTRAM to switch between detail carbon transport model or common carbon pool model. 
static boolean useCTRAM	= true;		 //use detail carbon transport model or not

See more instructions in the Model Instruction folder
