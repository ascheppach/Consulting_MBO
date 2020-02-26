We tried to keep the order and structure of our final work:

1_1 First we analyze the provided Code of Lars and give a boxplot-curve to show how much the optimization improve over the initial data and how much improvement is achieved in each step/Iteration. We run the provided code 10 times to build the boxplot-curve.

1_2 We analyze the effect of good / bad inital data on the provided Code

1_3 We analyze the effect of additional Iterations on the provided Code


####
2_ Then in the second part of our work we gonna make some BO Changes and analyze how this affect the mbo-optimization-process. 

2_1 In the first part of this chapter we analyze the single effect of some BO Changes. Thus, we change one hyperparameter of the BO and fix the rest. To analyze the effect of InitialDesign for instance, we fixed the rest of the mbo process to its defaults, and try out different intialDesigns. To get an idea how the result is going to look like, check out the file "affect inital data"(beside of intialdesign and amount initaldesign we gonna investigate also the affect of surrogates and infillcriterias). Furthermore we implemented a Framework, in order to automate the creation of boxplot-curves.

2_2 In the second part of this chapter we implement an hyperparamteroptimization of MBO (with Irace), to get better configurationen. Furthermore we do an ablation analysis to check out which hyperparameters have the biggest effect on the optimization. Check out the file "ablation" to get an idea of the result.


###
3_1 At the end we implement some Algorithms as alternative to MBO (random, tunedMBO from 2_2, Racing, CMAESR).

3_2 After we gonna benchmark the Algorithms with Batchtool to find out which tuningAlgorithm is capable to investigate the best configurations. Check out the file "algoComparison" to get an idea of the result.


Some additional comment: we fixed some objects to save computation time please clone this repro.
