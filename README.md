We tried to keep the order and structure of our final work:

1_ First we analyze the provided Code of Lars and give a boxplot-curve to show how much the optimization improve over the initial data and how much improvement is achieved in each step/Iteration. We run the provided code 10 times to build the boxplot-curve.

2_ Then in the second part of our work we gonna make some BO Changes and analyze how this affect the mbo-optimization-process. To analyze the effect of InitialDesign for instance, we fixed the rest of the mbo process to its defaults, and try out different intialDesigns. To get an idea how the result is going to look like, check out our Boxplots 2_4 and 2_5 (beside of intialdesign and amount initaldesign we gonna investigate also the affect of surrogates and infillcriterias).

3_ We implement four hyperparametertuning algorithms and at the beginning we want each tuning algorithm running 200 experiments to investigate the best configuration.

4_ At the end we gonna benchmark the hyperparametertuning Algorithms with Batchtool to find out which tuningAlgorithm is capable to investigate the best configurations.


Some additional comment: we fixed some objects to save computation time please clone this repro.
