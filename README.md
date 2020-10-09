# enmSdmPredImport

The `enmSdmPredImport` package for the R Statistical Environment enables the creation of virtual species for assessing the inferential power of species distribution models. The package is used in Smith, A.B. and Santos, M.J. Accepted. Testing the ability of species distribution models to infer variable importance. Ecography. (A pre-print of this manuscript is available for free at https://doi.org/10.1101/715904.) That manuscript is based on scripts in the GitHub repository https://github.com/adamlilith/enmSdmPredImport_scenarios which uses `enmSdmPredImport`. Those scripts provide the best illustrations on how to use this package.

## Use ##
The package has three main functions which are called in sequence:

`predImportMakeData`: Creates a set of data scenarios based on simulated landscapes (environmental layers) with user-defined properties and a species with a known relationship to the environment. We refer to the particular combination of landscape and species as a "data scenario." Data scenarios can be iterated to simulated randomness in placement of the species' presences, absences, and background sites used to train and evaluate a distribution model (a "data iteration"). Output is a series of files for a given data scenario, one per data iteration, used by subsequent functions.

`predImportTrainModels`: Trains a species distribution modeling algorithms on each data scenario. Algorithms supported include BIOLCIM, BRTs, GAMs, GLMs, MaxEnt (and MaxNet), and RFs, plus an "omniscient" model the same as used to simulate the species' original probability of distribution. Output is a set of files, one per algorithm, data iteration, and type of analysis ("multivariate" models with all variables, "reduced" models with each variable dropped in turn, and "univariate" models with just one variable at a time). Note that in Smith and Santos only the "multivariate" models were used.

`predImportEval`: Assesses the predictive accuracy and inferential accuracy of each combination of data scenario iteration, model algorithm, and model type (multivariate, reduced, univariate). Output is a series of files, one per data scenario containing a data frame with evaluation statistics.

Additionally, the `loadEvals` function is a utility to load and compile all evaluation files for a given data scenario into a single data frame for futher analysis.

All other functions in the package are called by the three main functions, but can also be used stand-alone. Of particular interest may be the `genesis` function for creating square and circular landscapes with different patterns of environmental variables.  See the help for this function and any other function for details.

## Installation ##
To install this package and dependencies you will need the packages (from CRAN) `sp`, `raster`, `gbm`, `dismo`, and `rJava`. You will also need to install dependences from GitHub:

`# install.packages('remotes') # only if you don't have "remotes" installed already!`  
`remotes::install_github('adamlilith/omnibus, dependencies=TRUE')`  
`remotes::install_github('adamlilith/statisfactory, dependencies=TRUE')`  
`remotes::install_github('adamlilith/legendary, dependencies=TRUE')`  
`remotes::install_github('adamlilith/enmSdm, dependencies=TRUE')`  
`remotes::install_github('adamlilith/enmSdmPredImport, dependencies=TRUE')`

Note that if you have issues installing any of the latter packages (e.g., because of using an earlier/later version of R), you can download the ZIP file of the package from the `zipTarFiles` directory in each of these packages. You can then install manually in R from this file.

Use of the model algorithm `maxent` requires copying a Java file into the "java" directory of the library of your `dismo` installation. You can obtain a copy of this file from https://biodiversityinformatics.amnh.org/open_source/maxent/.

## Citation ##

This package was first used in Smith, A.B. and Santos, M.J.  In press. Testing the ability of species distribution models to infer variable importance. *Ecography* https://doi.org/10.1111/ecog.05317

Abstract: Models of species’ distributions and niches are frequently used to infer the importance of range‐ and niche‐defining variables. However, the degree to which these models can reliably identify important variables and quantify their influence remains unknown. Here we use a series of simulations to explore how well models can 1) discriminate between variables with different influence and 2) calibrate the magnitude of influence relative to an ‘omniscient’ model. To quantify variable importance, we trained generalized additive models (GAMs), Maxent and boosted regression trees (BRTs) on simulated data and tested their sensitivity to permutations in each predictor. Importance was inferred by calculating the correlation between permuted and unpermuted predictions, and by comparing predictive accuracy of permuted and unpermuted predictions using AUC and the continuous Boyce index. In scenarios with one influential and one uninfluential variable, models failed to discriminate reliably between variables when training occurrences were < 8–64, prevalence was > 0.5, spatial extent was small, environmental data had coarse resolution and spatial autocorrelation was low, or when pairwise correlation between environmental variables was |r| > 0.7. When two variables influenced the distribution equally, importance was underestimated when species had narrow or intermediate niche breadth. Interactions between variables in how they shaped the niche did not affect inferences about their importance. When variables acted unequally, the effect of the stronger variable was overestimated. GAMs and Maxent discriminated between variables more reliably than BRTs, but no algorithm was consistently well‐calibrated vis‐à‐vis the omniscient model. Algorithm‐specific measures of importance like Maxent's change‐in‐gain metric were less robust than the permutation test. Overall, high predictive accuracy did not connote robust inferential capacity. As a result, requirements for reliably measuring variable importance are likely more stringent than for creating models with high predictive accuracy.

Last updated 2020-10-09
Adam B. Smith
