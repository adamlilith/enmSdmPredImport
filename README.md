# enmSdmPredImport

The `enmSdmPredImport` package for the R Statistical Environment enables the creation of virtual species for assessing the inferential power of species distribution models. The package is used in Smith, A.B. and Santos, M.J. Accepted. Testing the ability of species distribution models to infer variable importance. Ecography. (A pre-print of this manuscript is available for free at https://doi.org/10.1101/715904.) That manuscript is based on scripts in the GitHub repository https://github.com/adamlilith/enmSdmPredImport_scenarios which uses `enmSdmPredImport`. Those scripts provide the best illustrations of how to use this package.

The package has three main functions which are called in sequence:

##`predImportMakeData`
Creates a set of data scenarios based on simulated landscapes (environmental layers) with user-defined properties and a species with a known relationship to the environment. We refer to the particular combination of landscape and species as a "data scenario." Data scenarios can be iterated to simulated randomness in placement of the species' presences, absences, and background sites used to train and evaluate a distribution model (a "data iteration"). Output is a series of files for a given data scenario, one per data iteration, used by subsequent functions.

##`predImportTrainModels`
Trains a species distribution modeling algorithms on each data scenario. Algorithms supported include BIOLCIM, BRTs, GAMs, GLMs, MaxEnt (and MaxNet), and RFs, plus an "omniscient" model the same as used to simulate the species' original probability of distribution. Output is a set of files, one per algorithm, data iteration, and type of analysis ("multivariate" models with all variables, "reduced" models with each variable dropped in turn, and "univariate" models with just one variable at a time). Note that in Smith and Santos only the "multivariate" models were used.

##`predImportEval`
Assesses the predictive accuracy and inferential accuracy of each combination of data scenario iteration, model algorithm, and model type (multivariate, reduced, univariate). Output is a series of files, one per data scenario containing a data frame with evaluation statistics.

Last updated 2020-07-29
Adam B. Smith
