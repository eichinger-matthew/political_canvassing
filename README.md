# political_canvassing
Algorithm to find how best to assign canvassers to residents to maximize electoral vote shares

The repository is split into (1) .R files for running the model, (2) .RData objects with algorithm outcomes, and (3) a (very raw) .pdf file for the paper 

# Idea

There are plenty of scholarly papers that study how political parties operate canvassing campaigns in the real world. They mostly detail how campaigns choose stratgies, the tactics they use to reach people, and how they attract donations. Strangely, however, there are no scholarly papers that study how political canvassing - an individual part of a broader campaign strategy - should take place, given that a party has information on their constituents and their canvassers. Is this an oversight? Or do parties have a solid heuristic for canvassing that negates the need to build a canvassing algorithm? This projects tests this idea by creating a novel canvassing algorithm and calculating performance diagnostics.

# Note about runtime

The 'fast multi-person' assignment method runs pretty quickly - less than a minute on my graphics card, which is a 2070.
The 'slow single-person' assignment method runs pretty slowly - more than half an hour (maybe an hour?)

The marginal gains from the fast multi-person method outweight the marginal gains from the slow single-person method, in my opinion. Lets you do more exploration


# Files

aside from the master_script_canvassing.R script, the .R files are the individual components of the master script. they do narrow but key parts of the analysis.

algorithm_functions.R contain the primary functions used in the algorithm
canvassing_agents_generation.R creates the hypothetical canvassers and residents with associated characteristics
canvassing_geography_simulation.R has functions to distribution residents across an arbitrarily-sized xy plane; the distribution is structured using a "neighborhood' concept
canvassing_prediction_model.R is the parametric statistical model used to estimate the marginal probability of voting for a particular candidate, given that they are visited by a canvasser with a specific vector of characteristics
figures_and_tables.R generates graphs for the paper
house_generation.R is an extension to the main paper; it adds multi-person houses and multi-person apartments, where "travel time" is effectively zero
OLD_canvassing_programs.R is a previous script I used to develop the algorithm; it has no purpose in the repository other than transparency





