# political_canvassing
Algorithm to find how best to assign canvassers to residents to maximize electoral vote shares<br />

The repository is split into (1) .R files for running the model, (2) .RData objects with algorithm outcomes, and (3) a (very raw) .pdf file for the paper 

# Idea

There are plenty of scholarly papers on canvassing campaigns in the real world. They mostly detail how political parties choose stratgies, the tactics they use to reach people, and how they gather donations. Strangely, however, there are no scholarly papers on how political canvassing - an individual part of a broader campaign strategy - should optimally take place, given that a party has information on their constituents and their canvassers. This paper fills this gap by building a computational model of canvassing. I test multiple methods of allocating canvassers, some of which are good heuristics, some which are variations on the "optimal" approach, and some of which are bad heuristics but useful benchmarks. In the end, I find that (a) the optimal methods outperform good heuristics, (b) the "fast" optimal canvassing method can outperform the "slow" one, and (c) when campaigns lack detailed data on their constituents, a "neighborhood" heuristic is a good bet.

# Note about runtime

The 'fast multi-person' assignment method runs pretty quickly - less than a minute on my graphics card, which is a 2070.<br />
The 'slow single-person' assignment method runs pretty slowly - more than half an hour (maybe an hour?)<br />

The marginal gains from the fast multi-person method outweight the marginal gains from the slow single-person method, in my opinion. Lets you do more exploration


# Files

aside from the master_script_canvassing.R script, the .R files are the individual components of the master script. they do narrow but key parts of the analysis.<br />

algorithm_functions.R contain the primary functions used in the algorithm<br />
canvassing_agents_generation.R creates the hypothetical canvassers and residents with associated characteristics<br />
canvassing_geography_simulation.R has functions to distribution residents across an arbitrarily-sized xy plane; the distribution is structured using a "neighborhood' concept<br />
canvassing_prediction_model.R is the parametric statistical model used to estimate the marginal probability of voting for a particular candidate, given that they are visited by a canvasser with a specific vector of characteristics<br />
figures_and_tables.R generates graphs for the paper<br />
house_generation.R is an extension to the main paper; it adds multi-person houses and multi-person apartments, where "travel time" is effectively zero<br />
OLD_canvassing_programs.R is a previous script I used to develop the algorithm; it has no purpose in the repository other than transparency<br />




