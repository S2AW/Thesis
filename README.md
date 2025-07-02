This repository features a filtered .csv and R scripts that I consider works in progress.
I am uploaded to GitHub so that I may share with classmates the progress thus far. 

I don't know when I will consider this code finalized.

For context:
Adversary Network.R: Builds the network structures based on the provided CSV file.
Attack_Vector.R: Generates attack vectors to estimate baseline effort for each node, using the output from Just_Israel.R.
Bonacich_Centrality.R: Computes Bonacich centrality scores and identifies key actors within the network.
  The user must choose the type of network to analyze: binary or weighted.
  The user must also specify which vector to use: no vector (by commenting out a section), or vectors based on conflict involvement and casualty data.
  These options are indicated within the script and should be visible in the comments and code structure.
Empiric_Decrease.R: averages  events over time, highlighting key changes in relation to major historical events in the Middle East, using the output from Adversary Networks.R.
