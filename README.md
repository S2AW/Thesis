Repository Overview

This repository contains a filtered CSV dataset and a set of R scripts that are currently works in progress. I am sharing the repository to provide classmates with visibility into the development process and interim results. The code and documentation will continue to evolve; there is no set timeline for when I will consider the project finalized.

Scripts Included

Adversary_Network.R
Builds the network structures based on the provided CSV file.

Attack_Vector.R
Generates attack vectors to estimate baseline effort for each node, using the output from Adversary_Network.R.

Bonacich_Centrality.R
Computes Bonacich centrality scores and identifies key actors within the network.
Users must:

Select the type of network to analyze: binary or weighted.

Specify which vector to apply: no vector (by commenting out a designated section), or vectors derived from conflict involvement and casualty data.
These options are documented within the code and accompanying comments.

Empiric_Decrease.R
Averages events over time and highlights key changes associated with major historical developments in the Middle East, using the output from Adversary_Network.R.

Status

All scripts should be considered preliminary. Assumptions, methods, and outputs may change as the project progresses. Feedback is welcome.
