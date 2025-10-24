# Single Reinforcer Biphasic Sipper Model

This folder contains all materials related to the biphasic sipper model for experiments using a single reinforcer (e.g., ethanol OR sucrose).

## Contents
- `example_data/`: Sample MED-PC output files and processed data for ethanol or sucrose self-administration.
- `medpc_code/`: MED-PC program files used to run the biphasic sipper paradigm.
- `MedParser_SingleReinforcer_20250730.R`: Shiny application for parsing and analyzing data from the biphasic sipper model paradigm when only a single reinforcer is available.

## Paradigm Overview

Rats are trained to self-administer a single reinforcer (ethanol or sucrose) using a biphasic sipper model paradigm, as developed by [Samson et al. (1998).](https://pubmed.ncbi.nlm.nih.gov/9835295/)

Briefly, subjects are given a fixed amount of time (e.g., 20 minutes) to complete a single fixed ratio requirement (e.g., 20 lever presses). 
Upon completing the response requirement, they gain uninterrupted access to a sipper tube for a fixed duration (e.g., 20 minutes), allowing measurement of consummatory behavior following a discrete appetitive phase.

## Analysis Features Available in the Shiny Application

- Summary table including time stamps of key events, latencies to key events, lick counts, and topography details
- Cumulative record plots separated by the appetitive phase and consummatory phase
- Bin analyses: Generates a table of the number of lever presses or licks occurring throughout the session at user-defined bins
- Bout analyses: Generates a table of the number of licks that occur per drinking bout, a lapse in drinking for a user-defined period of time

## Documentation
For full documentation, see [`docs/single_reinforcer_sippermodel.md`](../docs/single_reinforcer_sippermodel.md).
