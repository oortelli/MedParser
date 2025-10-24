# MedParser Documentation Site

Welcome to the documentation site for **MedParser**, an R-based toolkit designed to parse and analyze behavioral data from .txt files. While originally developed for self-administration studies using Med-PC, the functions in MedParser are broadly applicable to any context where structured information needs to be extracted from plain text files.

This site provides detailed documentation for each experimental context where MedParser has been applied, including code, data examples, and usage notes.

---

## Paradigm Pages

Explore how MedParser has been adapted for different behavioral paradigms:

- [Single Reinforcer Self-Administration, Biphasic Sipper Model](single_reinforcer_sippermodel.md)  
 Subjects are given a fixed amount of time (e.g., 20 minutes) to complete a single fixed ratio requirement (e.g., 20 lever presses). Upon completing the response requirement, they gain uninterrupted access to a sipper tube for a fixed duration (e.g., 20 minutes), allowing measurement of consummatory behavior following an appetitive phase.

- Concurrent choice self-administration 
  *(Documentation coming soon — page will include parsing code and examples for dual-reinforcer paradigms)*

- Operant Fear Conditioning
  *(Documentation coming soon — page will include parsing code and examples for an operant fear paradigm)*

- IV Self-Administration (non-MedPC)
  *(Documentation coming soon — page will include parsing code and examples for IV self-administration paradigms that do not use Med-PC equipment)*

---

## About MedParser

MedParser is available on CRAN:  
https://CRAN.R-project.org/package=medparser

It provides standardized functions for extracting text from any .txt file. We use these functions to generate:
- Session-level metrics
- Time series data
- Cumulative records
- Behavioral summaries

---

## Citation & Contributions

If you use MedParser in your work, please cite the relevant publications listed in each paradigm page. Contributions and suggestions are welcome via GitHub Issues or Pull Requests.

---

## Getting Started

To begin, visit the single_reinforcer.md page or explore other paradigms above.
