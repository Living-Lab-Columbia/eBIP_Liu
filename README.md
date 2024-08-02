# Battery for Imagery and Perception (eBIP) (Liu et al., 2024)

## Introduction

This repository contains the code for the experiments and data analysis of the paper "Characterizing the Domains of Visual Mental Imagery: " by Liu et al. (2024).


## Requirements

The code is written in Python 3.12. The required packages are listed in the `requirements.txt` file. You can install them by running:

```bash
pip install -r requirements.txt
```

## Experiment Code
The experiment is implemented using PsychoPy. The code for the experiment is in the `experiment` folder.

## Analysis Code
The code for oue data analysis is in the `analysis` folder. It's split into different files, each corresponding to a different analysis. 

- `analysis_vivd_vviq.ipynb`: Analysis of the relationship between the VVIQ and Vividness and their relationships with imagery performance.
- `analysis_correlation.ipynb`: Correlation between the imagery and perception tasks, as well as across different domains.
- `analysis_logistic.ipynb`: Logistic regression models for predicting modalities(imagery/perception) from performance.
- `analysis_clustering.ipynb`: Clustering analysis of participants performance across Imagery/Perception and different domains.
- `anova_viz.R`: Performs two-way ANOVA for RT/Accuracy based on Domain and Modality, and visualizes the results.
- `corr_pair_test.R`: Follow-up to the correlation analysis, testing the whether the correlations are significantly different from each other.
- `fixed_effect_model.R`: Fixed effect model for predicting performance based on trial-by-trial confidence/vividness ratings.
