## Contact Information:
#### LinkedIn: www.linkedin.com/in/sarahvastani

#### Email: sarahvastani@gmail.com

# Research Projects and Work

## Project 1: AlphaPullDown: AI-Driven Modeling of Microbial Protein–Protein Interactions

### Folder Contents:
* bait_single.fasta → SanA bait sequence
* ecoli_candidates.fasta → prey sequences used in predictions
* pkl_files_link → ecoli_pkl_json.tar.gz containing feature .pkl + metadata .json archives
* pdb_files_link → ecoli_pdb.tar.gz containing ranked PDB structures for SanA complexes
* alphapulldown_modified_bundle(.zip) → patched Python scripts + SLURM wrappers
* predictions.csv → table containing confidence scores of predicted structures

### Description:
In this project, I used AlphaPulldown, a high‑throughput interface to AlphaFold‑Multimer, to predict potential protein–protein interactions in Escherichia coli. The goal was to generate structural models for candidate interaction partners, evaluate their plausibility, and integrate biological context to identify the most credible interaction hypotheses. This work supports downstream experimental validation and contributes to understanding functional relationships within bacterial systems.

The project involved preparing curated sequence inputs, generating AlphaFold feature dictionaries, running large‑scale multimer predictions on a computing cluster, and organizing the resulting structural models. To ensure biological relevance, each predicted interaction was evaluated using compartment localization, membrane topology, and functional annotations. Large model archives (PDBs, PKLs, JSONs) are hosted externally due to GitHub file‑size limits.

### Key Aspects of the Project:
#### High‑Throughput Structure Prediction:

* Used AlphaPulldown to automate feature generation and multimer model prediction across many candidate proteins.

* Ensured reproducibility through standardized SLURM job scripts and consistent sequence/MSA handling.

#### Feature Generation and Model Outputs

* Created AlphaFold feature dictionaries (.pkl, .json) and multimer structural models (.pdb) for each candidate pair.

* Organized outputs into compressed archives for efficient storage and sharing.

#### Biological Filtering and Interpretation

* Assessed predicted interactions using cellular compartment data, membrane topology predictions, and functional annotations.

* Filtered out structurally plausible but biologically incompatible interactions.

#### Reproducibility and Documentation
* Included workflow notes, command logs, and reasoning steps to ensure transparency and repeatability.

* Provided external links to large datasets for accessibility.

## Project 2: Predicting Migration Survival in Swainson’s Thrushes Using Machine Learning
### Folder Contents:
* Slides
* Report
* Code
* Dataset

### Description:
In this project, I used machine learning (ML) to explore how diverse traits in Swainson’s thrush hybrids—particularly those related to migration—affect their survival during migration. The study focused on two subspecies of Swainson’s thrushes, which form a migratory divide in the Coast Mountains of western North America. By analyzing a range of behavioral, morphological, and genetic traits, the project aimed to identify how these traits interact to determine survival, thus providing insight into the processes of ecological speciation.

The primary goal of this study was to leverage the power of machine learning techniques to detect complex, non-linear, and interactive relationships between various traits and survival outcomes. This study utilized two machine learning models: Random Forest and Neural Networks. The project included data collection through fieldwork (capturing birds, taking measurements, and collecting genetic samples), followed by feature selection and model training. The results of these models help understand the fitness landscape of Swainson’s thrush hybrids and provide insight into post-zygotic isolation mechanisms.

### Key Aspects of the Project:
#### Machine Learning Models:

* **Random Forest:** An ensemble method used to construct multiple decision trees and predict survival outcomes based on trait data.

* **Neural Networks:** Multi-layer perceptron (MLP) used to capture complex interactions between traits, leveraging hierarchical data representations to predict survival.

#### Feature Selection and Preprocessing:

* Phenotypic and genotypic features were included, such as body measurements, migratory behavior, and genetic ancestry.

* Missing data was handled using K-Nearest Neighbors (KNN) imputation, and class imbalance was addressed using the Synthetic Minority Over-sampling Technique (SMOTE).

#### Data Collection:

* Data was collected through the capture and tagging of juvenile Swainson’s thrushes, with tracking provided via automated radio telemetry and genetic analysis.

* The dataset included 479 birds, with a focus on survival tracking during migration.

#### Evaluation Metrics:

* Model performance was evaluated using accuracy, confusion matrices, and feature importance scores.

* The models identified key traits, such as norm_kipps, norm_bodyCondition, and norm_tarsus.length, as the most influential predictors of survival.

#### Ecological Implications:

* The project provides insights into how traits associated with migration influence survival, offering a better understanding of ecological speciation in hybrid zones.

* The use of ML to connect trait variation with fitness opens new avenues for studying post-zygotic isolation in migratory species.

## Project 3: Circadian Rhythms in Lumbriculus variegatus
### Folder Contents:
* Poster
* Report

### Description:
This project focused on investigating circadian rhythm mechanisms in Lumbriculus variegatus, an aquatic annelid worm. The aim was to examine the expression of the Timeless (TIM) protein, which plays a crucial role in regulating circadian rhythms in many organisms. The study involved performing immunohistochemical staining using TIM and 5HT antibodies across six time points in a 24-hour light-dark cycle, using 50 samples. The research aimed to uncover whether TIM protein expression follows a rhythmic pattern, and to identify its localization within specific tissues, particularly the brain and ventral nerve cord neurons. Additionally, microsurgical dissection techniques were employed, including dorsal head amputations to study tissue-specific protein expression.

### Key Aspects of the Project:
* **Objective:** To study the expression and rhythmicity of the TIM protein in Lumbriculus variegatus, contributing to circadian rhythm research.

* **Methodology:** Performed immunohistochemical staining at six time points across a 24-hour period, using TIM and 5HT antibodies to detect protein expression.

* **Experimental Design:** The worms were entrained in a light-dark cycle, and tissues were fixed at multiple time points for fluorescence microscopy.

* **Microsurgical Techniques:** Included precise dorsal head amputations to study the nervous system and underlying body structures.

* **Results:** The research successfully detected TIM protein expression in the ventral nerve cord, providing evidence of rhythmic expression that could inform future studies on circadian clock mechanisms.

* **Challenges:** There were ambiguities regarding the precise role of TIM, including potential evolutionary functions beyond circadian regulation, such as involvement in development or regeneration, which may be investigated in future studies.

* **Collaboration:** Worked closely with a lab partner and presented findings at an undergraduate research poster session and competition.

## Project 4: Graduate Defense - Turing Instability and Biological Applications
### Folder Contents:
* Slides

### Description:
This project explores Turing instability and its role in biological pattern formation through the lens of reaction-diffusion systems. Inspired by Alan Turing’s 1952 paper "The Chemical Basis of Morphogenesis," the presentation investigates how spontaneous patterns—such as animal stripes and spots—can arise from simple chemical interactions governed by partial differential equations. This project was developed as part of a graduate-level course on Ordinary and Partial Differential Equations and integrates foundational concepts from both biology and mathematics.

I begin by introducing the biological motivation for the study of pattern formation and Turing's groundbreaking idea that patterns can emerge from interactions between diffusing and reacting chemical species. I then walk through the mathematical formulation of reaction-diffusion equations, specifically the Gray-Scott model, and show how the stability of spatially uniform steady states can change when diffusion is introduced. Using linearization and eigenvalue analysis, I demonstrate how diffusion can destabilize a system that is otherwise stable—an essential condition for Turing pattern formation.

### Key Aspects of the Project:
* **Interdisciplinary Focus:** Combines biological questions (e.g. how spots and stripes form) with rigorous mathematical modeling.

* **Reaction-Diffusion Equations:** Introduces and explains how partial differential equations govern the diffusion and reaction of chemicals in space and time.

* **Gray-Scott Model:** A simplified two-species model is used to demonstrate how activator-inhibitor dynamics can lead to pattern formation.

* **Turing Instability:** The core concept that diffusion—normally a stabilizing force—can destabilize a steady state in the presence of certain reaction kinetics, giving rise to patterns.

* **Mathematical Techniques:**

  * Linearization around steady states.

  * Jacobian matrix and eigenvalue analysis.

  * Use of trace and determinant to determine stability.

  * Taylor expansion to derive the linearized system.

* **Biological Interpretation:** Turing’s theory is applied to explain natural phenomena, such as animal coat patterns and morphogenesis during development.

* **Personal Context:** Reflects the intersection of my background in biology and current mathematical training, illustrating how math can enhance understanding of biological complexity.
