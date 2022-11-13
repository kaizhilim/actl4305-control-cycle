# actl4305-control-cycle

<div align="center">
  <img src="cycle.png">
</div>

<!-- TABLE OF CONTENTS -->
<details open>
  <summary style = "font-size:13pt;">Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
    </li>
    <li>
      <a href="#repository-directory">Repository Directory</a>
    </li>
    <li>
      <a href="#pre-processing"> Preprocessing</a></li>
      <ol>
        <li><a href="#data-cleaning"> Data Cleaning</a></li>
        <li><a href="#data-preparation"> Data Preparation</a></li>
        <li><a href="#exploratory-data-analysis"> Exploratory Data Analysis (EDA)</a></li>
      </ol>
    <li>
      <a href="#modelling">Modelling</a>
      <ol>
        <li><a href="#cross-validation-approach"> Cross Validation Approach</a></li>
        <li><a href="#glm-ensemble-model"> GLM Ensemble Model</a></li>
        <li><a href="#key-variables"> Key Variables</a></li>
      </ol>
    </li>
    <li>
      <a href="#recommendations-and-limitations​">Recommendations & Limitations</a>
    </li>
  </ol>
</details>

## About the Project
>The objective of this project is to **predict the pure premium of IAG’s property owner package product as a mathematical combination of the key rating factors**. 

The models will account for interactions and associations between Property and Business Interruption covers, balancing model accuracy and the business constraint of interpretability.​
## Repository Directory

>Check out the interactive map of property claims by SA4 (open & run in browser): [Interactive Map of Property Claims](02%20EDA/01%20Map%20By%20SA4/PropertyClaimsSA4.html)

The best way to explore the repository is to start with the [`main.R`](main.R) file. The main script that runs the entire project. The repository is structured as follows:

1. [`01 Data Cleaning`](01%20Data%20Cleaning) - This folder contains the scripts that clean the data. Description of the data cleaning process can be found in the section [Data Cleaning](#data-cleaning).:

    - [`Data Cleaning Interface.R`](01%20Data%20Cleaning/Data%20Cleaning%20Interface.R) - Assuming the [assignment data](Assignment%20Data/Assignment%20Data.csv) is loaded, this interface script calls the other scripts in the folder. 

2. [`02 EDA`](02%20EDA) - This folder contains the scripts that perform exploratory data analysis (EDA). Description of the EDA process can be found in the section [Exploratory Data Analysis (EDA)](#exploratory-data-analysis).

   - [`01 Map By SA4`](02%20EDA/01%20Map%20By%20SA4) - This folder contains the scripts that create the interactive map of property claims by SA4. The map can be found [here](02%20EDA/01%20Map%20By%20SA4/PropertyClaimsSA4.html).

    - [`EDA.Rmd`](02%20EDA/EDA.Rmd) - R Markdown that performs EDA on the data. The output of this script is a report that can be found [here](02%20EDA/EDA.html).

3. [`03 Preparation`](03%20Preparation) - This folder contains the scripts that prepare the data for modelling. Description of the data preparation process can be found in the section [Data Preparation](#data-preparation).

4. [`04 Modelling`](04%20Modelling) - This folder contains the scripts that perform modelling, specifically for the models GLM, Tweedie, Neural Networks and Random Forests. Detailed outline of the process can be found in [Modelling](#modelling).

    - Read the [Cross Validation Approach](#cross-validation-approach), executed using [`Data Split.R`](03%20Preparation/02%20Data%20Split.R)

    - [`GLM Ensemble.R`](04%20Modelling/01%20GLM/GLM%20Ensemble.R) - This script performs the GLM Ensemble model. Read the [GLM Ensemble Model](#glm-ensemble-model) section for more details.

    - [`Tweedie.R`](04%20Modelling/02%20Tweedie/Tweedie.R) - Tweedie model.

    - [`Neural Networks.ipynb`](04%20Modelling/04%20Neural%20Network/Neural%20Network.ipynb) - Neural Networks model.

    - [`Random Forests.R`](04%20Modelling/05%20RF%20%26%20Decision%20Tree/RandomForest.R) - Random Forests model.

5. [`05 geo_code modelling`](05%20geo_code%20modelling) - This folder contains the scripts that perform modelling on the geo_code variable. Unfortunately, the encoding in this folder was not used in the final model.

    - [`run_geo_code.R`](05%20geo_code%20modelling/run_geo_code.R) - This script performs the modelling using the geo_code variable.


## Pre-Processing
### Data Cleaning

<figure align="center">
  <img src="06 Model Output Images/Missing Value Plot.jpg" alt="missing-values "/>
  <figcaption><i>Fig 1: Missing Values in Data</i></figcaption>
</figure>

Under the epicycle of analysis, the data was cleaned to ensure it met the expectations. As seen in Figure 1, missing values occurred together and it was interpretted that it was purely due to either, not having a business interruption coverage, or that the details associated with the property was not collected. Treating these values as *'unknowns'* easily resolved this issue.

<figure align="center">
  <img src="06 Model Output Images/Weight Curve.jpeg" alt="date-weights "/>
  <figcaption><i>Fig 2: Exponential Decay Weights</i></figcaption>
</figure>

Exponentially decaying weights were applied to weigh recent policies over older periods.

### Data Preparation

The data was then prepared for modelling under a relational database framework by grouping the data based on each unique policy id and coverage period.

![geocode-dim](06%20Model%20Output%20Images/Geo%20Code%20Dimension%20Reduction.jpg)

To achieve a balance of granularity and dimension reduction,postcodes were also grouped into [Australian Statistical Geography Standard (ASGS)](https://www.abs.gov.au/statistics/statistical-geography/australian-statistical-geography-standard-asgs) areas, if they had policy exposures below an optimised threshold. 

![geocode-demo](06%20Model%20Output%20Images/Concentration%20of%20Poor%20Claim%20Experience.jpg)

For example, [Prairiewood](https://www.abs.gov.au/census/find-census-data/quickstats/2016/SSC13258) which had only 9 policy exposures was combined with the Statistical Area 3, [Fairfield](https://www.abs.gov.au/census/find-census-data/quickstats/2016/12702), from which patterns emerged which showed that Fairfield had higher than claim frequency and sizes than the average of NSW. 

### Exploratory Data Analysis

![eda-interaction](02%20EDA/09%20Interaction/GrossIncurred.png)

EDA was then carried out, revealing features which had strong trends and relationships with the claim experience. For example, a comparison of the claim size distribution revealed that when both claims were made in conjunction, the policy was more likely to make a larger claim. ​

## Modelling

### Cross Validation Approach

![Model-Comparison](06%20Model%20Output%20Images/Model%20Comparison.png)

After conducting our exploratory analysis, we assessed a range of different models in order to determine which performed the best in achieving the target objectives. To determine the pure premium, frequency and severity were modelled separately. Each model was trained and validated using a 10-fold cross-validation process; with a 80/20 split for training and test data respectively. 

Models tested included [GLMs](04%20Modelling/01%20GLM/GLM%20Ensemble.R), [Tweedie](04%20Modelling/02%20Tweedie/Tweedie.R), [Neural Networks](04%20Modelling/04%20Neural%20Network/Neural%20Network.ipynb), [Random Forests](04%20Modelling/05%20RF%20&%20Decision%20Tree/RandomForest.R) and [Decision Trees](04%20Modelling/05%20RF%20&%20Decision%20Tree/DecisionTree.R). These models were evaluated and compared using Root Mean Squared Error and aimed to balance predictive accuracy, whilst prioritising interpretability, to provide useful insights for IAG’s commercial decisions. ​

### GLM Ensemble Model

![glm-ensemble](06%20Model%20Output%20Images/GLM%20Ensemble%20Outline.jpg)

Our final chosen model was a GLM ensemble model to achieve a balance between accuracy and interpretability for IAG’s pricing process. This model involved predicting property and business interruption lines separately, and capturing associations between coverages via a frequency, severity and aggregate loss interaction term. The incorporation of the interaction effect effectively reduced test errors by 2%, demonstrating the importance of allowing for interactions in the models to prevent underpricing which would be unsustainable in the long-term.​

### Key Variables

The model identified different key variables in line with our findings in our exploratory data analysis:

#### Property Claims

![VIP-Prop-Freq](06%20Model%20Output%20Images/VIP%20Prop%20Freq.png)

Key variables for property claims included high-risk areas concentrated in coastal Queensland and South West NSW, low-rise buildings prone to natural disasters and occupations in infrastructures. 

#### Business Interruption Claims

![VIP-LoI-Freq](06%20Model%20Output%20Images/VIP%20LoI%20Freq.png)

Business interruption lines were highly dependent on occupations at risk of workplace accidents, primarily in the retail and energy sector, but also shared commonalities in significant variables with property claims. Ultimately, these variable importance factors can be translated into rating factors for IAG’s commercial pricing.​

## Recommendations and Limitations​

Three recommendations are provided for IAG. Firstly, IAG should leverage this competitive advantage by more accurately reserving for future claims, thus increasing revenue via adverse selection. Secondly, IAG should incorporate the rating tables provided by our GLM model to increase sales. These rating tables provide greater clarity to internal and external stakeholders. Thirdly, IAG should consider other associations between products to leverage the aforementioned benefits and provide widespread industry impact. ​

​
Two limitations are noted with proposed solutions provided. Firstly, the scope of the data is extremely limited, which increases the likelihood of overfitting. IAG’s data can be supplemented with [APRA’s General Insurance Claim Development Statistics](https://www.apra.gov.au/general-insurance-claims-development-statistics), and further consultations with IAG’s pricing teams can help address specific data issues. Secondly, we note the accuracy of our model is less than ideal as shown in our RMSE values, hence consider utilising other models. ​