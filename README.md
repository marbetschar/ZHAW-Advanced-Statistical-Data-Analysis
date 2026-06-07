# Advanced Statistical Data Analysis – Complete Learning Objectives

This document summarizes **all Learning Objectives** for the **Advanced Statistical Data Analysis** course (ZHAW, MSE Program), organized by course part and session.

**Note:** Some weeks have explicit "Learning Objectives" sections, while others have implicit objectives derived from their main topics and content descriptions.

---

## 📊 Part I: Advanced Regression Modelling
*Lecturer: Andreas Ruckstuhl | Weeks 1-8*

### Week 1 – Review of Multiple Linear Regression (I)
*Implicit objectives from course introduction:*
- Introduces various problems that can be solved using (advanced) regression modelling
- Explains the models and inference methods appropriate for common cases
- Familiarises you with the statistical modelling approach
- Introduces the parts of R that help to solve regression problems

### Week 2 – Review of Linear Regression Modelling (II)
*Topics covered (implicit objectives):*
- AIC for variable selection
- Handling categorical predictors
- Multicollinearity detection (using VIF)
- Model validation techniques (cross-validation, PRESS)
- Prediction intervals
- Interpolation vs. extrapolation
- Distinction between predictive and causal modeling

### Week 3 – Advanced Topics in Linear Regression Modelling
*Topics covered (implicit objectives):*
- Weighted Least Squares (for non-constant variance)
- Robust Fitting (to handle outliers and leverage points)
- Fitting Smooth Functions (using LOESS and splines)
- Additive Regression Models
- Model Building strategies (residual analysis, transformations, variable selection, validation)

### Week 4 – Binary Regression (Logistic Regression)
- You know the Logistic Regression Model and its applications
- You know how to fit a Logistic Regression in R
- You know how to interpret the parameter of a Logistic Regression

### Week 5 – GLM: A Unifying Model Family
- You can identify members of the exponential family
- You know the two basic elements that define the GLM
- You know how to fit GLMs in R and know the algorithm underlying it
- You can interpret R output of a GLM fit
- You know the exponential family and some of its members
- You know the general structure of GLMs
- You can fit a Poisson and Gamma regression with R
- You will know the principle behind the fitting process and can interpret the summary output, part "Coefficients"

### Week 6 – Inference, Model Simplification, Variable Selection
- You know what deviances are in GLM
- You know what overdispersion is and can identify it
- You know when you can apply Wald-type confidence intervals and when it is better to use deviance-based confidence intervals
- You can apply the introduced methods in statistical data analysis using R

### Week 7 – Diagnostics / Model Adequacy Checking and Model Improvement
- You can understand how AIC is generalised to GLMs
- You can check the model adequacy and determine which assumptions, if any, are violated
- You can find appropriate transformations of predictors in a data-driven manner
- You can apply the introduced methods in statistical data analysis using R

### Week 8 – Some Extensions of GLM
- You know what a rate model is and how you can analyse it with GLM
- You know how a quasi model extends a GLM and when you can apply it
- You can fit these methods to data using R, interpret the results, make inference statements, and predictions

---

## 🔍 Part II: Causality
*Lecturer: Anna Drewek | Weeks 9-14 (Slides 01-06)*

### Causality 1 – Introduction (Slides 01)
- You understand **when and why causal reasoning is important**
- You are familiar with **causal graphical models**
- You understand the difference between **association, interventions, and counterfactuals**
- You know the difference between **experimental and observational data**

### Causality 2 – Simpson's Paradox & Graphical Models (Slides 02)
- You understand **Simpson's paradox and how to analyze it**
- You can express the **joint distribution by factorization** and calculate conditional distributions for a given graph
- You understand the concept of **(conditional) independence**
- You can identify **(conditional) independences for chain, fork, and collider** structures

### Causality 3 – D-Separation & Causal Effect Estimation (Slides 03)
- You know **D-Separation**
- You can estimate the **causal effect by using the adjustment formula**
- You can define **adjustment sets with the backdoor criterion**

### Causality 4 – Structural Causal Models (Slides 04)
- You are familiar with **(linear) structural causal models**
- You understand the concept of **direct and total causal effects**
- You can estimate the **direct and total causal effect** from a given linear structural causal model using regression

### Causality 5 – Advanced Topics (Slides 05)
- You understand the concept of **instrumental variables**
- You are familiar with **counterfactual reasoning**
- You know the **three steps in computing counterfactuals** (abduction, action, prediction) and can apply them to SCMs
- You understand the concept of **Markov Equivalence**

### Causality 6 – Causal Structure Learning (Slides 06)
- You know the **steps of PC algorithm and LiNGAM algorithm**
- You can perform **causal structure learning with PC and LiNGAM algorithm in R**
- You know the **4 steps of causal inference**:
  1. Create a causal model using expert knowledge (DAG)
  2. Identify whether and how the causal effect can be identified from observational data
  3. Estimate the causal effect from the data
  4. Test the estimated causal effect (validity)

---

**Source:** Advanced Statistical Data Analysis Library (ZHAW, MSE Program, SPR 2026)