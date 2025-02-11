# ⚽ Analyzing Football Players' Salaries: A Statistical Approach

## 📌 Overview
This project explores **European football players' salaries** through a **statistical modeling approach**, aiming to develop a predictive model based on **performance metrics, age, and club affiliation**. The study excludes external factors like sponsorships and media influence to create a **meritocratic salary estimation model**.

The project includes:
- **Exploratory data analysis** on player salaries.
- **Preprocessing & feature engineering** (data cleaning, missing values, variable selection).
- **Multiple linear regression models**, including:
  - **Stepwise regression** for optimal variable selection.
  - **Principal Component Regression (PCR)** for dimensionality reduction.
- **Identification of overpaid and underpaid players**.

## 📂 Project Setup
### Data
- **Seasons Analyzed:** 2022/2023
- **Datasets Used:**
  - **Capology:** Verified salary data for 2,891 players.
  - **FBref:** Performance statistics across top **five European leagues**.
- **Key Variables:**
  - **Demographics:** Age, nationality, club, position.
  - **Performance Metrics:** Goals, assists, passes, tackles, expected goals (xG), progressive carries.
  - **Club Financial Strength:** Categorized into **10 salary budget groups**.

### Tools & Technologies
- **R** for data analysis and modeling
- **Packages Used:** `ggplot2`, `dplyr`, `glmnet`, `pls`, `Metrics`, `stargazer`
- **Data Preprocessing:** Handling missing values, currency conversion, and feature scaling.

## 🔍 Methodology

### 1️⃣ Data Processing
- Combined **salary data** and **performance statistics**.
- **Cleaned missing values** and ensured **currency uniformity (converted to EUR)**.
- **Grouped teams into 10 tiers** based on wage budgets.

### 2️⃣ Regression Models
#### **📈 Multiple Linear Regression**
- Estimated a **baseline model** with **86 variables**.
- Identified **overfitting risks** due to high dimensionality.
- Conducted **diagnostic tests** (normality, homoscedasticity, residual analysis).

#### **🛠 Stepwise Regression (Backward Selection)**
- Removed **irrelevant predictors** using **Akaike Information Criterion (AIC)**.
- Improved **Adjusted R²** and **reduced overfitting**.

#### **🧩 Principal Component Regression (PCR)**
- Applied **Principal Component Analysis (PCA)** to reduce dimensionality.
- **31 principal components** selected to optimize predictive accuracy.

### 3️⃣ Model Evaluation
- Compared models using:
  - **Mean Squared Error (MSE)**
  - **R² and Adjusted R²**
  - **Residual & influence diagnostics**
- **Final Model Selection:** **PCR** due to better interpretability & lower complexity.

## 🏆 Key Findings

| Model | Adjusted R² | MSE (Test Set) |
|----------|--------------|------------------|
| **Stepwise Regression** | 0.632 | 1.25 × 10¹³ |
| **Principal Component Regression (PCR)** | 0.642 | **1.12 × 10¹³** |

- **PCR performed slightly better**, reducing **dimensionality from 86 to 31 features**.
- **Salary is strongly correlated with:**  
  - **Age (+53% per year)**  
  - **Club prestige (Top-tier clubs offer +130% higher salaries)**  
  - **Performance metrics (Goals, Assists, Passes, Progressive Carries).**  
- **Surprising findings:**  
  - **Defenders earn +2.6% more than other positions** (after controlling for other factors).  
  - **Strikers, despite scoring goals, had -3.5% lower salaries than expected.**  
  - **Short passes are highly rewarded (+7% salary increase per extra pass), while long passes reduce earnings (-4%).**

## 🏅 Most Underpaid Players (According to Model)
| Player | Club Group | Underpaid by (€) |
|----------|-------------|-----------------|
| **Karim Benzema** | 10 | **+€29.9M** |
| Riyad Mahrez | 7 | +€22.0M |
| Thiago Silva | 7 | +€20.1M |
| Mohamed Salah | 6 | +€17.3M |
| Rodri | 7 | +€17.0M |

## 💰 Most Overpaid Players (According to Model)
| Player | Club Group | Overpaid by (€) |
|----------|-------------|-----------------|
| **Kylian Mbappé** | 10 | **-€57.8M** |
| Neymar | 10 | -€43.0M |
| Frenkie de Jong | 9 | -€33.2M |
| Eden Hazard | 10 | -€22.7M |
| Gerard Piqué | 9 | -€18.0M |

## 📌 Conclusions
- **Club prestige is the most influential factor** in salary determination.
- **Player performance impacts wages but less than expected**, highlighting **market inefficiencies**.
- **The model could help standardize salaries**, reducing reliance on agents and negotiations.
- **Potential implementation of a salary cap** could promote fairness, similar to the **NFL system**.

## 📬 Contact
For inquiries or collaboration, feel free to reach out:
- **Email:** [lollordp@gmail.com](mailto:lollordp@gmail.com)
- **LinkedIn:** [Lorenzo Rossi](https://www.linkedin.com/in/lorenzo-rossi01/)
