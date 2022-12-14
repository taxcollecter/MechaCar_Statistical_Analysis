# MechaCar_Statistical_Analysis


## Linear Regression to Predict MPG
![MPG Analysis](https://github.com/taxcollecter/MechaCar_Statistical_Analysis/blob/72bacb7f5d1c2fd8140ad587b97e28879dcbd969/Resources/Deliverable1.png)

-- Reviewing the output of the linear regression and the variables provided, we can see that Spoiler angle, AWD, and Vehicle Weight all provide a "non-random" amount of variance to the mpg values in the dataset.

-- Based upon our p-value, the slope of our linear model is not zero. The value is much smaller than our assumed significance level of 0.05% 

--While the model helps identify how the variables impact mpg, I do not believe it predicts mpg effectively. I believe there are other factors that will need to considered before we can confidently assess (Engine specs, fuel grade, etc.)

## Summary Statistics on Suspension Coils
![PSI Summary](https://github.com/taxcollecter/MechaCar_Statistical_Analysis/blob/30482ecf3870b2e812732748054145e5187228ad/Resources/Deliverable2Summary.png)

![PSI by Lot](https://github.com/taxcollecter/MechaCar_Statistical_Analysis/blob/e49cd2d85f888e88d1ea2c91ee54e08b1b2cad6a/Resources/Deliverable2Lot.png)

--Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually?
No, the current manufacturing lots exceed the 100 PSI threshold in total and Lot 3 exceeds the threshold individually. 

## T-Tests on Suspension Coils
![Lot1 T-Test](https://github.com/taxcollecter/MechaCar_Statistical_Analysis/blob/7d33fc78fc07f0ef590ca1a69861a0c3f311d9ac/Resources/TestTestLot1.png)

![Lot2 T-Test](https://github.com/taxcollecter/MechaCar_Statistical_Analysis/blob/7d33fc78fc07f0ef590ca1a69861a0c3f311d9ac/Resources/TestTestLot2.png)

![Lot3 T-Test](https://github.com/taxcollecter/MechaCar_Statistical_Analysis/blob/7d33fc78fc07f0ef590ca1a69861a0c3f311d9ac/Resources/TestTestLot3.png)

In reviewing the output of the test by Lot, we can reject Lot 1 and Lot 2 per our hypothesis as their p-values are less than the .05 significance threshold. Lot 3 is not rejected as it exceeds the .05 threshold. 

## Study Design: MechaCar vs Competition
In our next review/study we'll review the Fuel perfomance, via MPG, of the MechaCar vs the Competition. In this study we'll leverage a Two-Sample t-Test to compare the MPG of two comprable cars with similar Weight, Height, Make and Structure. We'll review multiple instances of each car to determine if the mean of the MPG statistically differ and settle on which car is performing better. We assume the MechaCar will perform better due to the effiencies that have been implemented recently. We'll use Gas and gas consumed around a standard track to confirm our findings. 
