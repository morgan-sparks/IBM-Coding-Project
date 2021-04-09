# Model outline

## Question
How quickly do fish invade Lake Superior?

## Parameters

* Age at Maturity
* Habitat structure

## Functions

1. Births (this will be for actually creating babies), use gamma distribution (0.5,0.5)
    * track parameters from parents --> age at maturity, spawn river
2. Mortality (not added yet)
3. (a) Age at maturity == age (if yes go to step 4)
3. (b) Age +1
4. Migrate to home river or stray
5. Density dependence (e.g., how many spawning spots available in a river, kill if > K)
6. Pair up adults of different sexes to spawn

## Data strucutre
1. Lake --> one dataframe that doesn't maintain info from past gens
2. census --> an array that stores lake each year as a seperate element
3. summary --> sum stats to be computed after model on census