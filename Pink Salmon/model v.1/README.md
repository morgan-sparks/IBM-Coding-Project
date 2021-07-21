# Model outline

## Question
How quickly do fish invade Lake Superior?
What is the fitness of different aged fish (also those that strayed vs. those that stayed in river)

## Parameters

* Age at Maturity
* Habitat structure

## Populations

There seems like there could be many (>60), but it's not clear how many and their relative population size.

For now let's do 50 pops:

5 with k = 50,000

10 with k = 20,000

15 with k = 5,0000

20 with k = 1,500


*Relevant citations:*

South shore Lake Superior: Wagner and Stauffer 1982, ~50 rivers they think had populations 

North Shore Superior: Kwain 1982, ~ 8 rivers in this citation 

Two Minnesota tributaries: Nicolette and Spangler 1986, Cascade River (N_hat = 1250 +/- 300 95%CI) and Cross River (N_hat = 2000 +/- 100) 

## Functions

1. Births (this will be for actually creating babies), use gamma distribution (0.5,0.5)
    * track parameters from parents --> age at maturity, spawn river
2. Mortality (not added yet)
3. (a) Age at maturity == age (if yes go to step 4)
3. (b) Age +1
4. Migrate to home river or stray
5. Density dependence (e.g., how many spawning spots available in a river, kill if > K)
6. Pair up adults of different sexes to spawn

-- [x] somewhere in here we need to add fitness calculation

## Data strucutre
1. Lake --> one dataframe that doesn't maintain info from past gens
2. census --> an array that stores lake each year as a seperate element
3. summary --> sum stats to be computed after model on census

------------------------------------------------------------------------
## To do

- [x] set up data structure (Mark did a big array-based model and did 20 pops at k=12000 each and it sounded real big so maybe we limit around there, though maybe had juvenile pops up to 800000)---> maybe realistic for us is 50 pops with a few big pops and a lot of intermediate to small pops
   - [x] one big lake with multiple rivers where habitat changes to lake and back based on maturity
   - [ ] need to set up some redundancy where we can click on and off functions (i.e. age = 1 or 2 at beginning of script to turn off and on life history variation)
- [x] figure out sourcing and how to set up (should be source script that points to source folder with functions as individual scripts, and then an output folder)
- [x] will straying be a matrix with likelihoods built in?

## Questions for the group
 - [ ] How do we want to handle dead individuals (delete when they die vs. add death column and then delete at end of year after appended to census dataframe)
 
 ------------------------------------------------------------------------
 
 ##Fitness phenotypes
 1. 2 yr old
 2. 3 yr old
 3. 2 yr no-stray
 4. 2 yr stray
 5. 3 yr no-stray
 6. 3 yr stray
 
