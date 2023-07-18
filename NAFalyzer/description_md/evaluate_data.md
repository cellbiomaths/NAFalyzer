### Is my data good?

<br />

Not all NAF gradients are created equal. Some are better than others and will this lead to a more confident result. 
In general, your NAF should fulfill the following:

- Compartments should have distinct peeks in differing fractions
- Compartments should have a differing relative distribution across at least several fractions.

You should, however, **always** evaluate your data. 

---

#### How to evaluate your data
When using the Monte Carlo simulation, violin plots of the resulting relative distribution can be viewed under *Results > Violin* plots. This is the main tool we will use to judge the performance. 

If the algorithm is preforming well, i.e. the gradients allow for good separation, the plot will have a distinct wide belly. On the other hand, the worse the separation is, the skinnier the belly. In the example bellow, several gradients where used to demonstrate this point. The expected distribution for the metabolite is 30%, 50% and 20%. As can be seen, the results get worse, when the guidelines mentioned above aren't fulfilled. 

*Note: The shape of an ideal NAF my differ on your monitor from what is shown, e.g. being more streched or compressed. Importent is the relation between width and the overall height of the plot aswell as the general shape, e.g. how manny peaks are present, how shmooth is the cureve etc.*

<img src="figures/Comparing_gradients.png" alt="alt text" width="800"/>

How differing gradients effect results

<br />

In this example, **A)** and **B)** fulfill both points and can be confidently used. On the other hand, compartments in **C)** don't have distinct peeks and thus the resolution isn't as clear. This results in the second compartment being underestimated as the algorithm can't distinguish between compartment 2 and 3. This effect is even worse in **D)** where none of the guidelines are fulfilled.