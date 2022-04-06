# NOT-SO-RANDOM NOTES:

-  the existence of isolated nodes is not considered,
that is, I'm taking as {V u E} the elements of the rho file.

- about returning the Val type on the query functions,
I have implemented the Val type in such a way that when printing (show function) the result of the queries,
it will be displayed correctly.

- by requirement, populate has to use IO, 
this forces us to return IO PG in populate, slightly deviating from the specification demands

- overall, some slight deviations from the practice task demands have been implemented,
as the specification is somewhat loose and teachers have pointed out it should be treated
as a non-strict guideline

- I have forced myself to use first-order functions in several algorithms, so the program has a more functional tune,
there is also a heave use of comprehension lists, as I like to think in terms of those

- the Date type is treated as a String

- a (small) concession has been taken on the 120-characters long code lines so I could add some ASCII art

- the practice task description asked that 'For each query evaluated users must enter the real parameters and the result must print.'
but after a conversation with the responsible teacher, this requirement was dropped as long as we generated an informative test
        
- some typos were corrected from the given test files, so if you want to replicate the test results,
PLEASE EXECUTE THE SCRIPT WITH THE PROVIDED TEST FILES
