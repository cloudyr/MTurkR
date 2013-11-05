---
layout: default
title: Batch Features
ghurl: https://github.com/leeper/MTurkR/blob/gh-pages
---

# Mimicking the Requester UI Batch Features #

**MTurkR** supplies many features that the Requester UI does not offer. But the Requester UI also has a few features that are hard to emulate in R. This page describes some techniques for mimicking a couple of those features.

---
## HIT Layout Parameter Inputs and Outputs ##

Through HITLayout parameters, **MTurkR** can mimick the batch HIT creation features of the Requester UI. But, because the MTurk application does not preserve HITLayout inputs (at least not in a way that is accessible via the API), it is not straightforward to match HITLayout parameter input values to the assignment results of HITs.

We can mimic this using the following workflow:

 1. Store the HITLayout parameters in a dataframe.
 2. Create each HIT with the HITLayoutID and HITLayout parameters and then store the HITId (from `CreateHIT`) for each new HIT into that dataframe.
 3. Save the dataframe locally.
 4. After retrieving assignments with `GetAssignments`, reload your original dataframe.
 5. Then, merge your assignment dataframe with the original dataframe to combine input values and results.

In **MTurkR**, we could obtain this as follows:

```r
# first load credentials with `credentials()`
# create a dataframe of HITLayout parameters:
inputvalues <- 
data.frame( hitvar1=c("Input for HIT 1 for var1","Input for HIT 2 for var1","Input for HIT 3 for var1"),
            hitvar2=c("Input for HIT 1 for var2","Input for HIT 2 for var2","Input for HIT 3 for var2"),
            hitvar3=c("Input for HIT 1 for var3","Input for HIT 2 for var3","Input for HIT 3 for var3"))

# initialize a HITId variable:
inputvalues$HITId <- NA

# Create each HIT:
for(i in 1:nrow(inputvalues)){
    inputvalues$HITId[i] <- 
    CreateHIT(  hit.type="ANEXAMPLEHITTYPEID",
                hitlayoutid="ANEXAMPLEHITLAYOUTID",
                hitlayoutparameters=GenerateLayoutParameter(names(inputvalues),inputvalues[1,]))$HITId
}

# Save the `inputvalues` dataframe:
save(inputvalues, file='inputvalues.RData')

# Later, load the `inputvalues` dataframe:
load(file='inputvalues.RData')

# Then, get assignments:
assignmentresults <- GetAssignment(hit.type="ANEXAMPLEHITTYPEID",return.all=TRUE)

# Then, merge `inputvalues` and `assignmentresults`:
merge(inputvalues,assignmentresults,all=TRUE,by="HITId")
```

---
## HIT Pay and Efficiency Statistics ##

Another nice feature of the Requester UI is the ability to quickly see how long workers are spending on HITs and how that work translates into dollars/hour figures. The data necessary to calculate this is all returned by MTurkR, but it needs a little bit of simple wrapping to output it nicely. Here's a function that calculates all of the relevant information for a HIT:

```r
hitstats <- function(hit){
    info <- status(hit=hit)
    assign <- assignments(hit=hit,return.all=TRUE)

    out <- list(    HITId=info$HITId,
                    HITTypeId=info$HITTypeId,
                    CreationDate=info$CreationTime,
                    Title=info$Title,
                    Description=info$Description,
                    RewardAmount=info$Amount,
                    Assignments=info$NumberOfAssignmentsCompleted,
                    MeanTimeOnHIT=mean(assign$SecondsOnHIT),
                    MedianTimeOnHIT=median(assign$SecondsOnHIT))
    out$MeanHourlyWage <- round(as.numeric(info$Amount)/(out$MeanTimeOnHIT/3600),2)
    out$MedianHourlyWage <- round(as.numeric(info$Amount)/(out$MedianTimeOnHIT/3600),2)
    return(out)
}
```

The result is a list containing basic details of the HIT, along with the number of completed assignments, mean and median time spent on the HIT, and the translation of those times into average hourly wages. Obviously, the median time and median wage will be less influenced by outliers (e.g., individuals that take a very long time on a HIT).

While minimizing hourly wage may be a strong business goal, in other settings it make be reasonable to use this information to target a "fair" wage for workers. For example, in academic research, it is probably ethical to design HITs that pay at least minimum wage for the target population in order to avoid compensation-related coercion.
