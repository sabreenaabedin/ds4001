install.packages("bnlearn")
library(bnlearn)
data("coronary")
head(coronary)
bn_df <- data.frame(coronary)
## Use an optimization routine (hill-climbing in this case) to determine network structure.
res <- hc(bn_df)
plot(res)
res$arcs <- res$arcs[-which((res$arcs[,'from'] == "M..Work" & res$arcs[,'to'] == "Family")),]
plot(res)

##res2
set.arc(res, from = "Family", to = "Pressure")
## Create conditional tables for each node
res
fittedbn <- bn.fit(res2, data = bn_df)

##For example, let look at what is inside the Protein node.

print(fittedbn$Proteins)
print(fittedbn$M..Work)

## Now make inferences from the network:

cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no") )

## What is the chance that a non-smoker with pressure greater than 140 has a Proteins level less than 3?

cpquery(fittedbn, event = (Proteins=="<3"), evidence = ( Smoking=="no" & Pressure==">140" ) )

## Also work the other way:
## For example, Let’s see if a person’s Proteins level is greater than 3, then what is the chance that his or her Pressure level is greater than 140?

cpquery(fittedbn, event = (Pressure==">140"), evidence = ( Proteins=="<3" ) )


