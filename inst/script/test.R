## tests to check paging.

library(interactiveDisplayBase)
display(iris)

display(mtcars)


library(AnnotationHub)
ah = AnnotationHub()
df = as.data.frame(mcols(ah))

## This can be set up so that it's all on one page. 
## But: this makes things painfully slow and the 
## moment you do a search the indexing is all 
## screwed up anyways...

## Esentially here I have a problem where the call back is retrieving relative
## indices instead of the absolute ones that I need from it.