#/bin/bash

CORPUS=$1

 # Remove links between words
 # Remove <Correct!> tags
 # Remove all that is not enclosed in < >
 # Remove lemmas
 
cat $CORPUS | cg-conv -A | tr '/$^' '\n' | \
 sed -E 's/<m?w[0-9].*[0-9]>//' | \
 sed -E 's/<<Correct!>>//' | \
 sed -E 's/^[^\<].*[^\>]$//' | \
 sed -E 's/^[a-zA-Zñ�0-9_,%:!\?\.\+\*\-]+</</' | \
 sort | uniq 