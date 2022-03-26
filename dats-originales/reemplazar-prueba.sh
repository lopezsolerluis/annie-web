#!/bin/bash

# Para economizar, usamos longitudes de onda entre 3800 y 7500 A.
lambdaMin=3800;
lambdaMax=7500;

while read line ; do
    x=$( echo $line | cut -c 1-4 )
    y=$( echo $line | cut -c 8-15 )
    echo $line;
    printf '       {:x %s :y %s}\n' $x $y 
done < $1

