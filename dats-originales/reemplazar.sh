# Para economizar, usamos longitudes de onda entre 3800 y 7500 A.
lambdaMin=3800;
lambdaMax=7500;

printf '(def espectros-dat {\n' > espectros.cljs;

for file in *.dat; do
filename=${file%.*};
printf '    :%s [\n' $filename >> espectros.cljs;
while read line ; do
    x=$( echo $line | cut -c 1-4 )  # Supongo que todos terminan con un solo decimal
    if [[ $x -ge $lambdaMin && $x -le $lambdaMax ]]
    then
        y=$( echo $line | cut -c 8-15 )
        printf '       {:x %s :y %s}\n' $x $y >> espectros.cljs
    fi
done < $file
printf ']\n' >> espectros.cljs
done

printf '})\n' >> espectros.cljs;

# find . -name "*.dat" -printf '%P\n' -exec ./reemplazar {} \;

