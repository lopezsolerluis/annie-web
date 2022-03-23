printf '(def espectros-vector [' > espectros-vector.cljs;

for file in *.dat; do

filename=${file%.*};

printf "$filename " >> espectros-vector.cljs;

done

printf '])\n' >> espectros-vector.cljs;


