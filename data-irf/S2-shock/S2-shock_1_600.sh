nice ../ -c 1 -f ./S2-shock -s 1 -e 150 -p -l ./S2-shock_1.log &
nice ../ -c 1 -f ./S2-shock -s 151 -e 300 -p -l ./S2-shock_2.log &
nice ../ -c 1 -f ./S2-shock -s 301 -e 450 -p -l ./S2-shock_3.log &
nice ../ -c 1 -f ./S2-shock -s 451 -e 600 -p -l ./S2-shock_4.log &
echo "4 log files being generated."
