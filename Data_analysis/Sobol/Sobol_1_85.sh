#!/bin/bash
# Script generated by LSD
echo "Processing 85 configuration files in up to 8 parallel processes..."
if [ "$1" = "" ]; then LSD_EXEC="/home/italo/LSD/Work/model_jeec/lsdNW"; else LSD_EXEC="$1"; fi
if [ "$2" = "" ]; then LSD_CONFIG_PATH="/home/italo/LSD/Work/model_jeec/Data_analysis/Sobol"; else LSD_CONFIG_PATH="$2"; fi
echo "LSD executable: $LSD_EXEC"
echo "Configuration path: $LSD_CONFIG_PATH"
echo "Use Sobol.sh LSD_EXEC CONFIG_PATH to change default paths"
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 1 -e 11    > "$LSD_CONFIG_PATH"/Sobol_1.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 12 -e 22    > "$LSD_CONFIG_PATH"/Sobol_2.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 23 -e 33    > "$LSD_CONFIG_PATH"/Sobol_3.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 34 -e 44    > "$LSD_CONFIG_PATH"/Sobol_4.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 45 -e 55    > "$LSD_CONFIG_PATH"/Sobol_5.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 56 -e 65    > "$LSD_CONFIG_PATH"/Sobol_6.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 66 -e 75    > "$LSD_CONFIG_PATH"/Sobol_7.log 2>&1 &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/Sobol -s 76 -e 85    > "$LSD_CONFIG_PATH"/Sobol_8.log 2>&1 &
echo "8 log files being generated: Sobol_1.log to Sobol_8.log ."
