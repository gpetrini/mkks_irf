#!/bin/bash
# Script generated by LSD
echo "Processing 600 configuration files in up to 32 parallel processes..."
if [ "$1" = "" ]; then LSD_EXEC="nice /home/gpetrini/LSD/Work/PhD/mkks_irf/lsdNW"; else LSD_EXEC="$1"; fi
if [ "$2" = "" ]; then LSD_CONFIG_PATH="/home/gpetrini/LSD/Work/PhD/mkks_irf/data-irf/S2-noshock"; else LSD_CONFIG_PATH="$2"; fi
echo "LSD executable: $LSD_EXEC"
echo "Configuration path: $LSD_CONFIG_PATH"
echo "Use S2-noshock.sh LSD_EXEC CONFIG_PATH to change default paths"
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 1 -e 19 -p -l "$LSD_CONFIG_PATH"/S2-noshock_1.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 20 -e 38 -p -l "$LSD_CONFIG_PATH"/S2-noshock_2.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 39 -e 57 -p -l "$LSD_CONFIG_PATH"/S2-noshock_3.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 58 -e 76 -p -l "$LSD_CONFIG_PATH"/S2-noshock_4.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 77 -e 95 -p -l "$LSD_CONFIG_PATH"/S2-noshock_5.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 96 -e 114 -p -l "$LSD_CONFIG_PATH"/S2-noshock_6.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 115 -e 133 -p -l "$LSD_CONFIG_PATH"/S2-noshock_7.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 134 -e 152 -p -l "$LSD_CONFIG_PATH"/S2-noshock_8.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 153 -e 171 -p -l "$LSD_CONFIG_PATH"/S2-noshock_9.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 172 -e 190 -p -l "$LSD_CONFIG_PATH"/S2-noshock_10.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 191 -e 209 -p -l "$LSD_CONFIG_PATH"/S2-noshock_11.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 210 -e 228 -p -l "$LSD_CONFIG_PATH"/S2-noshock_12.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 229 -e 247 -p -l "$LSD_CONFIG_PATH"/S2-noshock_13.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 248 -e 266 -p -l "$LSD_CONFIG_PATH"/S2-noshock_14.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 267 -e 285 -p -l "$LSD_CONFIG_PATH"/S2-noshock_15.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 286 -e 304 -p -l "$LSD_CONFIG_PATH"/S2-noshock_16.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 305 -e 323 -p -l "$LSD_CONFIG_PATH"/S2-noshock_17.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 324 -e 342 -p -l "$LSD_CONFIG_PATH"/S2-noshock_18.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 343 -e 361 -p -l "$LSD_CONFIG_PATH"/S2-noshock_19.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 362 -e 380 -p -l "$LSD_CONFIG_PATH"/S2-noshock_20.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 381 -e 399 -p -l "$LSD_CONFIG_PATH"/S2-noshock_21.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 400 -e 418 -p -l "$LSD_CONFIG_PATH"/S2-noshock_22.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 419 -e 437 -p -l "$LSD_CONFIG_PATH"/S2-noshock_23.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 438 -e 456 -p -l "$LSD_CONFIG_PATH"/S2-noshock_24.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 457 -e 474 -p -l "$LSD_CONFIG_PATH"/S2-noshock_25.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 475 -e 492 -p -l "$LSD_CONFIG_PATH"/S2-noshock_26.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 493 -e 510 -p -l "$LSD_CONFIG_PATH"/S2-noshock_27.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 511 -e 528 -p -l "$LSD_CONFIG_PATH"/S2-noshock_28.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 529 -e 546 -p -l "$LSD_CONFIG_PATH"/S2-noshock_29.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 547 -e 564 -p -l "$LSD_CONFIG_PATH"/S2-noshock_30.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 565 -e 582 -p -l "$LSD_CONFIG_PATH"/S2-noshock_31.log &
$LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S2-noshock -s 583 -e 600 -p -l "$LSD_CONFIG_PATH"/S2-noshock_32.log &
echo "32 log files being generated: S2-noshock_1.log to S2-noshock_32.log ."
