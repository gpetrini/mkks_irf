#!/bin/sh

#################################################################################
# Script based on purpurea                                                      #
# File modifiled to generage lsd configurations without reliong on the LSD GUI  #
#################################################################################

Help()
{
  # Display help
  echo "These are the program's options: \n"
  echo "\n -h: Help and quit."
  echo "\n -d: Alter directory in which .lsd is saved (default is current directory). The simulation results and R report will be saved in this directory (if -r or -R options are enabled)."
  echo "\n -a: Action to perform over the folder in case it exists: backup, delete, nothing, quit"
  echo "\n -b: Alter base name for .lsd file (default is Sim1). The .lsd termination must not be included."
  echo "\n -u: Alter number of processing units to be used (default is the maximum number of processing units). Required argument: <number_cpus>. "
  echo "\n -M: Advanced mode to run MC runs, in order to optimize the memory use (overwrites -m option). This option runs all files in the working directory. A single run in each .lsd file is set and the seed will be changed progressively until the number of MC runs is reached (the first seed value is equal to one). It also creates auxiliary files in order to run parallel simulations for the same configuration file and use all processing units in the computer, or the maximum declared in '-u'. All .lsd files will have the same number of MC runs. Required argument: <number_mc>. Note that the log files will be overwritten by each simulation."
  echo "\n -p: Alter number of simulation periods. Required argument: <max_period>"
  echo "\n -s: Alter initial seed. Required argument: <seed>."
}




############################################################
# Main program                                             #
############################################################

echo "\n === MC Purpurea: Automatic LSD MC run === \n"

### create default variable values and empty variables ###

FOLDER=
LSDCONF=
BASEDIR=
PERIODS=
SEED=
MONTECARLO=100
UNIT=$(nproc)
EXPS=1
ACTION="backup"
LSD_EXEC="/path/to/lsd_executable"
LSD_CONFIG_PATH="/path/to/lsd_config"

### collect information from options and arguments in command ###

while getopts "hd:b:u:M:p:s" option; do # read options included in the command line, create a list and loop through the list
  case $option in

    h) # display help
      Help
      exit 0;; # quits the program, no additional option is checked.

    d) # alter directory in which the script is executed
      FOLDER=$OPTARG;;

    a) # Action to perform in the folder in case it exists
        ACTION$OPTARG;;
    b) # base LSD file name (without .lsd extension)
       # FIXME Check if lsd extension is passed
      LSDCONF=$OPTARG;;

    u) # alter number of processing units
      UNIT=$OPTARG;;


    M) # Number of monte carlo copies of LSD iles
      MONTECARLO=$OPTARG;;

    p) # update number of periods in simulation
      PERIODS=$OPTARG;;

    s) # update initial seed
      SEED=$OPTARG;;

    \?) # invalid option
       echo "Error: invalid option. Run -h option for help."
       exit 2;; # quits the program, no additional option is checked.

  # FIXME: Include option to delete
  esac # close cases
done


# check if base file exists and declare base file
if [ -f "$FOLDER/$LSDCONF.lsd" ] ; then
  echo "Base file is $BASE.lsd \n"
else
  echo "Selected file '$LSDCONF.lsd' does not exist or it is not located under '$FOLDER'. Please create file and rerun."
  exit 3
fi

# declare folder (if specified by user, otherwise current directory is used)

if [ -d "$FOLDER" ] ; then
    echo "Directory is '$FOLDER' \n"
    BASEFULL="$FOLDER/$LSDCONF" # update complete address of base file
else
    echo "Selected directory in option '-d' does not exist. Please create directory and rerun."
    exit 3
fi



# alter number of periods

if [ "$PERIODS" ] ; then
  sed -i -r "s/^(MAX_STEP) (.*)/MAX_STEP $PERIODS/" "$BASEFULL.lsd"
  echo "\n Number of periods in $BASE.lsd updated:"
  sed -n '/MAX_STEP/'p "$BASEFULL.lsd"
fi

# alter initial seed or save information from file

if [ "$SEED" ] ; then
  sed -i -r "s/^(SEED) (.*)/SEED $SEED/" "$BASEFULL.lsd"
  echo "\n Initial seed in $BASE.lsd has been updated:"
  sed -n '/SEED/'p "$BASEFULL.lsd"
fi


## FIXME DELETE THE FOLDER IN CASE IT EXISTS AND IT IS NOT EMPTY

  # run simulation files

echo "\n Start simulation. "

STARTTIME=$(date +%s)

if [ "$BASEDIR" ] ; then
    EXPS=$(ls -lR $BASEDIR/*.lsd | wc -l) # count number of lsd files

    IBASEFULL="$BASEDIR/$IBASEN" # update complete address of base file

    BASEFULL="$BASEDIR/$IBASEN"1

else
    EXPS=$(ls -lR *.lsd | wc -l) # count number of lsd files

    IBASEFULL="$IBASEN"

    BASEFULL="$IBASEN"1
fi

# create copy of files in order to use all processing units

MCUNITS=$((UNIT/EXPS)) # units per experiment (number of files)

MCUNITS=$((MCUNITS<MONTECARLO ? MCUNITS : MONTECARLO))

MCRUNS=$((MONTECARLO%MCUNITS > 0 ? MONTECARLO/MCUNITS+1 : MONTECARLO/MCUNITS)) # runs per file (djust to guarantee that all MC runs will be simulated)

MCUNITS=$((MONTECARLO%MCRUNS > 0 ? MONTECARLO/MCRUNS+1 : MONTECARLO/MCRUNS)) # correct units per experiment (optimized). It is aslso the number of simulation files per simulation configuration.

echo "\n Optimized mode: each simulation configuration is simulated in $MCUNITS processing units ( running $MCRUNS times each)"

K=1
while [ $K -le $EXPS ] ; do # for each experiment, create copies of files and alter seed

    J=1

    SIMSEED=1

    SIMS=$MCRUNS

    while [ $J -le $MCUNITS ]; do
        cp  "$BASEDIR/$IBASEN$K.lsd"  "$BASEDIR/$J$IBASEN$K.lsd"

        sed -i -r "s/^(SEED) (.*)/SEED $SIMSEED/" "$BASEDIR/$J$IBASEN$K.lsd"

        SIMSEED=$((SIMSEED+MCRUNS))

        SIMS="$SIMS $MCRUNS"

        J=$((J+1))
    done

    rm   "$BASEDIR/$IBASEN$K.lsd"

    K=$((K+1))
done


# run simulations

J=1
while [ $J -le $MCRUNS ] ; do
    echo "\n --- Run $J"

    # alter seed value for each .lsd file
    K=1
    while [ $K -le $EXPS ] ; do # for each experiment, update seed value by adding one
        if [ $J -ne 1 ] ;then # seed is updated only after the first run
            Y=1
            while [ $Y -le $MCUNITS ]; do
                SEEDCURRENT=$(sed -n '/SEED/'p "$BASEDIR/$Y$IBASEN$K.lsd" | awk '{print $NF}')
                SEEDNEW=$((SEEDCURRENT+1))
                sed -i -r "s/^(SEED) (.*)/SEED $SEEDNEW/" "$BASEDIR/$Y$IBASEN$K.lsd"
                Y=$((Y+1))
            done
        fi

        K=$((K+1))
    done


    J=$((J+1))
done


# rename files

K=1
while [ $K -le $EXPS ] ; do # for each experiment, create copies of files and alter seed

    J=1
    X=1

    while [ $J -le $MCUNITS ]; do

        Y=1
        while [ $Y -le $MCRUNS ]; do

            if [ $X -gt $MONTECARLO ]; then
                rm "$BASEDIR/$J$IBASEN$K"_"$X.res.gz"  # remove excess files
            else
                mv "$BASEDIR/$J$IBASEN$K"_"$X.res.gz"  "$BASEDIR/$IBASEN$K"_"$X.res.gz"
            fi


            Y=$((Y+1))
            X=$((X+1))
        done


        if [ $J -ne 1 ]; then
            rm "$BASEDIR/$J$IBASEN$K.lsd"
        else
            mv "$BASEDIR/$J$IBASEN$K.lsd"  "$BASEDIR/$IBASEN$K.lsd"
        fi

        J=$((J+1))

    done

    K=$((K+1))

done

# alter seed value back to original value

K=1
while [ $K -le $EXPS ] ; do
    SEEDCURRENT=$(sed -n '/SEED/'p "$IBASEFULL$K.lsd" | awk '{print $NF}')
    SEEDNEW=$((SEEDCURRENT-MCRUNS+1))
    sed -i -r "s/^(SEED) (.*)/SEED $SEEDNEW/" "$IBASEFULL$K.lsd"
    K=$((K+1))
done

# delete files with final values for each experiment

## FIXME Ensure recompilation

TOTAL_RUNS=$(MONTECARLO)
RUNS_PER_CORE=$((TOTAL_RUNS / UNIT))
REMAINDER=$((TOTAL_RUNS % UNIT))

start=1

for (( i=1; i<=UNIT; i++ )); do
    end=$((start + RUNS_PER_CORE - 1))
    if [[ $i -le $REMAINDER ]]; then
        end=$((end + 1))
    fi
    $LSD_EXEC -c 1 -f "$LSD_CONFIG_PATH"/S1-shock -s $start -e $end -p -l "$LSD_CONFIG_PATH"/S1-shock_${i}.log &
    start=$((end + 1))
done

echo "$(UNIT) log files being generated: FOO_1.log to BAR_$(UNIT).log."

rm -r "$BASEDIR/"*.tot.gz


ENDTIME=$(date +%s)
echo "Finished simulation (total time: $(($ENDTIME - $STARTTIME)) sec.). "
