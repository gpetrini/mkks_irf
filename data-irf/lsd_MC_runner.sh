#!/bin/bash

#################################################################################
# Script based on purpurea                                                      #
# File modifiled to generage lsd configurations without reliong on the LSD GUI  #
#################################################################################

Help() {
  # Display help
  printf "These are the program's options: \n"
  printf "\n -h: Help and quit."
  printf "\n -d: Alter directory in which .lsd is saved (default is current directory). The simulation results and R report will be saved in this directory (if -r or -R options are enabled)."
  printf "\n -b: Alter base name for .lsd file (default is Sim1). The .lsd termination must not be included."
  printf "\n -u: Alter number of processing units to be used (default is the maximum number of processing units). Required argument: <number_cpus>. "
  printf "\n -M: Advanced mode to run MC runs, in order to optimize the memory use (overwrites -m option). This option runs all files in the working directory. A single run in each .lsd file is set and the seed will be changed progressively until the number of MC runs is reached (the first seed value is equal to one). It also creates auxiliary files in order to run parallel simulations for the same configuration file and use all processing units in the computer, or the maximum declared in '-u'. All .lsd files will have the same number of MC runs. Required argument: <number_mc>. Note that the log files will be overwritten by each simulation."
  printf "\n -p: Alter number of simulation periods. Required argument: <max_period>"
  printf "\n -l: Path to the lsdNW binary"
  printf "\n -K: Alter the number of experiments. 1 means only the default (no-shock) configuration."
}

############################################################
# Main program                                             #
############################################################

printf "\n === MC Purpurea: Automatic LSD MC run === \n"

### create default variable values and empty variables ###

FOLDER=
LSDCONF=
PERIODS=
MONTECARLO=600
UNIT=$(nproc)
LSD_EXEC="./lsdNW"
EXP=7 ## Baseline + 6 experiments

### collect information from options and arguments in command ###

while getopts "hd:b:u:M:p:l:K" option; do # read options included in the command line, create a list and loop through the list
  case $option in

  h) # display help
    Help
    exit 0
    ;; # quits the program, no additional option is checked.

  d) # alter directory in which the script is executed
    FOLDER=$OPTARG ;;

  b) # base LSD file name (without .lsd extension)
    # FIXME Check if lsd extension is passed
    LSDCONF=$OPTARG ;;

  u) # alter number of processing units
    UNIT=$OPTARG ;;

  M) # Number of monte carlo copies of LSD iles
    MONTECARLO=$OPTARG ;;

  p) # update number of periods in simulation
    PERIODS=$OPTARG ;;

  l) # update NW executable path
    LSD_EXEC=$OPTARG ;;

  K) # change the number of experiments
    EXP=$OPTARG ;;

  \?) # invalid option
    echo "Error: invalid option. Run -h option for help."
    exit 2
    ;; # quits the program, no additional option is checked.

  esac # close cases
done

# declare folder (if specified by user, otherwise current directory is used)
if [ -d "$FOLDER" ]; then
  printf 'Working directory is: %s \n' "$FOLDER"
else
  printf "Provided directory does not exist. Please create directory and rerun."
  exit 3
fi

# check if base file exists and declare base file
if [ -f "$FOLDER/$LSDCONF.lsd" ]; then
  printf 'Base file is %s and is located under %s, continuing.\n' "$LSDCONF.lsd" "$FOLDER"
  BASEFULL="$FOLDER/$LSDCONF" # update complete address of base file
else
  printf 'Selected file %s does not exist or it is not located under %s. Please create file and rerun.\n' "$LSDCONF.lsd" "$FOLDER"
  exit 3
fi

# alter number of periods

if [ "$PERIODS" ]; then
  sed -i -r "s/^(MAX_STEP) (.*)/MAX_STEP $PERIODS/" "$BASEFULL.lsd"
  printf "\nNumber of periods in %s updated:" "$LSDCONF.lsd"
  sed -n '/MAX_STEP/p' "$BASEFULL.lsd"
fi

BASEDIR="$FOLDER/$LSDCONF"
if [ -d "$BASEDIR" ]; then
  backup_dir="$FOLDER/${LSDCONF}_backup"

  if [ -d "$backup_dir" ]; then
    printf "\nDeleting existing backup folder %s" "$backup_dir"
    rm -r "$backup_dir"
  fi
  printf "\nCreating a new backup folder: %s" "$backup_dir"
  cp -r "$BASEDIR" "$backup_dir"
fi

if [ ! -d "$BASEDIR" ]; then
  mkdir -p "$BASEDIR"
else
  rm -r "$BASEDIR"
  mkdir -p "$BASEDIR"
fi

for j in $(seq 1 "$EXP"); do
  SCENARIO=$((j - 1))
  DEST_FILE="$BASEDIR/${LSDCONF}${j}_1.lsd"
  cp "$FOLDER/$LSDCONF.lsd" "$DEST_FILE"
  sed -i -r "s/^(Param: Scenario) (.*)([\t ])[0-9\.]+\$/\1 \2\3$SCENARIO/" "$DEST_FILE"
done

for j in $(seq 1 "$EXP"); do
  for i in $(seq 1 "$MONTECARLO"); do
    TEMPLATE="$BASEDIR/${LSDCONF}${j}_1.lsd"
    DEST_FILE="$BASEDIR/${LSDCONF}${j}_$i.lsd"
    if [ "$TEMPLATE" != "$DEST_FILE" ]; then
      cp "$TEMPLATE" "$DEST_FILE"
    fi
    sed -i -r "s/^(SEED) (.*)/SEED $i/" "$DEST_FILE"
  done
done

EXPS=$(find "$BASEDIR" -type f -name "*.lsd" | wc -l)
printf "\nFolder %s populated with %d configuration files\n" "$BASEDIR" "$EXPS"
ls -la "$BASEDIR"

TOTAL_RUNS=$MONTECARLO
RUNS_PER_CORE=$((TOTAL_RUNS / UNIT))
REMAINDER=$((TOTAL_RUNS % UNIT))

SCRIPT="$BASEDIR/${LSDCONF}_1_$MONTECARLO.sh"

start=1

for ((i = 1; i <= UNIT; i++)); do
  end=$((start + RUNS_PER_CORE - 1))
  if [[ $i -le $REMAINDER ]]; then
    end=$((end + 1))
  fi
  CMD="nice ../$LSD_EXEC -c 1 -f ./$LSDCONF${j} -s $start -e $end -p -l ./${LSDCONF}${j}_$i.log &"
  echo "$CMD" >>"$SCRIPT"

  start=$((end + 1))
done
echo "echo \"$UNIT log files being generated.\"" >>"$SCRIPT"

# Make the generated script executable
chmod +x "$SCRIPT"

printf "Script %s generated and made executable.\n" "$SCRIPT"
