# Ensure that the source command is used.
if [ "${BASH_SOURCE-}" = "$0" ]; then
    echo  -e "\e[1;31m You must source this script: \$ source $0 \e[0m" >&2
    exit 33
fi

# ---------------------------------------------------------------------------- #
#   CONFIGURATION                                                              #
# ---------------------------------------------------------------------------- #

# Message
echo -e "\e[1;36m LOADING THE CONFIGURATION. \e[0m"

# Initialising default values.
ENV_CONFIG_FILE="./.env"
ENV_CONFIG_LOCAL_FILE="$ENV_CONFIG_FILE.local"
PY_COMMAND="python"
PY_VENV_DIR="./.venv"
PY_REQUIREMENTS_FILE="./requirements.txt"

# Loading the environment variables.
echo " > Loading variables from '$ENV_CONFIG_FILE'."
source $ENV_CONFIG_FILE

# Loading the local environment variables.
if [[ ! -f $ENV_CONFIG_LOCAL_FILE ]]; then
  echo " > Creating the '$ENV_CONFIG_LOCAL_FILE' file."
  touch $ENV_CONFIG_LOCAL_FILE
fi
echo " > Loading variables from '$ENV_CONFIG_LOCAL_FILE'."
source $ENV_CONFIG_LOCAL_FILE

# Derived variables
PY_VENV_BIN_DIR="$PY_VENV_DIR/bin"
PY_VENV_COMMAND="$PY_VENV_BIN_DIR/python"
PY_VENV_PIP_COMMAND="$PY_VENV_BIN_DIR/pip"
PY_VENV_ACTIVATE_SCRIPT="$PY_VENV_BIN_DIR/activate"


# ---------------------------------------------------------------------------- #
#   CONFIGURING PYTHON                                                         #
# ---------------------------------------------------------------------------- #

# Message
echo -e "\e[1;36m CONFIGURING THE PYTHON VIRUTAL ENVIRONMENT. \e[0m"

# Functions.
pip-install   () { $PY_VENV_PIP_COMMAND install -r $PY_REQUIREMENTS_FILE }
pip-freeze    () { $PY_VENV_PIP_COMMAND freeze > $PY_REQUIREMENTS_FILE }
py-venv-setup () {
  echo " > Setting up at '$PY_VENV_DIR' using '$PY_COMMAND'."
  $PY_COMMAND -m virtualenv $PY_VENV_DIR
  $PY_VENV_PIP_COMMAND install --upgrade pip
  pip-install
}
py-venv-reset () {
  rm -r $PY_VENV_DIR
  py-venv-setup
}

# Check if the Python virtual environment exists.
if [[ -d "$PY_VENV_DIR" && -f "$PY_VENV_ACTIVATE_SCRIPT" ]]; then
  echo "   Python Virtual Environment found at '$PY_VENV_DIR'"
else
  echo "   Python Virtual Environment '$PY_VENV_DIR' is missing."
  py-venv-setup
fi

# Activating the python virtual environment.
source $PY_VENV_ACTIVATE_SCRIPT

# ---------------------------------------------------------------------------- #
#   CONFIGURING HASKELL STACK                                                  #
# ---------------------------------------------------------------------------- #

build-thesis () {
  stack run build-thesis
}

repl () {
  stack ghci --ghc-options="-XOverloadedStrings -XOverloadedLists -XOverloadedLabels -XNoMonomorphismRestriction -XTypeApplications +RTS -M$GHCI_MEMORY_LIMIT"
}

echo -e "\e[1;36m CONFIGURING THE HASKELL ENVIRONMENT. \e[0m"

# ---------------------------------------------------------------------------- #
#   FINALIZING THE INITIALISATION SCRIPT                                       #
# ---------------------------------------------------------------------------- #

# Message
echo -e "\e[1;32m PROJECT SUCCESSFULLY ACTIVATED! \e[0m\n\n"
