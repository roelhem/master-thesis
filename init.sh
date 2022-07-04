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
WORKSPACE_DIR=$(pwd)
ENV_CONFIG_FILE="$WORKSPACE_DIR/.env"
ENV_CONFIG_LOCAL_FILE="$ENV_CONFIG_FILE.local"
PY_COMMAND="python"
PY_VENV_DIR="$WORKSPACE_DIR/.venv"
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


echo " > Deriving directories."
OUT_DIR="$WORKSPACE_DIR/out"
IMPL_DIR="$WORKSPACE_DIR/impl"
TEX_DIR="$WORKSPACE_DIR/tex"

HASKELL_IMPL_DIR="$IMPL_DIR/haskell"
HASKELL_IMPL_SRC_DIR="$HASKELL_IMPL_DIR/src"

TEX_FORMATS_DIR="$TEX_DIR/formats"
TEX_LIBRARY_DIR="$TEX_DIR/library"

LHS2TEX_PATH=":$TEX_FORMATS_DIR//:$HASKELL_IMPL_SRC_DIR"
TEX_PATH="$TEX_DIR/:$TEX_LIBRARY_DIR//:"

export TEXINPUTS=$TEX_PATH


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
#   TYPESETTING                                                                #
# ---------------------------------------------------------------------------- #

lhs-parse () {
  echo -e "\e[1;36m PARSING *.lhs FILES WITH lhs2TeX. \e[0m"
  lhs2TeX --let="debug=True" \
          -o "$OUT_DIR/out.tex" \
          --path="$LHS2TEX_PATH" \
          tools/build.lhs 
  echo -e "\e[1;36m FINISHED PARSING: \e[0m lhs2TeX ended with exit code $?"
}

buildth() {
  lhs-parse && latexmk -jobname="out" \
                       -output-directory=$OUT_DIR \
                       -pdf \
                       -interaction=nonstopmode \
                       -file-line-error \
                       -f \
                       $OUT_DIR/out.tex
  echo -e "\e[1;32m BUILD TERMINATED WITH: " $? "\e[0m\n\n"
}

# ---------------------------------------------------------------------------- #
#   FINALIZING THE INITIALISATION SCRIPT                                       #
# ---------------------------------------------------------------------------- #

# Message
echo -e "\e[1;32m PROJECT SUCCESSFULLY ACTIVATED! \e[0m\n\n"
