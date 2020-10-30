# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm

# Additional emacs load-path and autoload
LOAD_PATH := -L .
LOAD_AUTOLOAD := -l autoload

# Define Compile Command (COMPILE)
# Call batch-byte-compile function: -f
COMPILE := -f batch-byte-compile

# AUTOLOAD related variables
AUTOLOAD_DIR  := "${PWD}"
AUTOLOAD_FILE := "${PWD}/ob-elixir-autoloads.el"
AUTOLOAD_EVAL := --eval '(make-directory-autoloads ${AUTOLOAD_DIR} ${AUTOLOAD_FILE})'

# Expand the source code files
EL != ls *.el

# Compiled files
ELC := $(EL:.el=.elc)

# Entry Point
all: compile autoload

# Compile needed files
compile: $(ELC)

# Translate pure Elisp (.el) to byte compile (.elc)
$(ELC): $(EL)
	${EMACS} ${LOAD_PATH} ${COMPILE} ${.ALLSRC}

autoload:
	${EMACS} ${LOAD_AUTOLOAD} ${AUTOLOAD_EVAL}

# Remove {}.elc files
clean:
	${RM} ${ELC}
