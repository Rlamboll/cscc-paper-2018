.DEFAULT_GOAL := help

ifndef CONDA_PREFIX
$(error Conda not active, please install conda and then create an environment with `conda create --name cmip6-country-level-processing` and activate it with `conda activate cmip6-country-level-processing`))
else
ifeq ($(CONDA_DEFAULT_ENV),base)
$(error Do not install to conda base environment. Source a different conda environment e.g. `conda activate cmip6-country-level-processing` or `conda create --name cmip6-country-level-processing` and rerun make))
endif
VENV_DIR=$(CONDA_PREFIX)
endif

PYTHON=$(CONDA_PREFIX)/bin/python
CONDA_ENV_FILE=environment.yml


define PRINT_HELP_PYSCRIPT
import re, sys

for line in sys.stdin:
	match = re.match(r'^([\$$\(\)a-zA-Z_-]+):.*?## (.*)$$', line)
	if match:
		target, help = match.groups()
		print("%-20s %s" % (target, help))
endef
export PRINT_HELP_PYSCRIPT


help:  ## print short description of each target
	@python -c "$$PRINT_HELP_PYSCRIPT" < $(MAKEFILE_LIST)

.PHONY: conda-environment
conda-environment: $(CONDA_PREFIX)  ## create conda environment for running the notebooks
$(CONDA_PREFIX): $(CONDA_ENV_FILE)
	$(CONDA_EXE) env update --name $(CONDA_DEFAULT_ENV) -f $(CONDA_ENV_FILE)
	touch $(CONDA_PREFIX)
