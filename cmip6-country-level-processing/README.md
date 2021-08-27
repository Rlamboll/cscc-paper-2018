# CMIP6 country-level data processing

This folder contains the notebooks required to do the country-level data processing.
The method follows [Nicholls et al., *GDJ* 2021](https://doi.org/10.1002/gdj3.113), please cite [Nicholls et al., *GDJ* 2021](https://doi.org/10.1002/gdj3.113) if you use any country-level data in your own work.
We use weights from the 50m scale from [Natural Earth](https://www.naturalearthdata.com/) (see also [here](https://netcdf-scm.readthedocs.io/en/v2.1.0/weights.html#netcdf_scm.weights.get_natural_earth_50m_scale_region_weights)).

Please contact Zebedee Nicholls <zebedee.nicholls@climate-energy-college.org> with any issues (or raise an issue in this github repository and tag @znicholls).

## Install

### Git LFS

We use git LFS for managing large and binary files.
Before you start to work with these repository, make sure you run `git lfs install`.

### Conda environment

We use conda for managing our environments and make for automating workflows.
The environment for running the notebooks can be installed using `make conda-environment`.
Alternately, the steps below should produce the same thing (but please double check with 
the Makefile is this does not work).

```sh
conda create --name cmip6-country-level-data-processing
conda activate cmip6-country-level-data-processing
conda env update --name cmip6-country-level-data-processing -f environment.yml
```

## Run

Running the notebooks should be fairly straight-forward.
Simply run the notebooks in order.
Note, running the first crunching notebook requires access to the CMIP6 archive in the standard CMIP6 data reference syntax.

## Issues

Please contact Zebedee Nicholls <zebedee.nicholls@climate-energy-college.org> with any issues (or raise an issue in this github repository and tag @znicholls).
