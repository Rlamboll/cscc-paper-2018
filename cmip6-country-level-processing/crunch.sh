#!/bin/bash
for f in /data/cmip6/CMIP6/CMIP/*; do
    SOURCE_FILTER=$(basename $f)
    echo $SOURCE_FILTER
    SOURCE_FILTER=$SOURCE_FILTER jupyter nbconvert --to notebook --execute 010_crunch_raw_data.ipynb --inplace --ExecutePreprocessor.timeout=-1
done


