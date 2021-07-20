import iris.analysis
import iris.quickplot as qplt
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import regionmask
import xarray as xr


def load_pop_2020():
    gpw_v4_meta = pd.read_csv("gpw_v4_netcdf_contents_rev11.csv")
    population_raster = gpw_v4_meta[
        (gpw_v4_meta["file_name"] == "gpw_v4_population_count_rev11")
        & (gpw_v4_meta["raster_description"] == "Population count for the year 2020")
    ].loc[:, "order"]
    population_raster = int(population_raster)
    
    population_2020 = xr.load_dataset("gpw_v4_population_count_rev11_30_min.nc")[
        "Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 30 arc-minutes"
    ].sel(raster=population_raster)
    
    return population_2020


def get_pop_2020_iris(inp):
    population_2020_iris = inp.drop("raster")
    population_2020_iris.attrs["units"] = "1"
    population_2020_iris.name = "Population"
    population_2020_iris["latitude"].attrs["standard_name"] = "latitude"
    population_2020_iris["longitude"].attrs["standard_name"] = "longitude"
    population_2020_iris = population_2020_iris.to_iris()
    population_2020_iris.coord("latitude").guess_bounds()
    population_2020_iris.coord("longitude").guess_bounds()

    return population_2020_iris


def get_regions():
    ignore_regions = {
        "World|Natural Earth 50m|San Marino",
        "World|Natural Earth 50m|Turks and Caicos Is.",
        "World|Natural Earth 50m|Indian Ocean Ter.",
        "World|Natural Earth 50m|Falkland Is.",
        "World|Natural Earth 50m|Fr. S. Antarctic Lands",
        "World|Natural Earth 50m|N. Mariana Is.",
        "World|Natural Earth 50m|St-Barthélemy",
        "World|Natural Earth 50m|Wallis and Futuna Is.",
        "World|Natural Earth 50m|Sint Maarten",
        "World|Natural Earth 50m|St-Martin",
        "World|Natural Earth 50m|British Virgin Is.",
        "World|Natural Earth 50m|Åland",
        "World|Natural Earth 50m|Niue",
        "World|Natural Earth 50m|Heard I. and McDonald Is.",
        "World|Natural Earth 50m|Br. Indian Ocean Ter.",
        #     "World|Natural Earth 50m|Antarctica",
    }
    
    regions = (
        set(
            [
                "World|Natural Earth 50m|{}".format(c)
                for c in regionmask.defined_regions.natural_earth.countries_50.names
                #             for c in regionmask.defined_regions.natural_earth.countries_50.names[:1]
                #             + [
                #                 #                 "Australia",
                #                 #                 "Chile",
                #                 #                 "Russia",
                #                 #                 "Canada",
                #                 #                 "France",
                #                 #                 "United States of America",
                #                 #                 "Israel",
                #                 "Jamaica",
                #                 #                 "Maldives",
                #                 #                 "Norfolk Island",
                #             ]
            ]
            + ["World"]
        )
        - ignore_regions
    )
    regions = ",".join(regions)
    
    return regions


def get_natural_earth_50m_scale_nearest_last_resort_region_weights(region, population_2020_iris):
    def _calculate_region_weights(weight_calculator, cube, **kwargs):
        r_stripped = region.replace("Nearest ", "")
        weights_no_pop = weight_calculator.get_weights_array_without_area_weighting(
            r_stripped
        )
        if np.equal(np.sum(weights_no_pop), 0):
            region_mask = regionmask.defined_regions.natural_earth.countries_50[
                [region.split("|")[-1]]
            ]
            assert len(region_mask.centroids) == 1
            centre = region_mask.centroids[0]
            weights_no_pop[
                cube.cube.coord("latitude").nearest_neighbour_index(centre[1]),
                cube.cube.coord("longitude").nearest_neighbour_index(centre[0]),
            ] = 1

        return weights_no_pop

    return _calculate_region_weights


def get_natural_earth_50m_scale_popn_weighted_region_weights(region, population_2020_iris):
    def _calculate_region_weights(weight_calculator, cube, **kwargs):
        weights_no_pop = weight_calculator.get_weights_array_without_area_weighting(
            region.replace("Popn weighted ", "Nearest ")
        )

        try:
            cube.cube.coord("latitude").guess_bounds()
        except ValueError:
            pass

        try:
            cube.cube.coord("longitude").guess_bounds()
        except ValueError:
            pass

        pop_regrid = population_2020_iris.regrid(
            cube.cube, iris.analysis.AreaWeighted()
        )
        out = pop_regrid * weights_no_pop
        out = out.data
        out[np.isnan(out)] = 0

        return out

    return _calculate_region_weights


def plot_weights(example_cube, weights_to_plot, constraint=None, axes=None, **kwargs):
    for i, (label, weights) in enumerate(weights_to_plot.items()):
        if axes is None:
            ax = plt.figure().add_subplot(111)
        else:
            ax = axes[i]

        weight_cube = example_cube.cube.collapsed("time", iris.analysis.MEAN)
        weight_cube.data = weights
        weight_cube.data[np.where(np.equal(weight_cube.data, 0))] = np.nan
        weight_cube.units = ""
        if constraint is not None:
            weight_cube = weight_cube.extract(constraint)

        plt.sca(ax)

        qplt.pcolormesh(
            weight_cube,
            **kwargs,
        )

        plt.gca().set_title(label)
        plt.gca().coastlines(alpha=0.5)
