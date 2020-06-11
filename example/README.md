# Running harmonization from example documentation

This directory provides documentation for three example harmonized variables for two studies.


## Notable files

| file | description`  |
|---|---|
| `example_data/dbgap` | a directory containing simulated dbGaP data for two studies |
| `example_data/harmonized` | a directory containing the harmonized data produced by these example files |
| `example_bmi_1.json` | example JSON documentation for the example_bmi_1 variable |
| `example_height_1.json` | example JSON documentation for the example_height_1 variable |
| `example_weight_1.json` | example JSON documentation for the example_weight_1 variable |
| `harmonized-variable-reproduction-height.md` | example of reproducing the `example_height_1` harmonized variable using component **study** variables |
| `harmonized-variable-reproduction-weight.md` | example of reproducing the `example_weight_1` harmonized variable using component **study** variables |
| `harmonized-variable-reproduction-bmi.md` | example of reproducing the `example_bmi_1` variable using `example_height_1` and `example_weight_1` as component **harmonized** variables |

## Running the example harmonization

Start by following the steps in `harmonized-variable-reproduction-height.md` to reproduce the `example_height_1` variable.
Next, follow the `harmonized-variable-reproduction-weight.md` to reproduce the `example_weight_1` variable.

You will need to reproduce both `example_height_1` and `example_weight_1` before attempting to reproduce the `example_bmi_1` variable, since harmonized versions of height and weight are required to create this variable in `harmonized-variable-reproduction-bmi.md`.

Note that manual processing is required when reproducing DCC harmonization from the JSON documentation, as different investigators will have access to different sets of dbGaP files.
