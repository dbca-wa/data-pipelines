# Purpose
This folder serves as a template for automating data analysis and visualisation.
The intended workflow is to:

* read data from a CSV,
* analyse and visualise the data using R,
* save the resulting visualisation or graph as a PDF figure,
* keep data, code and output together in a CKAN dataset.

The goal is to automate this workflow, so that:

* it can be re-run with a single click,
* it can be re-run from another script,
* it is self-documenting and retains all involved artefacts (code, figure).


# Your data on CKAN
You need a dataset on CKAN with three resources:

* data as CSV,
* R code as TXT,
* one or several figures as PDF.

The file names of these resources do not matter.

Each resource has a resource ID, which is the part of the URL after `/resource/`.
E.g. the dataset [http://internal-data.dpaw.wa.gov.au/dataset/abundance-of-target-finfish-species-at-the-montebello-and-barrow-islands-marine-protected-areas/](http://internal-data.dpaw.wa.gov.au/dataset/abundance-of-target-finfish-species-at-the-montebello-and-barrow-islands-marine-protected-areas/)
has a CSV resource [http://internal-data.dpaw.wa.gov.au/dataset/abundance-of-target-finfish-species-at-the-montebello-and-barrow-islands-marine-protected-areas/resource/fd874a78-4d65-46fe-ba07-3b1ef0c54269](http://internal-data.dpaw.wa.gov.au/dataset/abundance-of-target-finfish-species-at-the-montebello-and-barrow-islands-marine-protected-areas/resource/fd874a78-4d65-46fe-ba07-3b1ef0c54269),
and the resource ID of that CSV is `fd874a78-4d65-46fe-ba07-3b1ef0c54269`.
PDF, TXT and all other resources also have an ID.

# One-off setup
Open `~/projects/data-pipelines/scripts/ckan_secret_template.R` and follow the 
instructions inside.

# Setting up the workflow automation
For this example, let's assume we want to automate the figure "benthic seagrass cover".

* Copy the `template` folder and rename it to your asset's name, e.g. "seagrass".
* Copy the sub-folder `indicator_template` and rename it to your indicator's name,
e.g. "benthic_cover"; repeat for each indicator as required.
* Make sure the names are lowercase and underscore-separated so we can use them in scripts.
* Modify the file `code.R` as per instructions inside.
