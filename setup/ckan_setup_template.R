library(ckanr)
# Save a copy of this file as "ckan_setup.R" in the same folder.
# Login to CKAN, click on your name, copy your API key on your profile page.
# Paste your CKAN API key:
MY_API_KEY = ""
ckanr::ckanr_setup(url="http://internal-data.dpaw.wa.gov.au/", key=MY_API_KEY)
