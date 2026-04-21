## this script doesn't actually do anything, but is just a place to keep some useful code for working with nomisR. 


### 1.1. first getting the overall dataset that contains information on all datasets available through nomis, then just using string searching to get the table id of the one I want. The table id is what you need to pull the data from nomis.
all_datasets_info <- nomis_data_info()
all_datasets_info <- data.table(all_datasets_info)

estimate_search <- all_datasets_info[grep("estimate", name.value), ]

### 1.2. for a given table id, the command below gives you all of the geographies that it's available at. 
geog_types <- data.table(nomis_get_metadata(id = "NM_2014_1", # as a note, NM_2020_1 is the code for 2011-based. 
                                            concept = "geography",
                                            type = "TYPE")) # TYPE151 for 2021 lsoas


