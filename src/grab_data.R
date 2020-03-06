# Source code for grabbing the Manson (2019) data

# store the url of the raw data
src_url <- "http://assets.nhgis.org/differential-privacy/wide_150.zip"

# store the desired filepath to store the data at
file_path <- "data/raw_data.zip"

# download the data
download.file(src_url, file_path)

# ... and unzip it in the data folder
unzip(file_path, exdir = "data")


