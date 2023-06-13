# Data Manifest

# Print directory tree for outputs
folders_exclude <- "gmaps|sample|car"
fs::dir_tree(path = 'output', recurse = TRUE, regex = folders_exclude, invert = TRUE)

# Files' size
list_dirs <- as.data.frame(fs::dir_tree(path = 'output', recurse = TRUE, regex = folders_exclude, invert = TRUE))
# Individual file size
sapply(list_dirs, function(x) gdata:::humanReadable(file.size(x)))
# Total size
sapply(list_dirs, function(x) gdata:::humanReadable(sum(file.size(x))))

