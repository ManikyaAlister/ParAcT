.libPaths("Library/Frameworks/R.framework/Versions/4.1/Resources/library")
lib = .libPaths()

install.packages(c("ggplot2","rtdist","msm"),
                   lib = lib,
                   repos = "https://cran.ms.unimelb.edu.au"
                   )