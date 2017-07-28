# Get Some test data ------------------------------------------------------
## Reading the raw data using the MSnbase package
library(xcms)
## Load 6 of the CDF files from the faahKO
cdf_files <- dir(system.file("cdf", package = "faahKO"), recursive = TRUE,
		 full.names = TRUE)[c(1:3, 7:9)]

## Define the sample grouping.
s_groups <- rep("KO", length(cdf_files))
s_groups[grep(cdf_files, pattern = "WT")] <- "WT"

## Define a data.frame that will be used as phenodata
pheno <- data.frame(sample_name = sub(basename(cdf_files), pattern = ".CDF",
				      replacement = "", fixed = TRUE),
		    sample_group = s_groups, stringsAsFactors = FALSE)

## Read the data.
raw_data <- readMSData2(cdf_files, pdata = new("NAnnotatedDataFrame", pheno)) 


# Extract chromatograms
bpis <- chromatogram(raw_data, aggregationFun = "max")



# Current baseplot --------------------------------------------------------
library(RColorBrewer)
sample_colors <- brewer.pal(3, "Set1")[1:2]
names(sample_colors) <- c("KO", "WT")

plot(bpis, col = paste0(sample_colors[raw_data$sample_group], "80"))





# Functions to do the plotting with ggplot2 -------------------------------
#attributes(bpis)


# Functions to get chromatograms in long format for ggplot2
.ChromatogramLongFormat <- function(x){
        data.frame(rtime = x@rtime, intensity = x@intensity, fromFile = x@fromFile, row.names = NULL, stringsAsFactors = FALSE)
}

.ChromatogramsLongFormat <- function(x, fileAnno = NULL){
        out <- lapply(x@.Data, .ChromatogramLongFormat)
        out <- do.call(rbind.data.frame,out)
        
        # if we supplied fileAnno add the corresponding annotation to each row
        if(!is.null(fileAnno)){
         fileAnno <- cbind.data.frame(fromFile =  1:nrow(fileAnno), fileAnno, stringsAsFactors = FALSE)
         out <- merge(out, fileAnno, all.y = TRUE,  by = "fromFile")
        }
        
        return(out)
}


# function that make the basic ggplot
plotChromatogams <- function(bpis, rt_unit = "sec", fileAnno = NULL) {
        require(ggplot2)
        bpis_long <- .ChromatogramsLongFormat(bpis, fileAnno = fileAnno)
        
        if(rt_unit=="min"){
                bpis_long$rtime <- bpis_long$rtime/60
                x_lab <- "Retention time (min)"
        }
        
        if(rt_unit=="sec"){
                x_lab <- "Retention time (sec)"
        }
        
        ggplot(data = bpis_long, aes(x = rtime, y = intensity, group = fromFile)) + 
                geom_line() +
                theme_classic() +
                labs(x = x_lab, y = "Intensity")
}




# User function in action -------------------------------------------------
# Basic plot
plotChromatogams(bpis)

# use minutes instead
p <- plotChromatogams(bpis, rt_unit = "min")
p


# color by sample assigning a new color to each sample using default ggplot2 color
p + aes(color=fromFile)

# define color for each file manually
# this does the same as the original base plot

col <- paste0(sample_colors[raw_data$sample_group], "80")
names(col) <- 1:length(col)

p + aes(color=fromFile) +
    scale_colour_manual(name = "File", values = col)



# We can instead add some info about each sample
# We can then use the columns in this table to modify the plot
fileAnno <- data.frame(sample_group = raw_data$sample_group)

p <- plotChromatogams(bpis, rt_unit = "min", fileAnno = fileAnno) +
aes(color=sample_group)



p + scale_colour_manual(name = "Group", values = sample_colors)






# TODO: handle several mz-ranges
