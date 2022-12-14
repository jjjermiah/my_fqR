# input: vector of DNA sequence strings

# return: vector of %GC values (what % of the bases are G or C)

# notes:
# 1) verify argument is a character vector
# 2) verify that the bases in the strings only consist of GAT or C and throw
#   a warning if other bases are found
# 3) cope with upper and lower case text

gc_content <- function(my_seq){

    #verify argument is a char vector
    if (!is.character(my_seq)) {
        stop("not chars!")
    } else if(grepl("[^GATC]", my_seq) |> all()) {
        # verify bases are only GATC, throw warning
        warning("Warning: there is a not GATC in the sequence")
    }

    # cope with upper and lowere case text
    my_seq_onlyGC <- gsub("[^GC]", "", my_seq, ignore.case = TRUE)

    # initalize vector of GC values of length of number of sequences
    GC <- vector(mode = "double", length = length(my_seq))

    GC <- mapply("/",
                lapply(my_seq_onlyGC, nchar),
                lapply(my_seq, nchar)
                )
    # return vector of %GC values
    return(100*GC)
}

my_Seq <- c(
    "GCCNGGCTATGCAAGCAGGCTGCAGTGTGGATATAGTCGT",
    "CAGNGAATCCTTGAGGCACCTTCTCTTATAAAAACA",
    "CAGaacgtCTTGAGGCACacccgTTATAAAAACA"
)
gc_content(my_Seq)