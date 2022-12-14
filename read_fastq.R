#input: fastq file
#return: tibble with:
# one row per fastq entry where the columns are:
    # 1) ID
    # 2) Bases
    # 3) Qualities
    # 4) GC content


library(assertthat)
library(tibble)
library(stringr)

read_fastq <- function(file){
    #Verify the fastq location provided is a file that is readable,
    # has an extension of .fq
    assert_that(is.readable(file))
    assert_that(has_extension(file, ext="fq"))

    # Read in file
    filelines <- scan(file, character())

    # Parse lines
    ids <- filelines[c(TRUE, FALSE, FALSE, FALSE)]
    seqs <- filelines[c(FALSE, TRUE, FALSE, FALSE)]
    qualities <- filelines[c(FALSE, FALSE, FALSE,TRUE)]

    # check if ids are unique
    if (anyDuplicated(ids)) {
        stop("duplicated IDs!!!")
    }
    if (!all(startsWith(ids, "@"))) stop("IDs dont start with @")

    if (!all(nchar(seqs) == nchar(qualities))) {
        stop("some sequences dont have the same length as bases")
    }

    filetibble <- tibble(
        IDS = str_sub(ids, 2),
        bases = seqs,
        Qualities = qualities,
        GC = gc_content(seqs)

    )
    return(filetibble)
}
path <- "testPkg/PackageDevelopmentData/fastq_examples/good.fq"
x <- read_fastq(path)
gc_content("GCGCA")
