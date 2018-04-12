# Test functions for assessVersion
#
# Author: stefanhaunsberger
###############################################################################

require(miRNAmeConverter);

context("assessVersion");

nc = MiRNANameConverter();

test_that("With miRNA input from file", {

			# Read names
			d = read.table(file = file.path("names.txt"),
			               header = TRUE, stringsAsFactor = FALSE);
			# Extract miRNA names (without assay id)
			miRNAs = substr(d$miRNA, 1,
			                sapply(gregexpr("-", d$miRNA), tail, 1) - 1);
			rm(d);
			# Some names are not valid
			target = 9.0;

			expect_equal((assessVersion(nc, miRNAs, verbose = FALSE))$version[1],
			             target);

		});

rm(nc);
