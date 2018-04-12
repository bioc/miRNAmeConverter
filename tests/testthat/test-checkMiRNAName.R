# TODO: Add comment
#
# Author: stefanhaunsberger
###############################################################################

require(miRNAmeConverter);

context("checkMiRNAName");

nc = MiRNANameConverter();

test_that("With 'valid' inputs", {

			miRNAs = c("hsa-miR-422b", "mmu-miR-872",
			           "gra-miR157b", "cel-miR-56*");
			target = c("hsa-miR-422b", "mmu-miR-872",
			           "gra-miR157b", "cel-miR-56*");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'invalid' inputs", {

	# "RNU-6" and "bpcv1-miR-B23" are not valid
	miRNAs = c("hsa-miR-422b", "RNU-6", "mmu-miR-872",
	           "gra-miR157b", "bpcv1-miR-B23", "bpcv1-miR-B1");
	target = c("hsa-miR-422b", "mmu-miR-872",
	           "gra-miR157b", "bpcv1-miR-B1");

	expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

})

test_that("With 'duplicate' inputs", {

			miRNAs = c("hsa-miR-422b", "mmu-miR-872", "gra-miR157b",
			           "mmu-miR-872", "gra-miR157b");
			target = c("hsa-miR-422b", "mmu-miR-872", "gra-miR157b");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'capital letter' (ignore.case should be TRUE) inputs", {

			miRNAs = c("cEL-miR-56*", "ebv-MIR-BART17-5p",
			           "gra-miR157b", "bpcv1-miR-B1");
			target = c("cEL-miR-56*", "ebv-MIR-BART17-5p", 
			           "gra-miR157b", "bpcv1-miR-B1");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

rm(nc);
