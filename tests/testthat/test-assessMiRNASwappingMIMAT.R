# Test functions for assessMiRNASwappingMIMAT function
#
# Author: Stefan Haunsberger
###############################################################################

require(miRNAmeConverter);

context("assessMiRNASwappingMIMAT");

nc = MiRNANameConverter();

test_that("With NO swapping inputs", {

			# "RNU-6" and "bpcv1-miR-B23" are not valid
			miRNAs = c("hsa-miR-422b", "RNU-6", "mmu-miR-872",
			           "bpcv1-miR-B23", "bpcv1-miR-B1");
			target = character();

			expect_equal(assessMiRNASwappingMIMAT(nc, miRNAs, verbose = FALSE),
			             target);

		})

test_that("With swapping inputs", {

			miRNAs = c("hsa-miR-493", "hsa-miR-422b", "RNU-6",
			           "mmu-miR-872", "hsa-miR-488", "bpcv1-miR-B23");
			target = c("hsa-miR-488", "hsa-miR-493");

			expect_equal(assessMiRNASwappingMIMAT(nc, miRNAs, verbose = FALSE),
			             target);

		})

rm(nc);

