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
			target = c("cel-miR-56*", "gra-miR157b",
			           "hsa-miR-422b", "mmu-miR-872");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'invalid' inputs", {

			# "RNU-6" and "bpcv1-miR-B23" are not valid
			miRNAs = c("hsa-miR-422b", "RNU-6", "mmu-miR-872",
			           "gra-miR157b", "bpcv1-miR-B23", "bpcv1-miR-B1");
			target = c("bpcv1-miR-B1", "gra-miR157b",
			           "hsa-miR-422b", "mmu-miR-872");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'duplicate' inputs", {

			miRNAs = c("hsa-miR-422b", "mmu-miR-872", "gra-miR157b",
			           "mmu-miR-872", "gra-miR157b");
			target = c("gra-miR157b", "hsa-miR-422b", "mmu-miR-872");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'capital letter' (ignore.case should be TRUE) inputs", {

			miRNAs = c("cEL-miR-56*", "ebv-MIR-BART17-5p",
			           "gra-miR157b", "bpcv1-miR-B1");
			target = c("bpcv1-miR-B1", "cel-miR-56*",
			           "ebv-miR-BART17-5p", "gra-miR157b");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'correct=TRUE' parameter.", {

			miRNAs = c("cEL-miR-56*", "ebv-MIR-BART17-5p",
			           "gra-miR157b", "BpcV1-miR-B1");
			target = c("bpcv1-miR-B1", "cel-miR-56*",
			           "ebv-miR-BART17-5p", "gra-miR157b");

			expect_equal(checkMiRNAName(nc, miRNAs, verbose = FALSE), target);

		})

test_that("With 'correct=FALSE' parameter.", {

			miRNAs = c("cEL-miR-56*", "ebv-MIR-BART17-5p",
			           "gra-miR157b", "BpcV1-miR-B1");
			target = c("cEL-miR-56*", "ebv-MIR-BART17-5p",
			           "gra-miR157b", "BpcV1-miR-B1");

			expect_equal(checkMiRNAName(nc, miRNAs,
			                            verbose = FALSE, correct = FALSE),
			             target);

		})

rm(nc);
