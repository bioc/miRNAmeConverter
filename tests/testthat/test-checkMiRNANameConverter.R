# TODO: Add comment
#
# Author: stefanhaunsberger
###############################################################################

require(miRNAmeConverter);

context("MiRNANameConverter-instantiation");

test_that("With 'valid' inputs", {

			target = "MiRNANameConverter";

			expect_equal(class(MiRNANameConverter())[1], target);

		})

