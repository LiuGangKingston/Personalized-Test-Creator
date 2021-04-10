       The Personalized Test Creator (PTC)

The PTC is a FORTRAN program that converts an input LATEX file containing test
questions into multiple output files that contain test sheets, solution sheets, and marking
sheets. The LATEX input file with the original question sets contains additional PTC specific
constructs that allow the randomization of test contents, such as the input quantities
and the ordering of the answers in multiple-choice questions. The correct answers can be
automatically computed using a user-defined FORTRAN subroutine with a fixed interface.
This scheme allows the automatic generation of randomized tests with the proper answer
and marking sheets from a single original file.
LATEX and FORTRAN are needed for extensive use of this package. 


However, if no answer 
is needed to be calcualted with FORTRAN language, no need to code further FORTRAN code.

We suggest to check the example files that come with the PTC package while reading the manual.
