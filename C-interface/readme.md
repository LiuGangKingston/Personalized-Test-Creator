This is the C-interface of the PTC. By using this interface (routines to access/set data in the FORTRAN code), users can do programming in C for answers to questions of the test papers.

To use it, please check the file c.interface.c and template.c or example*.c.

The commands to compile in Linux are 

     gcc -c example10.c
     gfortran PTC-1.f90 fortran.for.c.f90 example10.o
     
     
