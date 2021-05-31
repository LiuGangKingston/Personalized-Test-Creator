This is the CPP-interface of the PTC. By using this interface (routines to access/set data in the FORTRAN code), users can do programming in C++ for answers to questions of the test papers.

To use it, please check the file cpp.interface.cpp and template.cpp or example*.cpp.

The commands to compile in Linux are 

     gfortran -c PTC-1.f90 fortran.for.cpp.f90 
     g++ example10.cpp PTC-1.o fortran.for.cpp.o -lgfortran


