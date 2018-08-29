# Units and Programs

Pascal programs and libaries are built from one or more modules. The primary
unit specifies the type of module that the compiler should build and
lists the unit dependencies of the module.

## The primary unit

When the compiler is invoked, the path to the primary unit is passed as
an argument. The unit may start with an optional module type statement to
set the output name and module type:

    program HelloWorld

    library HelloWorld

If the module type statement is missing, the module is inferred to be
a program with the same name as the filename of the unit.