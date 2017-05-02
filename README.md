VerifierProject
===============

Command Line Args
-----------------
|Command line argument|meaning|
|---|---|
|``--printInput``|Print the input program|
|``--printSmt``|Print the generated SMT output|
|``--printTransformed``|Print the transformed program (see printTransformed below)|
|``--z3Args <arg>``|Command line arguments which should be forwarded to Z3|
|``--z3Exe <arg>``|Manually-specified full path to z3 executable|
|``--help``|Show help message|
|``<file>`` (required)|The file to verify|


Usage with ```sbt```:
```
sbt "run [options] <file>"
```

Example:
```
sbt "run --printTransformed src/test/resources/RealApplicationTests/SelectionSortNegative-amfc-6.vpr"
```

### printTransformed
since we use a ``wlp*`` function as described in the lecture we transform
all input programs to a use only the 4 statements below:

* ``s1; s2``
* ``({s1}) [] ({s2})`` (non-deterministic choice)
* ``assume A``
* ``assert A``

The output that the ``--printTransformed`` flag will produce contains
non-deterministic choices, which is not a part of the Viper language.

Automated tests
---------------
To run the automated tests, execute: ``sbt test`` in the ``VerifierProject`` subdirectory.

Creating the jar
----------------
To assemble the jar, execute: ``sbt assembly`` in the ``VerifierProject`` subdirectory.

Evaluation
----------
The summary of the test-cases can be found here: [src/test/resources](./VerifierProject/src/test/resources/).
