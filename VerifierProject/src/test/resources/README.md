Test Resources
==============

The viper programs that we used to test our implementation are in this folder and its sub-folders.
We used mainly automated tests to check the correctness of our verifier.
We did split positive and negative tests-cases into separate files and the tests just check the number of assertions,
invariants or post-conditions that may not hold.
For the positive tests we want to have 0 assertions that might not hold and for the negative tests more than 0.
We encoded this information of how many assertions are expected to "might-fail" into the filenames.

The filenames are of the form:
```<somethingMeaningful>-amfc-<assertion-might-fail-count>.vpr```

Note: amfc stands for "assertion might fail count".

From time to time we also did manual tests using Visual Studio Code.

Timeout
-------

For our automated tests we use a z3 timeout of 20 seconds. For the assembled jar version that can be used in Visual
Studio Code we use no timeout for z3.

WlpStar
-------

Contains test cases (.vpr files) that already are in DSA and reduced to the four statements:
* ```s1; s2```
* ```s1 [] s2```
* ```assume A```
* ```assert A```

The test-cases in this folder unit-test our ```wlp*``` function. They are very simple but ensure high line coverage in
the ```WlpStar``` class as well as ```ViperToSmtlibUtils```.
We used those tests early on in the project when the DSA transformation and method transformation was not implemented
yet.

EndToEnd
--------

Contains very simple test-cases that are mainly used to test the method transformation and DSA conversion but of course
also run through our weakest liberal precondition code.
The tests in this folder ensure a high line coverage in the ```MethodTransformer```.

ApplicationTests
----------------

Contains test-cases that are actually some implementation of an algorithm or a simple data structure.

//TODO list all the test cases here and bla bla...



