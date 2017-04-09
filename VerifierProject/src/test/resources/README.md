Test Resources
==============

The viper programs that we used to test our implementation are in this folder and its subfolders.

Filenames
---------

Into the filenames is encoded how many assertions are excepted to "might-fail".
Note: amfc stands for "assertion might fail count".

The filenames are of the form:
```<somethingMeaningful>-amfc-<assertion-might-fail-count>.vpr```

WlpStar
-------

Contains test cases (.vpr files) that already are in DSA and reduced to the four statements:
* ```s1; s2```
* ```s1 [] s2```
* ```assume A```
* ```assert A```

The test-cases in this folder test our ```wlp*``` function.



