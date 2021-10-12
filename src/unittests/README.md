# How to add a new unit tests file ?

1. Add the .ml file to this directory
2. Add the name of the file to the `TESTS_ML_FILES` variable in `src/Targets.ml`
3. All side effects of the ml file will be executed by the test runner.

```
let _ = run_tests ()
```

You are encouraged to use OUnit or some ready-made unit test framework, but technically,
it is not required.
