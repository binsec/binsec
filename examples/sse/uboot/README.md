Toy example inspired by U-Boot's CVE-2019-14192.

To build the example:
```
make uboot.run
```

To get a coredump:
```
make_coredump.sh core.snapshot uboot.run
```

To run the analysis (with shadow stack):
```
binsec -sse -sse-script verify.ini -shadow-stack core.snapshot
```
