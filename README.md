# CNS in Swish


## What is this?

this is CNS in [ChezScheme-Swish](https://github.com/becls/swish.git)
to replace the [previous CNS implementation](https://github.com/AiziChen/CNS.git)
to perform more performance and more reliable.


## Use

without build. you only need to download the binaries in `releases`.


## Build

CNS in Swish can run on multiple architectures.
such as aarch32, aarch64, x86, x86_64 platforms, with Linux/macOS and Windows.

### requirements

you need to install these software in your computer:

* ChezScheme 9.5.4 version or above.
* Swish 2.2.0 version or above

### compile & build & run

compile & build:

```shell
make        # build
make help   # show helps
```

run:
```shell
./cns -c config.ss  # run with configuration file
./cns -h            # show help

```
