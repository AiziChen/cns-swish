# CNS in Swish


## What is this?

This project is CNS in [Swish](https://github.com/becls/swish.git).
had been modified by Quanye: [Swish-for-CNS](https://github.com/AiziChen/swish/tree/for-cns)
for suitable for this project.

The intent of cns-scheme is to replace the [previous CNS implementation](https://github.com/AiziChen/CNS.git)
to perform more reliable band-width and more stable.


## Build

`note: if you does not want to build, you can download the binaries in github's 'releases' section directly `

`CNS in Swish` can run on multiple architectures.
such as aarch32, aarch64, x86, x86_64 platforms, with Linux/macOS and Windows.

### requirements

You need to install these software in your computer:

* GNU GCC or Clang.
* ChezScheme 9.5.4 version or above.
* Swish in [this branch](https://github.com/AiziChen/swish/tree/for-cns).

### build

```shell
make        # build
make help   # show helps
```

After build, The compiler will output the executable binary `cns`.


## Use

After you built or downloaded the binary `cns`:

```shell
chmod +x cns
./cns -c config.ss  # run with configuration file
./cns -h            # show help
```
