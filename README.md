# CNS in Swish


## What is this?

This project is CNS in [Swish](https://github.com/becls/swish.git).
modified version [Swish-for-CNS](https://github.com/AiziChen/swish/tree/for-cns)
for suitable this project.

The intent of this project is to replace the [previous CNS implementation](https://github.com/AiziChen/CNS.git)
to perform more reliable band-width and more stable.


## Build

`note: if you does not want to build, you can download the binary pack in github 'releases' directly `

`CNS in Swish` can run on multiple architectures.
such as aarch32, aarch64, x86, x86_64 platforms, with Linux,macOS and Windows.

### requirements

You need to install these 3 tools in your computer:

* GNU GCC or Clang.
* [ChezScheme 9.6.4](https://github.com/cisco/ChezScheme.git) version or above.
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
./cns -c config.ss  # run with configuration file, or just run with `./cns` to use the default configuration file `confitg.ss`
./cns -h            # show help
```
