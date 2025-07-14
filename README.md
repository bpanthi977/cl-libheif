# Common Lisp Bindings to libheif

Binding to [libheif](https://github.com/strukturag/libheif) in Common Lisp.

## Requirement

You need to have `libheif` shared library installed in your system.

## Use

To read a heif image as an array of rgb values do this:
```lisp
(heif:read-image "/path/to/image.heic")
```

To register heif formats with [opticl](https://github.com/slyrus/opticl) library:

```lisp
(heif:register-opticl-hander)
```

Then you can read and manipulate heif images as usual with `opticl`.

## License

MIT

