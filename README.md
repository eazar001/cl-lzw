# cl-lzw
This is a Common LISP package that implements the LZW compression algorithm. This library doesn't aim to be the most efficient, 
effective, or even well-rounded implementation of LZW. I merely once needed to decompress resources for another project which 
compressed graphics using this method. At the time of writing, there was no third-party Quicklisp package for it, so I wrote this.

The basic functionality is done and is all I need, so be warned that this library is most likely not suitable for your use-case.
Nonetheless, feel free to use/adapt this work if you somehow find it useful. Further work on this project is possible in the
future, but definitely by no means guaranteed.

## Additional notes
As this is not meant to be a perfect implementation of LZW, currently only 8-bit input files are supported for input. Also,
smaller files may actually increase in size, as this algorithm mainly works on exploiting pattern redundancy over larger files.
In particular this tends to excel with large text files, TIF files, BMP, etc. The current implementation has not be fine-tuned
to work properly with GIF files, although they will encode and decode properly.

## Usage
```
;; compress a list of bytes
(compress '(65 65 65 66))

;; decompress a list of encoded bytes
(decompress (compress '(65 65 65 66)))

;; compress a file with arbitrary 8-bit data into file.Z
(compress-file "my_file")

;; decompress an LZW encoded "my_file.Z" into its original 8-bit data as "file"
(decompress-file "my_file.Z")
```
