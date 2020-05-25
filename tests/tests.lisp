(in-package #:cl-lzw-tests)

(def-suite all-tests
    :description "Unit-tests for compression/decompression API")

(in-suite all-tests)

(defmacro with-teardown (path &body body)
  (let ((a (concatenate 'string path "-compressed.txt"))
        (b (concatenate 'string path "-decompressed.txt")))
    `(progn ,@body
            (if (probe-file ,a)
                (delete-file ,a))
            (if (probe-file ,b)
                (delete-file ,b)))))

(test compress-don-quixote
  :description "Compress and decompress \"Don Quixote\" novel."
  (cl-lzw:compress-file "tests/text-files/don-quixote.txt" "tests/text-files/don-quixote-compressed.txt")
  (cl-lzw:decompress-file
   "tests/text-files/don-quixote-compressed.txt"
   "tests/text-files/don-quixote-decompressed.txt")
  (with-teardown "tests/text-files/don-quixote"
    (let ((decompressed-bytes (read-file-bytes-to-list "tests/text-files/don-quixote-decompressed.txt" 8))
          (original-bytes (read-file-bytes-to-list "tests/text-files/don-quixote.txt" 8)))
      (is (equal decompressed-bytes original-bytes)))))

(test compress-moby-dick
  :description "Compress and decompress \"Moby Dick\" novel."
  (cl-lzw:compress-file "tests/text-files/moby-dick.txt" "tests/text-files/moby-dick-compressed.txt")
  (cl-lzw:decompress-file
   "tests/text-files/moby-dick-compressed.txt"
   "tests/text-files/moby-dick-decompressed.txt")
  (with-teardown "tests/text-files/moby-dick"
    (let ((decompressed-bytes (read-file-bytes-to-list "tests/text-files/moby-dick-decompressed.txt" 8))
          (original-bytes (read-file-bytes-to-list "tests/text-files/moby-dick.txt" 8)))
      (is (equal decompressed-bytes original-bytes)))))

(test compress-alice-in-wonderland
  :description "Compress and decompress \"Alice in Wonderland\" novel."
  (cl-lzw:compress-file
   "tests/text-files/alice-in-wonderland.txt"
   "tests/text-files/alice-in-wonderland-compressed.txt")
  (cl-lzw:decompress-file
   "tests/text-files/alice-in-wonderland-compressed.txt"
   "tests/text-files/alice-in-wonderland-decompressed.txt")
  (with-teardown "tests/text-files/alice-in-wonderland"
    (let ((decompressed-bytes (read-file-bytes-to-list "tests/text-files/alice-in-wonderland-decompressed.txt" 8))
          (original-bytes (read-file-bytes-to-list "tests/text-files/alice-in-wonderland.txt" 8)))
      (is (equal decompressed-bytes original-bytes)))))

(defun read-file-bytes-to-list (file-path bits)
  (with-open-file (stream file-path
                          :direction :input
                          :element-type (list 'unsigned-byte bits))
    (read-bytes-to-list stream nil)))

(defun read-bytes-to-list (stream output-bytes)
  (let ((b (read-byte stream nil)))
    (if b
        (read-bytes-to-list stream (cons b output-bytes))
        (reverse output-bytes))))
