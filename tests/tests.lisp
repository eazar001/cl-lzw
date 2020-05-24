(in-package #:cl-lzw-tests)

(def-suite all-tests
    :description "Main tests for compression API")

(in-suite all-tests)

(defun test-api ()
  (run! 'all-tests))

(def-test compress-don-quixote ()
  (cl-lzw:compress-file "../tests/text-files/don-quixote.txt" "../tests/text-files/don-quixote-compressed.txt")
  (cl-lzw:decompress-file
   "../tests/text-files/don-quixote-compressed.txt"
   "../tests/text-files/don-quixote-decompressed.txt")
  (let ((decompressed-bytes (read-file-bytes-to-list "../tests/text-files/don-quixote-decompressed.txt" 8))
        (original-bytes (read-file-bytes-to-list "../tests/text-files/don-quixote.txt" 8)))
    (is (equal decompressed-bytes original-bytes))))

(def-test compress-moby-dick ()
  (cl-lzw:compress-file "../tests/text-files/moby-dick.txt" "../tests/text-files/moby-dick-compressed.txt")
  (cl-lzw:decompress-file
   "../tests/text-files/moby-dick-compressed.txt"
   "../tests/text-files/moby-dick-decompressed.txt")
  (let ((decompressed-bytes (read-file-bytes-to-list "../tests/text-files/moby-dick-decompressed.txt" 8))
        (original-bytes (read-file-bytes-to-list "../tests/text-files/moby-dick.txt" 8)))
    (is (equal decompressed-bytes original-bytes))))

(def-test compress-alice-in-wonderland ()
  (cl-lzw:compress-file
   "../tests/text-files/alice-in-wonderland.txt"
   "../tests/text-files/alice-in-wonderland-compressed.txt")
  (cl-lzw:decompress-file
   "../tests/text-files/alice-in-wonderland-compressed.txt"
   "../tests/text-files/alice-in-wonderland-decompressed.txt")
  (let ((decompressed-bytes (read-file-bytes-to-list "../tests/text-files/alice-in-wonderland-decompressed.txt" 8))
        (original-bytes (read-file-bytes-to-list "../tests/text-files/alice-in-wonderland.txt" 8)))
    (is (equal decompressed-bytes original-bytes))))

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
