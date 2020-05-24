(defpackage #:cl-lzw
  (:use :cl)
  (:export #:compress-file
           #:decompress-file))

(in-package #:cl-lzw)

;;; LZW (Lempel-Ziv-Welch) compression algorithm

(defun compress-file (file out-file)
  "Compresses a file into an LWZ encoded file."
  (write-bytes-to-file (compress (read-file-bytes-to-list file 8)) out-file 16))

(defun decompress-file (file out-file)
  "Decompresses an encoded LWZ file."
  (write-bytes-to-file (decompress (read-file-bytes-to-list file 16)) out-file 8))

;;; Takes a list of bytes and encodes them into compressed LZW format
(defun compress (input-bytes)
  (compress-algorithm (init-dict) #x102 input-bytes (list #x100)))

;;; Takes a list of compressed LZW encoded bytes and decompresses them into their original format
(defun decompress (input-bytes)
  (decompress-algorithm (make-hash-table) #x102 input-bytes nil))

(defun init-dict ()
  (let ((dict (make-hash-table :test 'equal))
        (codes (loop for code from 0 to #xFF collect code)))
    (dolist (code codes)
      (add-to-dict (list code) code dict))
    dict))

;;; this is basically like init-dict except with the keys and values reversed for decompression
(defun init-d-dict ()
  (let ((dict (make-hash-table :test 'eql))
        (codes (loop for code from 0 to #xFF collect code)))
    (dolist (code codes)
      (add-to-dict code (list code) dict))
    dict))

(defun add-to-dict (key value dict)
  (if key
      (setf (gethash key dict) value)))

(defun update-dict (key new-code dict)
  (multiple-value-bind (old-code found) (gethash key dict)
    (cond ((not found)
           (add-to-dict key new-code dict)
           (values new-code new-code))
          (t
           (values nil old-code)))))

(defun update-d-dict (code byte dict)
  (multiple-value-bind (_ found) (gethash code dict)
    (declare (ignore _))
    (if (not found)
        (setf (gethash code dict) (append byte (list (car byte)))))))

(defun decode-byte (byte dict)
  (multiple-value-bind (val found) (gethash byte dict)
    (cond (found val)
          (t nil))))

(defun compress-algorithm (dict current-code input-bytes output-bytes)
  (cond ((> current-code #xFFFF)
         (compress-algorithm (init-dict) #x102 input-bytes (cons #x100 output-bytes)))
        (t
         (if input-bytes
             (destructuring-bind (byte . rest) input-bytes
               (let* ((next-byte (car rest))
                      (next-seq (and next-byte (list byte next-byte))))
                 (multiple-value-bind (updated new-code) (update-dict next-seq current-code dict)
                   (if updated
                       (compress-algorithm dict (1+ current-code) rest (cons byte output-bytes))
                       (compress-algorithm dict current-code (cons new-code (cdr rest)) output-bytes)))))
             (reverse (cons #x101 output-bytes))))))

(defun decompress-algorithm (dict current-code input-bytes output-bytes)
  (if input-bytes
      (destructuring-bind (encoded-byte . rest) input-bytes
        (let* ((byte (decode-byte encoded-byte dict))
               (next-encoded-byte (car rest))
               (next-byte (list (car (decode-byte next-encoded-byte dict)))))
          (if (and (or (< encoded-byte #x100) (> encoded-byte #x101)) (car next-byte))
              (add-to-dict current-code (append byte next-byte) dict)
              (and byte (update-d-dict current-code byte dict)))
          (cond ((= encoded-byte #x100)
                 (decompress-algorithm (init-d-dict) #x102 rest output-bytes))
                ((= encoded-byte #x101)
                 (decompress-algorithm dict current-code rest output-bytes))
                (t
                 (decompress-algorithm dict (1+ current-code) rest (cons byte output-bytes))))))
      (loop for x in (nreverse output-bytes) append x)))

(defun write-bytes-to-file (bytes file-path bits)
  (with-open-file (stream file-path
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type (list 'unsigned-byte bits))
     (dolist (b bytes)
       (write-byte b stream))))

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
