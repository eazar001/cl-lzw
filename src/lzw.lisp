(defpackage #:cl-lzw
  (:use :cl)
  (:export #:compress
           #:decompress
           #:compress-file
           #:decompress-file))

(in-package #:cl-lzw)

;;; LZW (Lempel-Ziv-Welch) compression algorithm

(defun compress-file (file)
  "Compresses a file into an LWZ encoded file with .Z extension."
  (let ((out-file (concatenate 'string file ".Z")))
    (write-bytes-to-file (compress (read-file-bytes-to-list file 8)) out-file 12)))

(defun decompress-file (file)
  "Decompresses an encoded LWZ file without the .Z extension."
  (let ((out-file (string-right-trim ".Z" file)))
    (write-bytes-to-file (decompress (read-file-bytes-to-list file 12)) out-file 8)))

(defun compress (input-bytes)
  "Takes a list of bytes and encodes them into compressed LZW format."
  (compress-algorithm (init-dict) 258 input-bytes nil))

(defun decompress (input-bytes)
  "Takes a list of compressed LZW encoded bytes and decompresses them into their original format."
  (decompress-algorithm (init-d-dict) 258 input-bytes nil))

(defun init-dict ()
  (let ((dict (make-hash-table :test 'equal))
        (codes (loop for code from 0 to 255 collect code)))
    (dolist (code codes)
      (add-to-dict (list code) code dict))
    dict))

;;; this is basically like init-dict except with the keys and values reversed for decompression
(defun init-d-dict ()
  (let ((dict (make-hash-table :test 'eql))
        (codes (loop for code from 0 to 255 collect code)))
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
  (cond ((> current-code 4095)
         (compress-algorithm (init-dict) 258 input-bytes (cons 256 output-bytes)))
        (t
         (if input-bytes
             (destructuring-bind (byte . rest) input-bytes
               (let* ((next-byte (car rest))
                      (next-seq (and next-byte (list byte next-byte))))
                 (multiple-value-bind (updated new-code) (update-dict next-seq current-code dict)
                   (if updated
                       (compress-algorithm dict (1+ current-code) rest (cons byte output-bytes))
                       (let ((next-seq (cons new-code (cdr rest))))
                         (compress-algorithm dict current-code next-seq output-bytes))))))
             (reverse (cons 257 output-bytes))))))

(defun decompress-algorithm (dict current-code input-bytes output-bytes)
  (if input-bytes
      (destructuring-bind (encoded-byte . rest) input-bytes
        (let* ((byte (decode-byte encoded-byte dict))
               (next-encoded-byte (car rest))
               (next-byte (list (car (decode-byte next-encoded-byte dict)))))
          (if (and (/= encoded-byte 256) (/= encoded-byte 257) (not (equal next-byte '(nil))))
              (add-to-dict current-code (append byte next-byte) dict)
              (update-d-dict current-code byte dict))
          (cond ((= encoded-byte 256)
                 (decompress-algorithm (init-d-dict) 258 rest output-bytes))
                ((= encoded-byte 257)
                 (decompress-algorithm dict current-code rest output-bytes))
                (t
                 (decompress-algorithm dict (1+ current-code) rest (cons byte output-bytes))))))
      (apply #'append (reverse output-bytes))))

(defun write-bytes-to-file (bytes file-path bits)
  (with-open-file (stream file-path :direction :output
                           :if-does-not-exist :create
                           :if-exists :supersede
                           :element-type (list 'unsigned-byte bits))
     (dolist (b (loop for b in bytes collect b))
       (write-byte b stream))))

(defun read-file-bytes-to-list (file-path bits)
  (with-open-file (stream file-path :direction :input :element-type (list 'unsigned-byte bits))
     (read-bytes-to-list stream nil)))

(defun read-bytes-to-list (stream output-bytes)
  (let ((b (read-byte stream nil)))
    (if b
        (read-bytes-to-list stream (cons b output-bytes))
        (reverse output-bytes))))
