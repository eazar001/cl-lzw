(defpackage #:cl-lzw
  (:use :cl)
  (:export #:compress #:decompress))

(in-package #:cl-lzw)

;;; LZW (Lempel-Ziv-Welch) compression algorithm

(defun compress (input-bytes)
  "Takes a list of bytes and encodes them into compressed LZW format."
  (compress-algorithm (init-dict) 256 input-bytes nil))

(defun decompress (input-bytes)
  "Takes a list of compressed LZW encoded bytes and decompresses is into its original format."
  (apply #'concatenate 'list (decompress-algorithm (init-d-dict) 256 input-bytes nil)))

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
           (values new-code found new-code))
          (t
           (values nil found old-code)))))

(defun update-input-bytes (new-code input-bytes)
  (cons new-code (cdr input-bytes)))

(defun compress-algorithm (dict current-code input-bytes output-bytes)
  (if input-bytes
      (destructuring-bind (byte . rest) input-bytes
        (let* ((next-byte (car rest))
               (next-seq (and next-byte (list byte next-byte)))
               (update-results (multiple-value-list (update-dict next-seq current-code dict)))
               (updated (car update-results))
               (found (cadr update-results))
               (new-code (caddr update-results)))
          (if updated
              (compress-algorithm dict (1+ current-code) rest (cons byte output-bytes))
              (let ((next-seq (update-input-bytes new-code rest)))
                (if found
                    (compress-algorithm dict current-code next-seq output-bytes)
                    (compress-algorithm dict current-code next-seq (cons byte output-bytes)))))))
      (reverse output-bytes)))

(defun decode-byte (byte dict)
  (multiple-value-bind (val found) (gethash byte dict)
    (cond (found val)
          (t nil))))

(defun update-d-dict (code byte dict)
  (multiple-value-bind (_ found) (gethash code dict)
    (declare (ignore _))
    (if (not found)
        (setf (gethash code dict) (apply #'concatenate 'list (list byte byte))))))

(defun decompress-algorithm (dict current-code input-bytes output-bytes)
  (if input-bytes
      (destructuring-bind (encoded-byte . rest) input-bytes
        (let* ((byte (decode-byte encoded-byte dict))
               (next-encoded-byte (car rest))
               (next-byte (decode-byte next-encoded-byte dict)))
          (cond ((and byte next-byte)
                 (add-to-dict current-code (apply #'concatenate 'list (list byte next-byte)) dict)
                 (decompress-algorithm dict (1+ current-code) rest (cons byte output-bytes)))
                (byte
                 (update-d-dict current-code byte dict)
                 (if rest
                     (decompress-algorithm dict (1+ current-code) (cons next-encoded-byte (cdr rest)) (cons byte output-bytes))
                     (decompress-algorithm dict (1+ current-code) (cdr rest) (cons byte output-bytes))))
                (t
                 (reverse output-bytes)))))

      (reverse output-bytes)))
