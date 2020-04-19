(defpackage #:cl-lzw
  (:use :cl)
  (:export #:compress #:decompress))

(in-package #:cl-lzw)

;;; LZW (Lempel-Ziv-Welch) compression algorithm

(defun compress (input-bytes)
  (compress-algorithm (init-dict) 256 input-bytes nil))

(defun decompress (input-bytes)
  (let ((dict (reverse-dict (nth-value 1 (compress-algorithm (init-dict) 256 input-bytes nil)))))
    (mapcan (lambda (code) (gethash code dict)) input-bytes)))

(defun init-dict ()
  (let ((dict (make-hash-table :test 'equal))
        (codes (loop for code from 0 to 255 collect code)))
    (dolist (code codes)
      (add-to-dict (list code) code dict))
    dict))

(defun reverse-dict (dict)
  (let ((r-dict (make-hash-table :test 'eql)))
    (maphash (lambda (k v) (add-to-dict v k r-dict)) dict)
    r-dict))

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
  (cons new-code (cddr input-bytes)))

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
              (let  ((next-seq (update-input-bytes new-code input-bytes)))
                (if found
                    (compress-algorithm dict current-code next-seq output-bytes)
                    (compress-algorithm dict current-code next-seq (cons byte output-bytes)))))))
      (values (reverse output-bytes) dict)))
