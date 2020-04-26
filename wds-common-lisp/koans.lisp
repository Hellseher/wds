(defpackage "cl-koans"
  (:use cl))

(in-package :cl-koans)

(defun recursive-path (path &key follow-symlinks)
  "Walk through each elements of the PATH and return a list of recursivly found
elemens as absolute pathes to them."
  (let ((collection))
    (fad:walk-directory path (lambda (name) (push name collection)))
  collection))

(defun path-filter (paths patterns)
  "Filter PATHS agains each element from PATTERNS, reunturn a list of all
  matched."
  (mapcar #'ppcre:scan ))
