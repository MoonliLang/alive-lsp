(defpackage :alive/sys/eval
    (:use :cl)
    (:export :*read-function-alist*
             :eval-fn
             :from-string)
    (:local-nicknames (:astreams :alive/sys/streams)
                      (:deps :alive/deps)
                      (:pkgs :alive/packages)))

(in-package :alive/sys/eval)

(defvar *last-read-function* #'read)

(defvar *read-function-alist*
  (list (cons :lisp #'read)
        (cons :cl #'read)
        (cons :asd #'read)
        (cons :asdf #'read)))
(declaim (ftype (function (stream (or null keyword)) *) eval-fn))
(defun eval-fn (input lang)
  (let ((read-function (if lang
                           (cdr (assoc lang *read-function-alist*))
                           *last-read-function*)))
    (setf *last-read-function* read-function)
    (eval (funcall read-function input))))


(declaim (ftype (function (deps:dependencies string &key
                                             (:pkg-name string)
                                             (:lang (or null keyword))
                                             (:stdin-fn function)
                                             (:stdout-fn function)
                                             (:stderr-fn function)
                                             (:query-fn function)
                                             (:trace-fn function)) *) from-string))
(defun from-string (deps str &key pkg-name lang stdin-fn stdout-fn stderr-fn query-fn trace-fn)
    (astreams:with-redirect-streams (:stdin-fn stdin-fn :stdout-fn stdout-fn :stderr-fn stderr-fn :query-fn query-fn :trace-fn trace-fn)
        (let* ((input (make-string-input-stream str))
               (pkg (pkgs:lookup pkg-name))
               (*package* (if pkg pkg *package*)))
            (when (and pkg-name (not pkg))
                  (error (make-condition 'pkgs:package-not-found :name pkg-name)))

            (deps:do-eval deps input lang))))
