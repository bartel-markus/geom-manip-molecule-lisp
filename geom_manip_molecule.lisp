;;;; geom_manip_molecule.lisp

(in-package #:geom_manip_molecule)

(defvar *masses* '(("x" . 0.000)
                   ("h" . 1.008)
                   ("he" . 4.003)
                   ("li" . 7.016)
                   ("be" . 9.012)
                   ("b" . 11.009)
                   ("c" . 12.000)
                   ("n" . 14.003)
                   ("o" . 15.995)
                   ("f" . 18.998)
                   ("ne" . 19.992)
                   ("na" . 22.990)
                   ("mg" . 23.985)
                   ("al" . 26.981)
                   ("si" . 27.977)
                   ("p" . 30.974)
                   ("s" . 31.972)
                   ("cl" . 34.969)
                   ("ar" . 39.962)
                   ("k" . 38.964)
                   ("ca" . 39.963)
                   ("sc" . 44.956)
                   ("ti" . 47.948)
                   ("v" . 50.944)
                   ("cr" . 51.941)
                   ("mn" . 54.938)
                   ("fe" . 55.935)
                   ("co" . 58.933)
                   ("ni" . 57.935)
                   ("cu" . 62.930)
                   ("zn" . 63.929)
                   ("ga" . 68.926)
                   ("ge" . 73.921)
                   ("as" . 74.922)
                   ("se" . 79.917)
                   ("br" . 78.918)
                   ("kr" . 83.912)
                   ("rb" . 84.912)
                   ("sr" . 87.906)
                   ("y" . 88.906)
                   ("zr" . 89.905)
                   ("nb" . 92.906)
                   ("mo" . 97.905)
                   ("tc" . 98.906)
                   ("ru" . 101.904)
                   ("rh" . 102.906)
                   ("pd" . 107.904)
                   ("ag" . 106.905)
                   ("cd" . 113.903)
                   ("in" . 114.904)
                   ("sn" . 119.902)
                   ("sb" . 120.904)
                   ("te" . 129.906)
                   ("i" . 126.904)
                   ("xe" . 131.904)))

(defmacro ->> (var &rest lst)
  (if (cdr lst)
      `(->> (,@(car lst) ,var) ,@(cdr lst))
      `(     ,@(car lst) ,var)))

(defun remove-empty-strings (lst)
  (remove-if
   #'(lambda (element)
       (string= "" element))
   lst))

(defun extract-moldata (filepath)
  (->>
   (uiop:read-file-lines filepath)
   (nthcdr 2)
   (remove-empty-strings)
   (mapcar (lambda (line)
             (remove-empty-strings (uiop:split-string line :separator " "))))))

(defclass molecular-structure ()
  ((filepath :initarg :filepath :reader filepath)
   (atoms-list :accessor atoms-list)
   (coord-arr :accessor coord-arr)))

(defmethod initialize-instance :after ((mol molecular-structure) &rest initargs)
  (with-slots (filepath atoms-list coord-arr) mol
    (let ((moldata (extract-moldata filepath)))
      (setf atoms-list (mapcar #'car moldata))
      (setf coord-arr (numcl:asarray
                       (mapcar #'(lambda (row)
                                   (cdr (mapcar #'read-from-string row)))
                               moldata))))))

(defmethod no-of-atoms ((mol molecular-structure))
  (length (atoms-list mol)))

(defmethod total-mass ((mol molecular-structure))
  (loop for atom-symbol in (atoms-list mol)
        sum (cdr (assoc (string-downcase atom-symbol) *masses* :test #'string=))))

(defun remove-rows (array rows-to-remove)
  (destructuring-bind (num-rows num-columns) (numcl:shape array)
    ;; (magicl:from-list (loop for i below num-rows
    ;;                         unless (member i rows-to-remove) nconc
    ;;                                                          (loop for c below num-columns collect
    ;;                                                                                        (magicl:tref array i c)))
    ;;                   `(,(- num-rows (length rows-to-remove))
    ;;                     ,num-columns))
    ))

(defmethod delete-atoms ((mol molecular-structure) &rest atom-indices)
  (let ((indices-zero-indexed (mapcar #'1- atom-indices)))
    (with-slots (atoms-list coord-arr) mol
      ;; delete atom symbols from atoms array
      (mapc #'(lambda (ind) (delete (elt atoms-list ind) atoms-list)) indices-zero-indexed)
      ;; (loop for ind in indices-zero-indexed
      ;;       collecting (elt atoms-list ind) into refs
      ;;       do (mapc #'(lambda (e) (delete e atoms-list)) refs))
      ;; modify coordinate array
      ;; (setf coord-arr (remove-rows coord-arr indices-zero-indexed))
      )))

(defmethod get-coord-by-no ((mol molecular-structure) N)
  (with-slots (coord-arr) mol
    (numcl:aref coord-arr (1- N))))

(defvar mol7 (make-instance 'molecular-structure :filepath  "~/Downloads/Urea.xyz"))
