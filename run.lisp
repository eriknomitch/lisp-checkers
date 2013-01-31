;; -----------------------------------------------
;; UTILITY ---------------------------------------
;; -----------------------------------------------
(defun make-index-list (length)
  (let ((index -1))
    (map-into (make-list length)
              (lambda ()
                (incf index)))))

;; -----------------------------------------------
;; CONSTANTS -------------------------------------
;; -----------------------------------------------
(defconstant +board-size+ 8)

;; -----------------------------------------------
;; GENERICS --------------------------------------
;; -----------------------------------------------
(defgeneric board-find-checker (board x y))
(defgeneric board-display (board))
(defgeneric board-lookup (board x y))
(defgeneric checker-status (checker))

;; -----------------------------------------------
;; CLASS->CHECKER --------------------------------
;; -----------------------------------------------
(defclass checker ()
  ((x :accessor checker-x
      :initarg :x
      :initform 0)
   (y :accessor checker-y
      :initarg :y
      :initform 0)
   (color :accessor checker-color
          :initarg :color)
   (king :accessor checker-king
         :initarg :king
         :initform nil)))

;; Methods
;; - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod checker-status ((checker checker))
  (if (checker-king checker)
    (if (eq (checker-color checker) :black) "K" "k")
    (if (eq (checker-color checker) :black) "o" "x")))

;; Functions
;; - - - - - - - - - - - - - - - - - - - - - - - -
(defun make-checker (&rest make-instance-args)
  (apply #'make-instance
         (append '(checker) make-instance-args)))

(defun make-checkers-for-row (row color &optional (offset 0))
  (let ((checkers '()))
    (dotimes (x +board-size+)
      (when (= (mod x 2) 0)
        (push (make-checker :x (+ x offset) :y row :color color)
              checkers)))
    checkers))

(defun make-checkers-for-color (start color &optional (mod-offset 0))
  (let ((checkers '()))
    (dotimes (row 3)
      (setf checkers 
            (append checkers 
                    (make-checkers-for-row (+ row start) color (mod (+ row mod-offset) 2)))))
    checkers))

(defun make-checkers-for-board ()
  (append
    (make-checkers-for-color 0 :red)
    (make-checkers-for-color 5 :black 1)))

;; -----------------------------------------------
;; CLASS->BOARD ----------------------------------
;; -----------------------------------------------
(defclass board ()
  ;; Board "has many" checkers.
  ((checkers :accessor board-checkers
             :initarg :checkers
             :initform (make-checkers-for-board))))

;; Methods
;; - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod board-find-checker ((board board) x y)
  (find-if #'(lambda (checker)
               (and (= (checker-x checker) x)
                    (= (checker-y checker) y)))
           (board-checkers board)))

(defmethod board-lookup ((board board) x y)
  (format nil "~a  "
    (let ((checker (board-find-checker board x y)))
      (if checker
        (checker-status checker)
        "-"))))

(defmethod board-display ((board board))
  (dotimes (x +board-size+)
    (dotimes (y +board-size+)
      (format t "~a" (board-lookup board x y)))
    (format t "~%")))

;; -----------------------------------------------
;; MAIN ------------------------------------------
;; -----------------------------------------------
(defvar *board* (make-instance 'board))

(board-display *board*)

