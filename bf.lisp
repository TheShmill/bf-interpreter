(defun scanner (input)
  (let ((index 0))
    (lambda ()
      (if (>= index (length input))
          nil
          (let ((char (elt input index)))
            (incf index)
            char)))))

(defun advance (scanner)
  (funcall scanner))

(defun parse-bf (source)
  (let ((scanner (scanner source)))
    (loop for c = (advance scanner)
          until (not c)
          collecting (parse-op c scanner))))

(defun parse-op (c scanner)
  (case c
    (#\+ 'add)
    (#\- 'sub)
    (#\> 'right)
    (#\< 'left)
    (#\. 'print)
    (#\, 'read)
    (#\[ (parse-loop scanner))
    (#\] (error "unmatched ]"))))

(defclass state ()
  ((buffer :accessor state-buffer
           :initform (make-array 4000 :initial-element 0))
   (head :accessor state-head
         :initform 0)))

(defun parse-loop (scanner)
  (loop for c = (advance scanner)
        while (not (eq c #\]))
        if (null c)
          do (error "unmatched [")
        collecting (parse-op c scanner)))

(defun interpret-program (ops)
  (let ((state (make-instance 'state)))
    (loop for op in ops
          do (interpret-instruction op state))
    state))

(defun interpret-instruction (op state)
  (if (consp op)
      (interpret-loop op state)
      (case op
        (add (incf (elt (state-buffer state) (state-head state))))
        (sub (decf (elt (state-buffer state) (state-head state))))
        (right (setf (state-head state)
                     (mod (1+ (state-head state))
                          (length (state-buffer state)))))
        (left (setf (state-head state)
                    (mod (1- (state-head state))
                         (length (state-buffer state)))))
        (print (format t "~a" (code-char (elt (state-buffer state)
                                              (state-head state)))))
        (read (setf (elt (state-buffer state) (state-head state))
                    (char-code (read-char)))))))

(defun interpret-loop (ops state)
  (loop until (= 0 (elt (state-buffer state) (state-head state)))
        do (loop for op in ops
                 do (interpret-instruction op state))))

(defun run-bf (source)
  (interpret-program (parse-bf source)))
