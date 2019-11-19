(in-package :vico-lib)

;;; graphemes - portable version of the grapheme breaking algorithm in sb-unicode

(defun %binary-search (value seq key)
  (declare (simple-vector seq))
  (declare (function key))
  (labels ((recurse (start end)
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (elt (svref seq i))
                      (key-value (funcall key elt)))
                 (cond ((< value key-value)
                        (recurse start i))
                       ((> value key-value)
                        (recurse (1+ i) end))
                       (t
                        i))))))
    (recurse 0 (length seq))))

(defun binary-search (value seq &key (key #'identity))
  "Binary search for simple vectors."
  (let ((index (%binary-search value seq key)))
    (if index
        (svref seq index))))

(defun hangul-syllable-type (character)
  "Returns the Hangul syllable type of CHARACTER.
The syllable type can be one of :L, :V, :T, :LV, or :LVT.
If the character is not a Hangul syllable or Jamo, returns NIL"
  (let ((cp (char-code character)))
    (cond
      ((or
        (and (<= #x1100 cp) (<= cp #x115f))
        (and (<= #xa960 cp) (<= cp #xa97c))) :L)
      ((or
        (and (<= #x1160 cp) (<= cp #x11a7))
        (and (<= #xd7B0 cp) (<= cp #xd7C6))) :V)
      ((or
        (and (<= #x11a8 cp) (<= cp #x11ff))
        (and (<= #xd7c8 cp) (<= cp #xd7fb))) :T)
      ((and (<= #xac00 cp) (<= cp #xd7a3))
       (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT)))))

(defun grapheme-break-class (char)
  "Returns the grapheme breaking class of CHARACTER, as specified in UAX #29."
  (let ((cp (when char (char-code char)))
        (gc (when char (cl-unicode:general-category char)))
        (not-spacing-mark
          #(#x102B #x102C #x1038 #x1062 #x1063 #x1064 #x1067 #x1068 #x1069
            #x106A #x106B #x106C #x106D #x1083 #x1087 #x1088 #x1089 #x108A
            #x108B #x108C #x108F #x109A #x109B #x109C #x19B0 #x19B1 #x19B2
            #x19B3 #x19B4 #x19B8 #x19B9 #x19BB #x19BC #x19BD #x19BE #x19BF
            #x19C0 #x19C8 #x19C9 #x1A61 #x1A63 #x1A64 #xAA7B #xAA7D)))
    (cond
      ((not char) nil)
      ((= cp 10) :LF)
      ((= cp 13) :CR)
      ((or (member gc '("Mn" "Me") :test #'string=)
           (cl-unicode:has-binary-property char "Other_Grapheme_Extend"))
       :extend)
      ((or (member gc '("Zl" "Zp" "Cc" "Cs" "Cf") :test #'string=)
           ;; From Cn and Default_Ignorable_Code_Point
           (eql cp #x2065) (eql cp #xE0000)
           (<= #xFFF0 cp #xFFF8)
           (<= #xE0002 cp #xE001F)
           (<= #xE0080 cp #xE00FF)
           (<= #xE01F0 cp #xE0FFF)) :control)
      ((<= #x1F1E6 cp #x1F1FF) :regional-indicator)
      ((and (or (string= gc "Mc")
                (eql cp #x0E33) (eql cp #x0EB3))
            (not (binary-search cp not-spacing-mark))) :spacing-mark)
      (t (hangul-syllable-type char)))))

(defun make-grapheme-searcher (sequence &key (start 0) from-end
                                          (length (length sequence))
                                          (accessor #'schar))
  "Returns a stateful object that can be used by calling NEXT-GRAPHEME to obtain
offsets of graphemes starting from START (non-inclusive) and proceeding forwards
if FROM-END is NIL and backwards if FROM-END is T in the sequence SEQUENCE.
Elements will be accessed by calling ACCESSOR, which defaults to SCHAR (argument
coerced to SIMPLE-STRING). LENGTH defaults to (length sequence) and is used to
determine when to terminate."
  (let ((sequence (cl-ppcre-custom::maybe-coerce-to-simple-string sequence))
        (step (if from-end -1 1))
        (c1 nil)
        (c2 (and (>= start 0) (< start length) ; bounds check before accessing
                 (grapheme-break-class (funcall accessor sequence start))))
        (end start))
    (lambda ()
      (loop
        (incf end step)
        (when (or (and from-end (< end 0)) (>= end length))
          (return nil))
        (shiftf c1 c2 (grapheme-break-class (funcall accessor sequence end)))
        (cond
          ((and (eql c1 :cr) (eql c2 :lf)))
          ((or (member c1 '(:control :cr :lf))
               (member c2 '(:control :cr :lf)))
           (setf start end) (return end))
          ((or (and (eql c1 :l) (member c2 '(:l :v :lv :lvt)))
               (and (or (eql c1 :v) (eql c1 :lv))
                    (or (eql c2 :v) (eql c2 :t)))
               (and (eql c2 :t) (or (eql c1 :lvt) (eql c1 :t)))))
          ((and (eql c1 :regional-indicator) (eql c2 :regional-indicator)))
          ((eql c2 :extend))
          ((or (eql c2 :spacing-mark) (eql c1 :prepend)))
          (t (setf start end) (return end)))))))

(defun search-next-grapheme (searcher)
  "Returns the next grapheme found by SEARCHER or if SEARCHER has already
searched the entire string, NIL."
  (funcall searcher))

(defun list-graphemes (string)
  "Breaks STRING into graphemes according to the default grapheme breaking rules
specified in UAX #29, returning a list of strings. The last element may not be a
grapheme."
  (loop :with string = (coerce string 'simple-string)
        :with searcher = (make-grapheme-searcher string)
        :with list = (list)
        :with last = 0
        :for idx = (funcall searcher)
        :while idx
        :do (push (subseq string last idx) list)
            (setf last idx)
        :finally (push (subseq string last) list)
                 (return (nreverse list))))

;; (defun map-grapheme-boundaries (function string)
;;   (do ((length (length string))
;;        (start 0)
;;        (end 1 (1+ end))
;;        (c1 nil)
;;        (c2 (and (> (length string) 0) (grapheme-break-class (char string 0)))))
;;       ((>= end length)
;;        (if (= end length) (progn (funcall function string start end) nil)))
;;     (flet ((brk () (funcall function string start end) (setf start end)))
;;       (declare (dynamic-extent #'brk))
;;       (shiftf c1 c2 (grapheme-break-class (char string end)))
;;       (cond
;;         ((and (eql c1 :cr) (eql c2 :lf)))
;;         ((or (member c1 '(:control :cr :lf))
;;              (member c2 '(:control :cr :lf)))
;;          (brk))
;;         ((or (and (eql c1 :l) (member c2 '(:l :v :lv :lvt)))
;;              (and (or (eql c1 :v) (eql c1 :lv))
;;                   (or (eql c2 :v) (eql c2 :t)))
;;              (and (eql c2 :t) (or (eql c1 :lvt) (eql c1 :t)))))
;;         ((and (eql c1 :regional-indicator) (eql c2 :regional-indicator)))
;;         ((eql c2 :extend))
;;         ((or (eql c2 :spacing-mark) (eql c1 :prepend)))
;;         (t (brk))))))

;; (defun map-graphemes (function string)
;;   (let ((array (make-array 0 :element-type (array-element-type string)
;;                              :adjustable t
;;                              :displaced-to string)))
;;     (flet ((fun (string start end)
;;              (declare (type string string))
;;              (funcall function (adjust-array array (- end start) :displaced-to string
;;                                                                  :displaced-index-offset start))))
;;       (declare (dynamic-extent #'fun))
;;       (map-grapheme-boundaries #'fun string))))

;; (defun graphemes (string)
;;   "Breaks STRING into graphemes according to the default
;; grapheme breaking rules specified in UAX #29, returning a list of strings."
;;   (let (result)
;;     (map-graphemes (lambda (a) (push (subseq a 0) result)) string)
;;     (nreverse result)))
