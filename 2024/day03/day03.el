;; searches the buffer for the next instance of a valid mul string and computes the product or returns nil
(defun next-mul-result () 
  (if (re-search-forward "mul(\\([1-9][0-9]*\\),\\([1-9][0-9]*\\))" nil t) ; look for next mul expression
      (* (string-to-number (match-string 1)) (string-to-number (match-string 2))) ; if found return product
    nil)) ; otherwise return nil

;; add all mul results together until reaching a nil
(defun sum-of-muls () 
  (let ((next-val (next-mul-result)) (total 0)) ; create local vars next-val and total
    (while next-val ; while next val is not nil
      (setq total (+ total next-val)) ; add it to total
      (setq next-val (next-mul-result))) ; then update next val
    total)) ; return total

;; return the answer for part 1
(defun mull-it-over-part1 (filename)
  (with-temp-buffer ; create a temporary buffer which will hold our file contents
    (insert-file-contents filename) ; load contents of filename into buffer
    (sum-of-muls))) ; return sum

;; searches buffer for next instruction, "do()", "don't()", or "mul(X,Y)"
;; if found, returns the instruction as a string
;; if not found, return nil
(defun next-inst ()
    (if (re-search-forward "mul([0-9]+,[0-9]+)\\|do()\\|don't()" nil t)
        (match-string 0) nil))

;; return the answer for part 2
(defun mull-it-over-part2 (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    ;; create local vars
    (let ((inst (next-inst)) (enabled t) (total 0))
      (while inst ; while a next instruction exists
        (if (string= inst "do()")
            (progn (setq enabled t)) ; enable adding 
          (if (string= inst "don't()")
              (progn (setq enabled nil)) ; disable adding

            (progn
              (string-match "mul(\\([1-9][0-9]*\\),\\([1-9][0-9]*\\))" inst)
              (if enabled
                  (progn
                    (message "hi")
                    (setq total (+ total (* (string-to-number (match-string 1 inst)) (string-to-number (match-string 2 inst))))))
                )
              )
            )
          )
        (setq inst (next-inst))
        )
      total
    )))

(setq input-file "input.txt")
(message "part1: sum of muls = %d" (mull-it-over-part1 input-file))
(message "part2: sum of muls = %d" (mull-it-over-part2 input-file))
