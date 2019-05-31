(define template-c-file "c/template.c")

(define (c-template)
  (read-file template-c-file))

;;replace //CODE in template with string
(define (template-replace str)
  (find-and-replace (c-template) "//CODE" str))

;; assume no overlaps
(define (find-and-replace base replace with)
  (let ((index (string-search-forward replace base)))
    (if (not index)
        base
        (string-append (string-slice base 0 index)
                       with
                       (find-and-replace (string-slice base (+ index (string-length replace)))
                                         replace with)))))

;; (find-and-replace "hello" "he" "she")
;; ;Value: "shello"

;; (find-and-replace "glob slob mob" "ob" "oboe")
;; ;Value: "globoe sloboe moboe"
