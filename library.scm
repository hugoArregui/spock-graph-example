(define (add1 i)
  (+ i 1))

(define (foldl kons seed l)
  (if (null? l)
    seed
    (foldl kons (kons seed (car l)) (cdr l))))

;; interact with js Arrays
(define Array-set!
  (%native-lambda
    "var array = arguments[1];"
    "var index = arguments[2];"
    "var value = arguments[3];"
    "array[index] = value;"
    "return K(value);"))

(define Array-get
  (%native-lambda
    "var array = arguments[1];"
    "var index = arguments[2];"
    "var value = array[index];"
    "return K(value);"))

(define (list->Array l)
  (let ((array (new (%host-ref 'Array) (length l))))
    (foldl (lambda (i x) 
             (Array-set! array i x)
             (add1 i))
           0 
           l)
    array))

(define (debug . m) 
  (%inline "console.log.apply" (%host-ref "this") (list->Array m)))

