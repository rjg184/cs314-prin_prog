; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

;; (load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;; Short definition for empty list
(define null '())

;; Gets the position of an item from a list
(define pos
  (lambda (item list)
    (cond
      ((null? list) 0)
      ((equal? item (car list)) 0)
      (else (+ 1 (pos item (cdr list)) )))
    ))

;; Determines the number of occurences of a given letter (a-z) in a given list
(define num_of_occur
  (lambda (letter)
    (lambda (list)
      (cond
        ((null? list) 0)
        ((equal? (car list) letter)
         (+ 1 ((num_of_occur letter) (cdr list))))
        (else ((num_of_occur letter) (cdr list))))
      )))

;; Determines the number of occurences of '#t' in a given list
(define occur-t_bool
  (lambda (list)
      (cond
        ((null? list) 0)
        ((equal? #t (car list))
         (+ 1 (occur-t_bool (cdr list))))
        (else (occur-t_bool (cdr list))))
    ))

;; Determines/evaluates the number of occurrences of each letter in a given paragraph (list of words)
(define evalOccurence
  (lambda (letter p)
    (reduce + (map (num_of_occur letter)(reduce append (list p) null)) 0)
    ))

;; Checks if each word in paragraph p (list of words) is in the provided dictionary
(define check-p
  (lambda (paragraph)
    (map (lambda (x) (spell-checker x)) paragraph)
    ))

;; Encodes a paragraph p (list of words) by n (given distance shift)
(define enc
  (lambda (paragraph n)
    (map (lambda (x) ((encode-n n) x)) paragraph)
    ))

;; Shifts any letter to the letter 'e'
(define calc_shift-e
  (lambda (x)
    (+ (- 26 x) 4) ))

;; Determines/checks if any of the shifted words are valid words in the dictionary
(define checkWords
  (lambda (p)
    (map occur-t_bool
         (list
          (check-p (enc p 0)  )
          (check-p (enc p 1)  )
          (check-p (enc p 2)  )
          (check-p (enc p 3)  )
          (check-p (enc p 4)  )
          (check-p (enc p 5)  )
          (check-p (enc p 6)  )
          (check-p (enc p 7)  )
          (check-p (enc p 8)  )
          (check-p (enc p 9)  )
          (check-p (enc p 10) )
          (check-p (enc p 11) )
          (check-p (enc p 12) )
          (check-p (enc p 13) )
          (check-p (enc p 14) )
          (check-p (enc p 15) )
          (check-p (enc p 16) )
          (check-p (enc p 17) )
          (check-p (enc p 18) )
          (check-p (enc p 19) )
          (check-p (enc p 20) )
          (check-p (enc p 21) )
          (check-p (enc p 22) )
          (check-p (enc p 23) )
          (check-p (enc p 24) )
          (check-p (enc p 25) )
          ))))

;; Creates a list that contains the number of occurrences of each letter in a given paragraph (list of words)
(define checkFreq
  (lambda (p)
    (list
     (evalOccurence 'a p)
     (evalOccurence 'b p)
     (evalOccurence 'c p)
     (evalOccurence 'd p)
     (evalOccurence 'e p)
     (evalOccurence 'f p)
     (evalOccurence 'g p)
     (evalOccurence 'h p)
     (evalOccurence 'i p)
     (evalOccurence 'j p)
     (evalOccurence 'k p)
     (evalOccurence 'l p)
     (evalOccurence 'm p)
     (evalOccurence 'n p)
     (evalOccurence 'o p)
     (evalOccurence 'p p)
     (evalOccurence 'q p)
     (evalOccurence 'r p)
     (evalOccurence 's p)
     (evalOccurence 't p)
     (evalOccurence 'u p)
     (evalOccurence 'v p)
     (evalOccurence 'w p)
     (evalOccurence 'x p)
     (evalOccurence 'y p)
     (evalOccurence 'z p)
     )))

;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
    (cond
      ((member w dictionary) #t) ;; checks if given word is a member of dictionary list
      (else #f))
   ))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
      ;; Encript(x) = vtl( (ltv(x) + n) mod26)
      (map vtl
           (map (lambda (x) (modulo (+ x n) 26))
                (map (lambda (x) (ltv x)) w)))
      )))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (map (lambda (x) (map encoder x)) d)
    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
     (define gen-dec-a
      (lambda (n)
        (encode-n n) ))
    (gen-dec-a (pos (apply max (checkWords p)) (checkWords p) ))
    ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    (define gen-dec-b
      (lambda (n)
        (encode-n n) ))
    (gen-dec-b (calc_shift-e (pos (apply max (checkFreq p)) (checkFreq p) )))
    ))

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    (encode-d d decoder)
    ))

;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)