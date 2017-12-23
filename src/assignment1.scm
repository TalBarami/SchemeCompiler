;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                ASSIGNMENT 1                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "src/pc.scm")

(define base-16 16)

(define base-10 10)

(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <english-lowercase>
  (range #\a #\z))

(define <english-uppercase>
  (range #\A #\Z))

(define <english-chars>
  (new (*parser <english-lowercase>)
       (*parser <english-uppercase>)
       (*disj 2)
       done))

(define <english-chars-to-lowercase>
  (new (*parser <english-lowercase>)
       (*parser <english-uppercase>)
       (*pack (lambda (char)
                (integer->char (+ (char->integer char) 32))))
       (*disj 2)
       done))

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    (new (*parser (char #\;))
         
         (*parser <any-char>)
         (*parser <end-of-line-comment>)
         *diff *star
         
         (*parser <end-of-line-comment>)
         (*caten 3)
         done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr-start>))
       (*caten 2)
       done))

(define <infix-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <infix-expression>))
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
        <infix-comment>
        <sexpr-comment>))

(define <skip>
  (disj <comment>
        <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
           (*parser <p>)
           (*parser <wrapper>)
           (*caten 3)
           
           (*pack-with
            (lambda (_left e _right) e))
           done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

(define ^dictionary
  (lambda (var val)
    (new (*parser (word-ci var))
         (*pack (lambda (_) val))
         done)))

(define (list->number list base)
  (fold-left (lambda (value digit)
               (+ (* value base) digit))
             0
             list))

(define (char->symbol ch)
  (string->symbol (string ch)))

(define <hex-char>
  (let ((zero (char->integer #\0))
        (lc-a (char->integer #\a))
        (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
         (*pack
          (lambda (ch)
            (- (char->integer ch) zero)))
         
         (*parser (range #\a #\f))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) lc-a))))
         
         (*parser (range #\A #\F))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) uc-a))))
         
         (*disj 3)
         done)))

(define ^prefix-expression
  (lambda (current next) 
    `(,(car next) ,current ,(cadr next))))

(define ^array-get
  (lambda (identifier index)
    `(vector-ref ,identifier ,index)))

(define ^function-call
  (lambda (identifier argument)
    `(,identifier ,@argument)))

(define ^boolean
  ^dictionary)

(define <boolean>
  (new (*parser (^boolean "#t" '#t))
       (*parser (^boolean "#f" '#f))
       (*disj 2)
       done))

(define <char-prefix>
  (new (*parser (word "#\\"))
       done))

(define <visible-simple-char>
  (new (*parser (range (integer->char 33) (integer->char 255)))
       done))

(define ^named-char
  ^dictionary)

(define <named-char>
  (new (*parser (^named-char "lambda" (integer->char 955)))
       (*parser (^named-char "newline" #\newline))
       (*parser (^named-char "nul" #\nul))
       (*parser (^named-char "page" #\page))
       (*parser (^named-char "return" #\return))
       (*parser (^named-char "space" #\space))
       (*parser (^named-char "tab" #\tab))
       (*parser (^named-char "duck" (integer->char 78189)))
       
       (*disj 8)
       done))

(define <hex-unicode-char>
  (new (*parser (char-ci #\x))
       
       (*parser <hex-char>) *plus
       (*pack
        (lambda (numbers-list)
          (integer->char
           (list->number numbers-list base-16))))
       
       (*caten 2)
       (*pack-with (lambda (_ char) char))
       done))

(define <char>
  (new (*parser <char-prefix>)
       
       (*parser <named-char>)
       (*parser <hex-unicode-char>)
       (*parser <visible-simple-char>)
       (*disj 3)
       
       (*caten 2)
       (*pack-with
        (lambda (prefix char)
          char))
       done))

(define <natural>
  (new (*parser (char #\0)) *plus
       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 3)
       (*pack-with
        (lambda (_ a s)
          (string->number
           (list->string
            `(,a ,@s)))))
       
       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
        (lambda (a s)
          (string->number
           (list->string
            `(,a ,@s)))))
       
       (*parser (char #\0)) *plus
       (*pack (lambda (_) 0))
       
       (*disj 3)
       done))

(define <integer>
  (new (*parser (char #\+))
       (*parser <natural>)
       (*caten 2)
       (*pack-with
        (lambda (++ n) n))
       
       (*parser (char #\-))
       (*parser <natural>)
       (*caten 2)
       (*pack-with
        (lambda (-- n) (- n)))
       
       (*parser <natural>)
       
       (*disj 3)
       done))

(define <fraction>
  (new (*parser <integer>)
       (*parser (char #\/))
       (*parser <natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
        (lambda (num div den)
          (/ num den)))
       done))

(define <number>
  (new (*parser <fraction>)
       (*parser <integer>)
       (*disj 2)
       
       (*delayed (lambda () <symbol>))
       (*parser (range #\0 #\9))
       *diff
       *not-followed-by
       done))

(define <string-literal-char>
  (new (*parser <any-char>)
       
       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)
       *diff
       done))

(define ^string-meta-char
  ^dictionary)

(define <string-meta-char>
  (new (*parser (^string-meta-char "\\\\" #\\))
       (*parser (^string-meta-char "\\\"" #\"))
       (*parser (^string-meta-char "\\t" #\tab))
       (*parser (^string-meta-char "\\f" #\page))
       (*parser (^string-meta-char "\\n" #\newline))
       (*parser (^string-meta-char "\\r" #\return))
       (*parser (^string-meta-char "\\duck" (integer->char 78189)))
       
       (*disj 7)
       done))

(define <string-hex-char>
  (new (*parser (word-ci "\\x"))
       (*parser <hex-char>) *plus
       (*parser (char #\;))
       
       (*caten 3)
       (*pack-with
        (lambda (backslash-x hex-chars-list semicolon)
          (integer->char
           (list->number hex-chars-list base-16))))
       
       done))

(define <string-char>
  (new (*parser <string-meta-char>)
       (*parser <string-literal-char>)
       (*parser <string-hex-char>)
       
       (*disj 3)
       done))

(define <string>
  (new (*parser (char #\"))
       (*parser <string-char>) *star
       (*parser (char #\"))
       (*caten 3)
       
       (*pack-with
        (lambda (open-delim chars close-delim)
          (list->string chars)))
       
       done))

(define <symbol-digit>
  <digit-0-9>)

(define <symbol-char>
  (new (*parser <english-chars-to-lowercase>)
       (*parser <symbol-digit>)
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\/))
       (*disj 14)
       
       done))

(define <symbol>
  (new (*parser <symbol-char>) *plus
       (*pack (lambda (chars)
                (string->symbol
                 (list->string
                  chars))))
       done))

(define <proper-list>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr-start>)) *star
       (*parser (char #\)))
       
       (*caten 3)
       (*pack-with
        (lambda (left-bracket body right-bracket) body))
       done))

(define <improper-list>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr-start>)) *plus
       (*parser (char #\.))
       (*delayed (lambda () <sexpr-start>))
       (*parser (char #\)))
       
       (*caten 5)
       (*pack-with
        (lambda (left-bracket first dot rest right-bracket)
          `(,@first ,@rest)))
       done))

(define <vector>
  (new (*parser (word "#("))
       (*delayed (lambda () <sexpr-start>)) *star
       (*parser (char #\)))
       
       (*caten 3)
       (*pack-with
        (lambda (hash-and-left-bracket body right-bracket)
          (list->vector body)))
       done))

(define <quoted>
  (new (*parser (char #\'))
       (*delayed (lambda () <sexpr-start>))
       
       (*caten 2)
       (*pack-with
        (lambda (quote-mark body)
          `',body))
       done))

(define <quasi-quoted>
  (new (*parser (char #\`))
       (*delayed (lambda () <sexpr-start>))
       
       (*caten 2)
       (*pack-with
        (lambda (quasi-quote-mark body)
          (list 'quasiquote body)))
       done))

(define <unquoted>
  (new (*parser (char #\,))
       (*delayed (lambda () <sexpr-start>))
       
       (*caten 2)
       (*pack-with
        (lambda (unquoted-mark body)
          (list 'unquote body)))
       done))

(define <unquoted-and-spliced>
  (new (*parser (word ",@"))
       (*delayed (lambda () <sexpr-start>))
       
       (*caten 2)
       (*pack-with
        (lambda (unquoted-and-spliced-mark body)
          (list 'unquote-splicing body)))
       done))

(define ^op ^dictionary)

(define <infix-extension-prefix>
  (new (*parser (word "##"))
       (*parser (word "#%"))
       
       (*disj 2)
       done))

(define <infix-sexpr-escape>
  (new (*parser <infix-extension-prefix>)
       (*delayed (lambda () <sexpr-start>))
       (*caten 2)
       (*pack-with
        (lambda (prefix sexpr) sexpr))
       
       done))

(define <infix-number>
  (new (*parser <fraction>)
       (*parser <integer>)
       (*disj 2)
       
       done))

(define <infix-symbol-char>
  (new (*parser <symbol-char>)
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (word "**"))
       (*parser (char #\+))
       (*parser (char #\/))
       (*disj 6)
       
       *diff
       done))

(define <infix-symbol>
  (new (*parser <symbol-digit>) *star
       (*parser <infix-symbol-char>) *plus
       (*caten 2)
       
       (*pack-with (lambda (digits chars)
                     (string->symbol
                      (list->string
                       (append digits chars)))))
       done))

(define <infix-token>
  (^<skipped*> 
   (disj <infix-sexpr-escape>
         <infix-symbol>
         <infix-number>
         )))

(define <infix-parenthesis>
  (new (*parser <skip>) *star
       (*parser (char #\())
       (*delayed (lambda () <infix-expression>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with
        (lambda (left-par exp right-par)
          exp))
       
       (*parser <skip>) *star
       (*caten 3)
       (*pack-with (lambda (left-space body right-space) body))
       
       (*parser <infix-token>)
       
       (*disj 2)
       
       done))

(define <infix-args-list>
  (new (*delayed (lambda () <infix-expression>))
       
       (*parser (char #\,))
       (*delayed (lambda () <infix-expression>))
       (*caten 2)
       (*pack-with
        (lambda (comma arg)
          arg))
       *star
       
       (*caten 2)
       (*pack-with
        (lambda (first rest)
          `(,first ,@rest)))
       
       (*parser <epsilon>)
       
       (*disj 2)
       done))

(define <infix-function-call>
  (new (*parser <skip>) *star
       (*parser <infix-parenthesis>)
       
       (*parser (char #\())
       
       (*parser <skip>) *star
       (*delayed (lambda () <infix-args-list>))
       (*parser <skip>) *star
       (*caten 3)
       (*pack-with (lambda (left-space body right-space) body))
       
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (left-bracket exp right-bracket) exp))
       *plus
       
       (*caten 2)
       (*pack-with
        (lambda (identifier arguments)
          (fold-left ^function-call
                     identifier
                     arguments)))
       
       (*parser <skip>) *star
       (*caten 3)
       (*pack-with (lambda (left-space body right-space) body))
       
       (*parser <infix-parenthesis>)
       
       (*disj 2)
       done))

(define <infix-array-get>
  (new (*parser <skip>) *star
       (*parser <infix-function-call>)
       
       (*parser (char #\[))
       (*delayed (lambda () <infix-expression>))
       (*parser (char #\]))
       (*caten 3)
       (*pack-with (lambda (left-bracket exp right-bracket) exp))
       *plus
       
       (*caten 2)
       (*pack-with
        (lambda (identifier references)
          (fold-left ^array-get
                     identifier
                     references)))
       
       (*parser <skip>) *star
       (*caten 3)
       (*pack-with (lambda (left-space body right-space) body))
       
       
       (*parser <infix-function-call>)
       
       (*disj 2)
       done))

(define <infix-neg>
  (new (*parser <skip>) *star
       
       (*parser (char #\-)) *plus
       (*parser (char #\space)) *star
       (*parser <infix-array-get>)
       (*caten 3)
       
       (*pack-with
        (lambda (signs whitespace expression)
          (if (and (number? expression)
                   (null? whitespace))
              (fold-left (lambda (current next)
                           `(- ,current))
                         (- expression)
                         (cdr signs))
              (fold-left (lambda (current next)
                           `(- ,current))
                         expression
                         signs))))
       
       (*parser <skip>) *star
       (*caten 3)
       (*pack-with (lambda (left-space body right-space) body))
       
       (*parser <infix-array-get>)
       
       (*disj 2)
       done))


(define <infix-pow>
  (new (*parser <infix-neg>)
       (*parser (char #\^))
       (*parser (word "**"))
       (*disj 2)
       (*parser <infix-neg>)
       (*caten 2)
       (*pack-with
        (lambda (op num) num))
       *star
       
       (*caten 2)
       (*pack-with
        (lambda (first rest)
          (if (null? rest)
              first
              (letrec ((^<infix-pow> (lambda (current rest)
                                       (if (null? (cdr rest))
                                           `(expt ,current ,@rest)
                                           `(expt ,current ,(^<infix-pow> (car rest) (cdr rest)))))))
                (^<infix-pow> first rest)))))
       done))

(define <infix-mul-div>
  (new (*parser <infix-pow>)
       (*parser (^op "*" '*))
       (*parser (^op "/" '/))
       (*disj 2)
       (*parser <infix-pow>)
       (*caten 2)
       *star
       
       (*caten 2)
       (*pack-with
        (lambda (first rest)
          (fold-left ^prefix-expression first rest)))
       done))

(define <infix-add-sub>
  (new (*parser <infix-mul-div>)
       (*parser (^op "+" '+))
       (*parser (^op "-" '-))
       (*disj 2)
       (*parser <infix-mul-div>)
       (*caten 2)
       *star
       
       (*caten 2)
       (*pack-with
        (lambda (first rest)
          (fold-left ^prefix-expression first rest)))
       done))

(define <infix-expression>
  (^<skipped*> <infix-add-sub>))

(define <infix-extension>
  (new (*parser <infix-extension-prefix>)
       (*parser <infix-expression>)
       
       (*caten 2)
       (*pack-with
        (lambda (prefix expression)
          expression))
       
       done))

(define <sexpr-start>
  (^<skipped*>
   (disj <boolean>
         <char>
         <number>
         <string>
         <symbol>
         <proper-list>
         <improper-list>
         <vector>
         <quoted>
         <quasi-quoted>
         <unquoted>
         <unquoted-and-spliced>
         <infix-extension>
         )))

(define <sexpr> <sexpr-start>)