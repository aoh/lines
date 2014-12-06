#!/usr/bin/ol --run

(import
   (owl args))

(define (lex<? a b)
   (cond
      ((null? a) #true)
      ((null? b) #false)
      ((lesser? (car a) (car b)) #true)
      ((eq? (car a) (car b))
         (lex<? (cdr a) (cdr b)))
      (else #false)))

;; store is ff of byte → ff', list ending here stored at #false
(define (intern store original-lst)

   ;; #false = blacklisted, 0 = not found, or unique object (byte list)
   (define (find store lst)
      (cond
         ((eq? store #empty) 0)
         ((null? lst)
            (get store #false 0))
         (else
            (find (get store (car lst) #empty) (cdr lst)))))

   (define (save store lst val)
      (if (null? lst)
         (put store #false val)
         (put store (car lst)
            (save (get store (car lst) #empty) (cdr lst) val))))

   (let ((val (find store original-lst)))
      (if (eq? val 0)
         (values (save store original-lst original-lst) original-lst)
         (values store val))))

(define (read-block fd)
   (get-block fd #x7fff))

(define (read-file-quick store path noter)
   (let ((fd (if (equal? path "-") stdin (open-input-file path))))
      (if fd
         (let loop ((store store) (block #false) (pos 0) (end 0) (cs null) (ls #empty))
            (if (eq? pos end)
               (lets ((next (read-block fd)))
                  (if (eof? next)
                     (if (null? cs)
                        (begin
                           (if (not (eq? fd stdin))
                              (close-port fd))
                           ;; leaves are sorted
                           (values store (ff-foldr (λ (out k v) (cons k out)) null ls)))
                        (begin
                           (noter "Warning: no newline at end of file: " path)
                           (loop store (vector 10) 0 1 cs ls)))
                     (loop store next 0 (sizeb next) cs ls)))
               (lets
                  ((c (refb block pos))
                   (pos _ (fx+ pos 1)))
                  (if (eq? c 10)
                     (lets ((store line (intern store cs)))
                        (if line
                           (loop store block pos end null (put ls line #true)) ;; could also count if necessary
                           (loop store block pos end null ls)))
                     (loop store block pos end (cons c cs) ls)))))
         (begin
            (print-to stderr "ERROR: cannot read " path)
            (values store #false)))))

;; set all leaf values to #false 
(define (blacken ff)
   (ff-fold
      (λ (this k v)
         (if k
            (fupd this k (blacken v))
            (fupd this k #false)))
      ff ff))

(define (maybe-read-blacklist noter pathp)
   (lets/cc fail
      ((store
         (fold 
            (λ (store path)
               (lets
                  ((store ls (read-file-quick store path noter)))
                  (if ls
                     (begin
                        (noter "blacklisted " (length ls) " lines from " path)
                        store)
                     (begin
                        (print-to stderr "ERROR: failed to read blacklist: " path)
                        (fail #false)))))
            #empty pathp)))
      (blacken store)))

;; ((path . sorted-interned-lines) ...)
(define (read-files store paths noter)
   (lets
      ((count (length paths))
       (start (time-ms)))
      (let loop ((store store) (paths paths) (done null) (n 1))
         (if (null? paths)
            ;; no need to intern more, lines are now eq?
            (reverse done)
            (lets
               ((store this (read-file-quick store (car paths) noter))
                (node (cons (car paths) this))
                (elapsed (+ 1 (- (time-ms) start)))
                (avg (floor (/ elapsed n)))
                (left-est (floor (/ (* avg (- count n)) 1000)))
                (m s (quotrem (floor left-est) 60)))
               (if this
                  (begin
                     (noter n "/" count " (" (length (cdr node)) "): " (car paths) ", left " m ":" (if (< s 10) "0" "") s)
                     (loop store (cdr paths) (cons node done) (+ n 1)))
                  #false))))))

(define (substract node best)
   (define (sub a b)
      (cond
         ((null? b) a)
         ((null? a) a)
         (else
            (let ((a1 (car a)) (b1 (car b)))
               (cond
                  ((eq? a1 b1)
                     (sub (cdr a) (cdr b)))
                  ((lesser? a1 b1)
                     (cons (car a)
                        (sub (cdr a) b)))
                  (else
                     (sub a (cdr b))))))))
   (cons (car node) ;; keep name
      (sub (cdr node) (cdr best)))) ;; ignore name

(define (select-biggest nodes)
   (let loop ((nodes (cdr nodes)) (best (car nodes)) (len (length (car nodes))) (worse null))
      (if (null? nodes)
         (values worse best)
         (let ((this-len (length (car nodes))))
            (if (> this-len len)
               (loop (cdr nodes) (car nodes) this-len (cons best worse))
               (loop (cdr nodes) best len (cons (car nodes) worse)))))))

;; state is ((path node ...) ...), nodes are sorted
(define (choose-next state)
   (lets
      ((nodes
         (keep (λ (x) (pair? (cdr x))) state))) ;; drop empty nodes
      (if (null? nodes)
         #empty
         (lets
            ((nodes best (select-biggest nodes))) ;; removes it from nodes
            (put 
               (choose-next
                  (map (λ (node) (substract node best)) nodes))
               (reverse (string->list (car best)))
               #true)))))

(define (intersector nodes)
   (lets
      ((nodes (map cdr nodes)) ;; ignore paths
       (all (fold (λ (all this) (put all this #true)) #empty (car nodes))))
      (let loop ((all all) (nodes (cdr nodes)))
         (if (null? nodes)
            all
            (loop
               (fold
                  (λ (new line)
                     (if (get all line #false)
                        (put new line #true)
                        new))
                  #empty (car nodes))
               (cdr nodes))))))

(define (reverse-intersect nodes all)
   (fold
      (λ (uniq node)
         (fold
            (λ (uniq line)
               (cond
                  ((get all line #false) uniq)
                  ((get uniq line #false) uniq)
                  (else (put uniq line #true))))
            uniq (cdr node)))
      #empty nodes))

(define (difference nodes rest)
   (let ((sub (fold (λ (sub ls) (fold (λ (sub l) (put sub l #true)) sub ls)) #empty rest)))
      (fold (λ (out l) (if (get sub l #false) out (put out l #true))) #empty nodes)))

(define (union-all nodes)
   (fold
      (λ (done node)
         (fold
            (λ (done line)
               (if (getf done line)
                  done
                  (put done line line)))
            done (cdr node)))
      #empty nodes))

(define stderr-writer
   (λ stuff
      (for-each (λ (thing) (display-to stderr thing)) stuff)
      (print-to stderr "")))

(define sinker (λ x x))

(define command-line-rules
   (cl-rules
      `((help "-h" "--help" comment "show this thing")
        (intersect "-i" "--intersect" comment "output all records present in all files")
        (union "-u" "--union" comment "output all records present in any of the files")
        (difference "-d" "--difference" comment "output all records of the first not present in any of the latter files")
        (reverse-intersect "-r" "--reverse-intersect" comment "output all records not present in all files")
        (cover "-c" "--cover" comment "output a subset of paths which together contain all the records")
        (sort "-s" "--sort" comment "output sorted results")
        (verbose "-v" "--verbose" comment "output runtime info to stderr")
        (blacklist "-b" "--blacklist" has-arg plural comment "ignore content in this file"))))

(define usage-text
   "lines [-h] [-s] [-i] [-u] [-b <path>] <path> ...")

(define (print-ff ff)
   (ff-fold
      (λ (_ bs v)
         (print (list->string (reverse bs))))
      ff ff))

(define (output-lines sort? ff)
   (if sort?
      (for-each 
         (λ (bs) (print (list->string bs)))
         (sort lex<? 
            (map reverse
               (ff-fold (λ (out bs v) (cons bs out)) null ff))))
      (print-ff ff))
   0)

(define (start-lines dict args)
   (cond
      ((getf dict 'help)
         (print usage-text)
         (print (format-rules command-line-rules))
         0)
      ((null? args)
         ;; exit with 0, because nothing is the union of no sets. this 
         ;; situation might happen in scripted use.
         0)
      (else
         (lets
            ((noter
               (if (getf dict 'verbose)
                  stderr-writer
                  sinker))
             (store
               (maybe-read-blacklist noter
                  (get dict 'blacklist null)))
             (data
               (read-files store args noter)))
            (if (and store data)
               (output-lines
                  (getf dict 'sort)
                  (cond
                     ((getf dict 'intersect)
                        (intersector data))
                     ((getf dict 'reverse-intersect)
                        (reverse-intersect data
                           (intersector data)))
                     ((getf dict 'difference)
                        (let ((data (map cdr data)))
                           (difference (car data) (cdr data))))
                     ((getf dict 'cover)
                        (choose-next data))
                     (else
                        ;; union is default behavior
                        (union-all data))))
               1)))))

(λ (args)
   (process-arguments (cdr args)
      command-line-rules
      usage-text start-lines))
