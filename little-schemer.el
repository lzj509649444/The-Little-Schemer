;;http://wiki.dourok.info/doku.php/%E5%B7%A5%E5%85%B7/emacs/elisp%E7%AC%94%E8%AE%B0
;; function atom and  test
(defun atom? (x)
  (not (listp x)))
(atom? '(1 2))
(atom? 1)
(atom? 'a)
(atom? (quote a))
(quote b)
(quote 1)



;; function null?
(defun null? (x)
  (cond
   ((and (listp x) (null x)))
   (t nil)))
(null? '(1 2))
(null? 1)

;; lat? function
(defun lat? (l)
  (cond
   ((null? l))
   ((atom? (car l)) (lat? (cdr l)))
   (t nil)))
(lat? '(1 2 3 (a b c)))

;; eq? function
(defun eq? (a b)
  (eq a b))
(eq? 1 2)
(eq? 1 '(1 2))

;; member? function
(defun member? (a lat)
  (cond
   ((null? lat) nil)
   ((or (eq? (car lat) a)
        (member? a (cdr lat))))))
(setq l '(coffee tea or milk))
(or (atom? '(1 2))
    (atom? '(2 3)))
(member? 'tea l)
(member? 'rice l)
(member? 1 '(b c))
(member? 'milk l)
(member? 'or l)
(member? 'meat '(mashed potatoes and meat gravy))


;; function remember
(defun remember (a lat)
  (cond
   ((null? lat) (quote ()))
   ((not (listp lat)) nil)
   ((eq? a (car lat)) (cdr lat))
   ((cons (car lat)
          (remember a (cdr lat))))))
(remember 'meat '(egg potatoes and meat gravy))
(remember 'egg '(egg potatoes and meat gravy))
(remember 'gravy '(egg potatoes and meat gravy))
(remember 'beaf '(egg potatoes and meat gravy))
(remember 'and (cdr '(and egg)))

;; firsts function
(defun firsts (l)
  (cond
   ((null? l) l)
   ((atom? l) l)
   ((cons (cond
           ((atom? (car l)) (car l))
           ((car (car l))))
          (firsts (cdr l))))))
(firsts '((1 2) a))
(firsts '((1 2) ()))
(firsts '(a (1 2) (a b) c))
(firsts '((a b) (c d) (e f)))
(firsts '())
(firsts '( ( (five plu ms) four)(eleven green oranges)((no) more)))

;; insertR function
(defun insertR (new old lat)
  (cond
   ((null? lat)
    (and (listp old) (null? old)
         (cons new lat)))
   ((null? old) '())
   ((eq? old (car lat))
    (cons old(cons new (cdr lat))))
   ((cons (car lat)
          (insertR new old (cdr lat))))))
(insertR 'jalapen 'and '(tacos tamales and salsa))
(insertR 'hello 'tacos '(tacos tamales and salsa))
(insertR 'hello 'salsa '(tacos tamales and salsa))
(insertR 'hello 'a '())
(insertR 'hello '() '())
(insertR 'hello '() '(world))
(insertR 'e 'd '(a b c d f g d h))

;; insertL function
(defun insertL (new old lat)
  (cond
   ((null? lat)
    (and (listp old) (null? old)
         (cons new lat)))
   ((null? old) '())
   ((eq? old (car lat))
    (cons new lat))
   ((cons (car lat)
          (insertL new old (cdr lat))))))
(insertL 'jalapen 'and '(tacos tamales and salsa))
(insertL 'hello 'tacos '(tacos tamales and salsa))
(insertL 'hello 'a '())
(insertL 'hello '() '())
(insertL 'hello '() '(world))
(insertL 'e 'd '(a b c d f g d h))
(insertL 'hello 'salsa '(tacos tamales and salsa))

(defun subst (new old lat)
  (cond
   ((null? lat) '())
   ((eq? old (car lat))
    (cons new (cdr lat)))
   ((cons (car lat)
          (subst new old (cdr lat))))))
(setq new 'stopping)
(setq old 'fudge)
(setq lat '(ice cream with fudge for dessert))
(subst new old lat)
(subst 'stopping 'fudge '(ice cream with fudge for dessert))
(subst 'stopping 'bb lat)

;; subst2 function
(defun subst2 (new o1 o2 lat)
  (cond
   ((null? lat) '())
   ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
    (cons new (cdr lat)))
   ((cons (car lat)
              (subst2 new o1 o2 (cdr lat))))))
(subst2 new 'ice 'with lat)
(subst2 new '() '() lat)

;; multiremember function
(defun multiremember (a lat)
  (cond
   ((null? lat) '())
   ((eq? a (car lat))
    (multiremember a (cdr lat)))
   ((cons (car lat)
          (multiremember a (cdr lat))))))
(multiremember 'cup '(coffee cup tea cup and hick cup))
(multiremember 'tea '(coffee cup tea cup and hick cup))

;; multiremember2 function(keep all the atom that are same)
(defun multiremember2 (a lat)
  (cond
   ((null? lat) '())
   ((eq? a (car lat))
    (cons (car lat)
          (multiremember2 a (cdr lat))))
   ((multiremember2 a (cdr lat)))))
(multiremember2 'cup '(coffee cup tea cup and hick cup))
(multiremember2 'cup '('() coffee cup tea '() cup and hick cup))

;; multiinsertR function
(defun multiinsertR (new old lat)
  (cond
   ((null? lat) '())
   ((eq? old (car lat))
    (cons (car lat)
          (cons new (multiinsertR new old (cdr lat)))))
   ((cons (car lat)
          (multiinsertR new old (cdr lat))))))
(multiinsertR 'glass 'cup '(coffee cup tea cup and hick cup))

;; multisubst function
(defun multisubst (new old lat)
  (cond
   ((null? lat) '())
   ((eq? old (car lat))
    (cons new (multisubst new old (cdr lat))))
   ((cons (car lat)
          (multisubst new old (cdr lat))))))
(multisubst 'glass 'cup '(coffee cup tea cup and hick cup))

;; chapter 4  Numbers Games
(atom? '14)
'14
'()
14
(eq? 14 '14)

;; add1 function
(defun add1 (n)
  (+ n 1))
(add1 2)
(add1 4)
(add1 -4)
(add1 1.232)

;; sub1 function
(defun sub1 (n)
  (- n 1))
(sub1 1)
(sub1 5)
(sub1 0)

;; zero? function
(defun zero? (n)
  (zerop n))
(zero? 1)
(zero? 0)
(zero? 1492)

;; + function
(zero? 3)
(defun o+ (a b)
  (cond
   ((zero? b) a)
   ((o+ (add1 a) (sub1 b)))))
(o+ 2 3)

;; zero? is like null?
;; add1 like cons
;; cons builds lists and add1 builds numbers

;; keys:
;; 1.how to build list
;; 2.how to build numbers
(defun o++ (a b)
  (cond
   ((zero? b) a)
   ((add1 (o++ a (sub1 b))))))
(o++ 2 3)
(o++ -1 1)

(defun o- (a b)
  (cond
   ((zero? b) a)
   ((sub1 (o- a (sub1 b))))))
(o- 2 3)
(o- 3 2)
(o- 0 0)
(o- 3 0)

;; addtup
(defun addtup (tup)
  (cond
   ((null? tup) 0)
   ((+ (car tup) (addtup (cdr tup))))))
(addtup '(1 2 3))
;; Here is what we filled in:
;; ( + ( car tup) ( addtup ( cdr tup))).
;; Notice the similarity between this line, and
;; the last line of the function rember:
;; ( cons ( car lat ) ( rember a ( cdr lat ) ) ) .


;; x function
(defun ox (a b)
  (cond
   ((zero? b) 0)
   ((+ a (ox a (sub1 b))))))
(ox 2 3)
(ox 3 2)

;; tup+ function
(defun tup+ (t1 t2)
  (cond
   ((and (null? t1) (null? t2))
    '())
   ((null? t1)
    (cons (car t2)
          (tup+ '() (cdr t2))))
   ((null? t2)
    (cons (car t1)
          (tup+ (cdr t1) '())))
   ((cons (+ (car t1) (car t2))
          (tup+ (cdr t1) (cdr t2))))))
(tup+ '() '())
(tup+ '(1 2) '(3 4))
(tup+ '(1 2) '(3 4 5))
(tup+ '(1 2 3) '(2))
(tup+ '() '(1 2))
(tup+ '(1) '())

(defun tup++ (t1 t2)
  (cond
   ((and (null? t1) (null? t2))
    '())
   ((null? t1) t2)
   ((null? t2) t1)
   ((cons (+ (car t1) (car t2))
          (tup++ (cdr t1) (cdr t2))))))
(tup++ '() '())
(tup++ '(1 2) '(3 4))
(tup++ '(1 2) '(3 4 5))
(tup++ '(1 2 3) '(2))
(tup++ '() '(1 2))
(tup++ '(1) '())
(tup++ '() '(1 2 3 4 5))

;; > function
(defun o> (a b)
  (cond
   ((and (zero? a) (zero? b))
    nil)
   ((zero? a) nil)
   ((zero? b) t)
   ((o> (sub1 a) (sub1 b)))))
(o> 0 0)
(o> 1 2)
(o> 2 1)
(o> 3 3)

(defun o< (a b)
  (cond
   ((and (zero? a) (zero? b))
    nil)
   ((zero? a) t)
   ((zero? b) nil)
   ((o< (sub1 a) (sub1 b)))))

(o< 0 0)
(o< 1 2)
(o< 2 1)
(o< 3 3)

(defun o= (a b)
  (cond
   ((o> a b) nil)
   ((o< a b) nil)
   (t)))
(o= 2 3)
(o= 2 2)

;; expt function
(defun oexpt (a b)
  (cond
   ((zero? b) 1)
   ((ox a (oexpt a (sub1 b))))))
(oexpt 1 0)
(oexpt 0 2)
(oexpt 2 3)
(oexpt 3 2)

;; length function
(defun length (lat)
  (cond
   ((null? lat) 0)
   ((add1 (length (cdr lat))))))
(length '(1 2 3))
(length '())

;; pick function
(defun pick (n lat)
  (cond
   ((null? lat) '())
   ((= n 1) (car lat))
   ((pick (sub1 n) (cdr lat)))))
(pick 4 '(lasagna spaghetti ravioli macaroni met))

;; rempick
(defun rempick (n lat)
  (cond
   ((null? lat) '())
   ((= n 1) (cdr lat))
   ((cons (car lat)
          (rempick (sub1 n) (cdr lat))))))
(rempick 3 '(hotdogs with hot mustartd))

;;number?
(defun number? (n)
  (numberp n))
(number? 1)
(number? 'b)
(number? '1)

;; no-nums
(defun no-nums (lat)
  (cond
   ((null? lat) '())
   ((number? (car lat))
    (no-nums (cdr lat)))
   ((cons (car lat)
          (no-nums (cdr lat))))))
(no-nums '(5 pears 6 prunes 9 dates))

;; all-nums
(defun all-nums (lat)
  (cond
   ((null? lat) '())
   ((number? (car lat))
    (cons (car lat) (all-nums (cdr lat))))
   ((all-nums (cdr lat)))))
(all-nums '(5 pears 6 prunes 9 dates))


;; eqan? which is true if its two arguments are the same atom
(defun eqan? (a b)
  (cond
   ((and (number? a) (number? b))
    (= a b))
   ((or (number? a) (number? b))
    nil)
   ((eq? a b))))
(eqan? 1 '1)
(eqan? 1 'b)
(eq? 1 'b)
(eqan? 'b 'b)

;; occur
(defun occur (a lat)
  (cond
   ((null? lat) 0)
   ((eq? a (car lat))
    (add1 (occur a (cdr lat))))
   ((cond
     ((atom? (car lat))
      (occur a (cdr lat)))
     ((+ (occur a (car lat))
         (occur a (cdr lat))))))))
(occur 3 '(3 2 3 (2 3 4)))
(occur 3 '((2 3 4)))
(occur 3 '(2 3 4))
(occur 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
(occur 'banana '((banana) (split (((banana ice))) (cream (banana)) (sherbet))
                 (banana)
                 (bread)
                 (banana brandy)))


;; one?
(defun one? (a)
  (= a 1))
(defun one?? (a)
  (cond
   ((zero? a) nil)
   ((zero? (sub1 a)) t)))
(one? 1)
(one?? 1)
(one? 2)
(one?? 2)

;; using one? in rempick
(defun one-rempick (n lat)
  (cond
   ((null? lat) '())
   ((one? n) (cdr lat))
   ((cons (car lat)
          (one-rempick (sub1 n) (cdr lat))))))
(one-rempick 3 '(lemon meringue salty pie))

;; chapter 5
;; Oh My Gawd *: it's full of stars

;; rember*
(defun rember* (a lat)
  (cond
   ((null? lat) '())
   ((eqan? a (car lat))
    (rember* a (cdr lat)))
   ((cons (cond
           ((atom? (car lat))
            (car lat))
           ((rember* a (car lat))))
          (rember* a (cdr lat))))))
(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

;; rember**
(defun rember** (a lat)
  (cond
   ((null? lat) '())
   ((atom? (car lat))
    (cond
     ((eqan? a (car lat))
      (rember** a (cdr lat)))
     ((cons (car lat)
            (rember** a (cdr lat))))))
   ((cons (rember** a (car lat))
          (rember** a (cdr lat))))))
(rember** 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))

;; insertR*
(defun insertR* (new old l)
  (cond
   ((null? l) '())
   ((eq? old (car l))
    (cons old (cons new (cdr l))))
   ((cons (cond
           ((atom? (car l))
            (car l))
           ((insertR* new old (car l))))
          (insertR* new old (cdr l))))))

(setq l '((how much (wood))
          could
          ((a (wood) chuck))
          (((chuck)))
          (if (a) ((wood chuck)))
          could chuck wood))
(insertR* 'roast 'chuck l)

;; subst*
(defun subst* (new old l)
  (cond
   ((null? l) '())
   ((eqan? old (car l))
    (cons new (subst* new old (cdr l))))
   ((cons (cond
           ((atom? (car l))
            (car l))
           ((subst* new old (car l))))
          (subst* new old (cdr l))))))
(subst* 'apple 'banana
        '((banana)
          (banana)
          (bread)
          (banana brandy)))
(subst* 'apple 'banana
        '(split
          (((banana ice)))
          (cream (banana))
          (sherbet)))
(subst* 'apple 'banana
        '((banana)
          (banana)
          (bread)
          (banana brandy)
          (split
           ((banana ice))
           (cream (banana))
           (sherbet))))

;; member*
(defun member* (a l)
  (cond
   ((null? l) nil)
   ((eqan? a (car l)) t)
   ((or (cond
         ((atom? (car l))
          (member* a (cdr l)))
         ((member* a (car l))))
        (member* a (cdr l))))))
(member* 'chips '((potato) (chips ((whith) fish) (chips))))
(member* 'chips '((potato) (chipss ((whith) fish) (chipss))))
(member* '1 '(2 3 1))
(member* '1 '(3 4 5))

;; leftmost
(defun leftmost (l)
  (cond
   ((null? l) nil)
   ((atom? (car l)) (car l))
   ((leftmost (car l)))))
(leftmost '())
(leftmost '(((1 2))))


;; eqlist?
(defun eqlist? (l1 l2)
  (cond
   ((and (null? l1) (null? l2)) t)
   ((or (null? l1) (null? l2)) nil)
   ((and (atom? (car l1)) (atom? (car l2)))
    (and (eqan? (car l1) (car l2))
         (eqlist? (cdr l1) (cdr l2))))
   ((or (atom? (car l1)) (atom? (car l2))) nil)
   ((and (eqlist? (car l1) (car l2))
         (eqlist? (cdr l1) (cdr l2))))))
(eqlist? '((1 2 (3)) 4) '((1 2 (3)) 4))

;; equal?
(defun equal? (s1 s2)
  (cond
   ((and (atom? s1) (atom? s2))
    (eqan? s1 s2))
   ((or (atom? s1) (atom? s2)) nil)
   ((eqlist? s1 s2))))

;; chapter6 Shadows

;; chapter7 Friends and Relations
(defun member?? (a lat)
  (cond
   ((null? lat) nil)
   ((or (equal? (car lat) a)
        (member?? a (cdr lat))))))
(defun set? (lat)
  (cond
   ((null? lat) t)
   ((member?? (car lat) (cdr lat)) nil)
   ((set? (cdr lat)))))
(set? '(apple peaches apple plum))
(set? '(apples peaches pears plums))
(set? '())
(set? '(apple 3 pear 4 9 apple 3 4))
(set? '((apple 3)
        (banana 4)
        (apple 3)))

;;makeset
(defun makeset (lat)
  (cond
   ((null? lat) '())
   ((member?? (car lat) (cdr lat))
    (makeset (cdr lat)))
   ((cons (car lat)
          (makeset (cdr lat))))))
(makeset '(apple peaches apple plum))
(makeset '(apple 3 pear 4 9 apple 3 4))
;;using multiremember
(defun makeset2 (lat)
  (cond
   ((null? lat) '())
   ((cons (car lat)
          (makeset2 (multiremember (car lat) (cdr lat)))))))
(makeset2 '(apple peaches apple plum))
(makeset2 '(apple 3 pear 4 9 apple 3 4))

;; subset?
(defun subset? (set1 set2)
  (cond
   ((null? set1) t)
   ((member?? (car set1) set2)
    (subset? (cdr set1) set2))
   (nil)))
(subset? '(4 pounds of horseradish)
         '(four pounds of horseradish 5 ounces horseradish))
(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and
             light duckling wings))

;;eqset?
(defun eqset? (s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))
(eqset? '(6 large chickens with wings)
        '(6 large chickens with wings))

;;intersect?
;; at least one atom in set1 is in set2
(defun intersect? (s1 s2)
  (cond
   ((null? s1) nil)
   ((or (member?? (car s1) s2)
         (intersect? (cdr s1) s2)))))
(intersect? '(stewed tomatoes and macaroin)
            '(macaroin and cheese))

;; intersect
(defun intersect (s1 s2)
  (cond
   ((null? s1) '())
   ((member?? (car s1) s2)
    (cons (car s1)
          (intersect (cdr s1) s2)))
   ((intersect (cdr s1) s2))))
(intersect '(stewed tomatoes and macaroin)
            '(macaroin and cheese))

;; union
(defun union (s1 s2)
  (cond
   ((null? s1) s2)
   ((null? s2) s1)
   ((cons (car s1)
          (union (cdr s1) (multiremember (car s1) s2))))))
(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))


;; intersectall
(defun intersectall (lat)
  (cond
   ((null? lat) '())
   ((intersect (car lat)
               (cond
                ((null? (cdr lat)) (car lat) )
                ((intersectall (cdr lat))))))))
(intersectall '(( 6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plu ms)
                (and 6 pru nes with some apples)))

;; a-pari?
(defun a-pair? (l)
  (cond
   ((atom? l) nil)
   ((or (null? l) (null? (cdr l))) nil)
   ((null? (cdr (cdr l))) t)
   (nil)))
(a-pair? '(1 2))
(a-pair? '(1))

;;fun?
(defun fun? (rel)
  (set? (firsts rel)))
(fun? '((d 4) (b 0) (e 5) (g 4) (b 9)))


;; reverl
(defun switch (pair)
  (cond
   ((a-pair? pair)
       (insertR (car pair) (car (cdr pair)) (cdr pair)))))
(switch '(1 2))

(defun reverl (rel)
  (cond
   ((null? rel) nil)
   ((a-pair? (car rel))
    (cons (switch (car rel))
          (reverl (cdr rel))))
   (nil)))
(reverl '((8 a) (pumpkin pie) (got sick)))

;;fullfun?
(defun seconds (fun)
  (firsts (reverl fun)))
(seconds '((8 a) (pumpkin pie) (got sick)))
(defun fullfun? (fun)
  (set? (seconds fun)))

;; reverse

;; chapter8 Lambda the Ultimate
(defun rember-f (test? a l)
  (cond
   ((null? l) '())
   ((funcall test? a (car l))
    (cdr l))
   ((cons (car l)
          (rember-f test? a (cdr l))))))
(rember-f (function =) 5 '(6 2 5 3))
(rember-f (function equal?) '(pop corn)
          '(lemonade (pop corn) and (cake)))

;;closures in emacs lisp
;;http://www.emacswiki.org/emacs/FakeClosures
(defun foo (n)
  (lexical-let ((n n))
    #'(lambda () n)))

(defun eq?-m (a)
  (lexical-let ((a a))
    #'(lambda (x)
        (equal? x a))))

(defun eq?-c (a)
  (lexical-let ((a a))
    (function
     (lambda (x)
       (equal? x a)))))

(setq eq?-salad (eq?-c 'salad))
(funcall (eq?-m 'aa) 'aa)
(funcall (eq?-c '(a (a b))) '(a (a b)))
(funcall (eq?-c '(a (a b) ((a)) a)) '(a (a b) ((a)) a))
(funcall eq?-salad 'salad)

;;rewrite rember-f
(defun rember-ff (test?)
  (lexical-let ((func test?))
    (function
     (lambda (a l)
       (cond
        ((null? l) '())
        ((funcall func a (car l))
         (cdr l))
        ((cons (car l)
               (funcall (rember-ff func) a (cdr l)))))))))
(setq rember-eq? (rember-ff (function eq?)))
(funcall rember-eq? 5 '(2 3 5 6))
(funcall rember-eq? 'tuna '(tuna salad is good))
(funcall rember-eq? 'tuna '(shrimp salad and tuna salad))
(funcall rember-eq? 'eq? '(equal? eq? eqan? eqlist? eqpair?))

;;
(defun insertL-f (test?)
  (lexical-let ((func test?))
    (function
     (lambda (new old lat)
       (cond
        ((null? lat)
         (and (listp old) (null? old)
              (cons new lat)))
        ((null? old) '())
        ((funcall func old (car lat))
         (cons new lat))
        ((cons (car lat)
               (funcall (insertL-f func) new old (cdr lat)))))))))
(setq L-insert (insertL-f (function eq?)))
(funcall L-insert 'jalapen 'and '(tacos tamales and salsa))
(funcall L-insert 'hello 'tacos '(tacos tamales and salsa))
(funcall L-insert 'hello 'a '())
(funcall L-insert 'hello '() '())
(funcall L-insert 'hello '() '(world))
(funcall L-insert 'e 'd '(a b c d f g d h))
(funcall L-insert 'hello 'salsa '(tacos salsa tamales and salsa))

;; insert-g ( insert either at the left or at right)
(defun seqL (new lat)
  (cons new lat))

(defun seqR (new lat)
  (cons (car lat) (cons new (cdr lat))))
(seqL 'a '(1 2))
(seqR 'a '(1 2))

;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; return a function  VS  pass function as argument
;; different from pass the function as argument?
;; dynamic generate function like ruby meta programming
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defun insert-g (test?)
  (lexical-let ((test? test?))
    (function
     (lambda (new old lat)
       (cond
        ((null? lat)
         (and (listp old) (null? old)
              (cons new lat)))
        ((null? old) '())
        ((equal? old (car lat))
         (funcall test? new lat))
        ((cons (car lat)
               (funcall (insert-g test?) new old (cdr lat)))))))))

;; generate two functions
(setq Linsert (insert-g (function seqL)))
(setq Rinsert (insert-g (function seqR)))

(funcall Linsert 'jalapen 'and '(tacos tamales and salsa))
(funcall Linsert 'hello 'tacos '(tacos tamales and salsa))
(funcall Linsert 'hello 'a '())
(funcall Linsert 'hello '() '())
(funcall Linsert 'hello '() '(world))
(funcall Linsert 'e 'd '(a b c d f g d h))
(funcall Linsert 'hello 'salsa '(tacos salsa tamales and salsa))

(funcall Rinsert 'jalapen 'and '(tacos tamales and salsa))
(funcall Rinsert 'hello 'tacos '(tacos tamales and salsa))
(funcall Rinsert 'hello 'a '())
(funcall Rinsert 'hello '() '())
(funcall Rinsert 'hello '() '(world))
(funcall Rinsert 'e 'd '(a b c d f g d h))
(funcall Rinsert 'hello 'salsa '(tacos salsa tamales and salsa))


;; generate another two functions
(setq LLinsert (insert-g
               (lambda (new lat)
                 (cons new lat))))
(setq RRinsert (insert-g
               (lambda (new lat)
                 (cons (car lat) (cons new (cdr lat))))))
(funcall LLinsert 'jalapen 'and '(tacos tamales and salsa))
(funcall LLinsert 'hello 'tacos '(tacos tamales and salsa))
(funcall LLinsert 'hello 'a '())
(funcall LLinsert 'hello '() '())
(funcall LLinsert 'hello '() '(world))
(funcall LLinsert 'e 'd '(a b c d f g d h))
(funcall LLinsert 'hello 'salsa '(tacos salsa tamales and salsa))

(funcall RRinsert 'jalapen 'and '(tacos tamales and salsa))
(funcall RRinsert 'hello 'tacos '(tacos tamales and salsa))
(funcall RRinsert 'hello 'a '())
(funcall RRinsert 'hello '() '())
(funcall RRinsert 'hello '() '(world))
(funcall RRinsert 'e 'd '(a b c d f g d h))
(funcall RRinsert 'hello 'salsa '(tacos salsa tamales and salsa))


;;multirember&co
(defun multirember&co (a lat col)
  (cond
   ((null? lat)
    (funcall col '() '()))
   ((equal? a (car lat))
    (multirember&co a (cdr lat)
                    (lexical-let ((col col))
                      (lambda (newlat seen)
                        (funcall col newlat (cons (car lat) seen))))))
   ((multirember&co a (cdr lat)
                    (lexical-let ((col col))
                      (lambda (newlat seen)
                        (funcall col (cons (car lat) newlat) seen)))))))

(cons 'a nil)
(defun a-friend (x y)
  (null? y))
(multirember&co 'tuna '(strawberries tuna and swordfish) (function a-friend))
(multirember&co 'tuna '(tuna) (function a-friend))
(multirember&co 'tuna '() (function a-friend))
(multirember&co 'tuna '(1 2 3) (function a-friend))

(defun new-friend (newlat seen)
  (a-friend newlat (cons 'tuna seen)))
(multirember&co 'tuna '(strawberries tuna and swordfish) (function new-friend))
(multirember&co 'tuna '(tuna) (function new-friend))
(multirember&co 'tuna '() (function new-friend))

(defun last-friend (x y)
  (length x))
(multirember&co 'tuna '(strawberries tuna and swordfish) (function last-friend))
(multirember&co 'tuna '() (function last-friend))
(multirember&co 'tuna '(tuna) (function last-friend))
(funcall (eq?-m 'asa) 'asa)

(defun multiinsertLR (new oldL oldR lat)
  (cond
   ((null? lat) '())
   ((eq? oldL (car lat))
    (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
   ((eq? oldR (car lat))
    (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
   ((cons (car lat)
          (multiinsertLR new oldL oldR (cdr lat))))))
(multiinsertLR 'x 'a 'b '(a o a o b o b b a b o))

;; querstion
;; 1. what's the difference between multiinsertLR and multiinsertLR&col
;; 2. multiinsertLR only collect one kind of sequence, but
;; multiinsertLR&col collect various of sequence with more information
;;multiinsertLR&co
(defun multiinsertLR&co (new oldL oldR lat col)
  (cond
   ((null? lat)
    (funcall col '() 0 0))
   ((eq? oldL (car lat))
    (multiinsertLR&co new oldL oldR (cdr lat)
                      (lexical-let ((col col) (lat lat))
                        (lambda (newlat L R)
                          (funcall col (cons new (cons oldL newlat))
                                   (+ 1 L) R)))))
   ((eq? oldR (car lat))
    (multiinsertLR&co new oldL oldR (cdr lat)
                      (lexical-let ((col col) (lat lat))
                        (lambda (newlat L R)
                          (funcall col (cons oldR (cons new newlat))
                                   L (+ 1 R))))))
   ((multiinsertLR&co new oldL oldR (cdr lat)
                      (lexical-let ((col col) (lat lat))
                        (lambda (newlat L T)
                          (funcall col (cons (car lat) newlat)
                                   L R)))))))
(defun col1 (lat L R)
  lat)
(defun col2 (lat L R)
  L)
(defun col3 (lat L R)
  R)
(multiinsertLR&co 'salty
                  'fish
                  'chips
                  '(chips and fish or fish and chips)
                  (function col1))
(multiinsertLR&co 'salty
                  'fish
                  'chips
                  '(chips and fish or fish and chips)
                  (function col2))
(multiinsertLR&co 'salty
                  'fish
                  'chips
                  '(chips and fish or fish and chips)
                  (function col3))

;;evens-only*
(defun even? (a)
  (and (atom? a) (evenp a)))
(even? 2)
(even? '(2))
(defun evens-only* (lat)
  (cond
   ((null? lat) '())
   ((even? (car lat))
    (cons (car lat)
          (evens-only* (cdr lat))))
   ((cond
     ((atom? (car lat))
      (evens-only* (cdr lat)))
     ((cons (evens-only* (car lat))
            (evens-only* (cdr lat))))))))
(evens-only*
 '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(defun evens-only*&co (lat col)
  (cond
   ((null? lat)
    ;; collector final arguments
    (funcall col '() 1 0))
   ((even? (car lat))
    (evens-only*&co (cdr lat)
                    (lexical-let ((col col) (lat lat))
                      ;; where comes the lambda arguments
                      (lambda (newlat p s)
                        (funcall col
                                 ;; this is collector(col) arguments
                                 (cons (car lat) newlat)
                                 (* (car lat) p)
                                 s)))))
   ((cond
     ((atom? (car lat))
      (evens-only*&co (cdr lat)
                      (lexical-let ((col col) (lat lat))
                        (lambda (newlat p s)
                          (funcall col newlat p
                                   (+ (car lat) s))))))
     ((evens-only*&co (car lat)
                      (lexical-let ((col col) (lat lat))
                        ;; al ap as are pass by col funcall
                        ;; how this works
                        (lambda (al ap as)
                          (evens-only*&co (cdr lat)
                                   (lexical-let ((al al) (ap ap)
                                                 (as as) (lat lat)
                                                 (col col))
                                     (lambda (dl dp ds)
                                       (funcall col (cons al dl)
                                                (* ap dp)
                                                (+ as ds)))))))))))))
(defun evens-friend (e p s)
  e)
(evens-only*&co
 '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
 (function evens-friend))

(defun evens-product-friend (e p s)
  p)
(defun evens-sum-friend (e p s)
  s)

(evens-only*&co
 '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
 (function evens-product-friend))
(evens-only*&co
 '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
 (function evens-sum-friend))

(defun the-last-friend (newl product sum)
  (cons sum
        (cons product newl)))
(evens-only*&co
 '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
 (function the-last-friend))

;;chapter9 Again, and Again and Again
(defun pick (n lat)
  (cond
   ((null? lat) '())
   ((not (number? n)) '())
   ((= 1 n)
    (car lat))
   ((pick (- n 1) (cdr lat)))))
(pick 3 '(1 2 this))

;; dead loop
(defun keep-looking (a n lat)
  (cond
   ((equal? n (pick n lat))
    (equan? n a))
   ((number? n)
    (keep-looking a (pick n lat) lat))
   ((equal? n a))))
(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))
(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))
(looking 'caviar '(7 1 2 caviar 5 6 3))

;; chapter10 What is the value of All of This?
(defun build (s1 s2)
  (cons s1 (cons s2 '())))

(setq new-entry (function build))
(funcall new-entry '() '(1 2 (a)))

(build '(appetizer entree flour age)
       '(pate dog duck))
(build '(rice apple tomato)
       '((noodle jiaozi) (soup orange)))

(defun first (l)
  (car l))
(first '(1 2 3))

(defun second (l)
  (car (cdr l)))
(second '(1 2 3))

(defun third (l)
  (car (cdr (cdr l))))
(third '(1 2 3 4))
(second '((appetizer entree beverge)
          (food tastes good)))

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

;; think recursive
(defun lookup-in-entry-help (name names values entry-f)
  (cond
   ((null? names) (funcall entry-f name))
   ((eq? name (car names))
    (car values))
   ((lookup-in-entry-help name
                          (cdr names)
                          (cdr values)
                          entry-f))))
(lookup-in-entry 'entree
                 '((appetizer entree beverge)
                   (pate boeuf vin))
                 (lambda (n) '()))
(lookup-in-entry 'entree
                 '()
                 (lambda (n) '()))

(defun lookup-in-table (name table table-f)
  (cond
   ((null? table) (funcall table-f name))
   ((lookup-in-entry name
                     (car table)
                     (lambda (name)
                         (lookup-in-table name
                                        (cdr table)
                                        table-f))))))
(lookup-in-table
 'beverage
 '(((entree dessert) (spaghetti spumoni))
   ((appetizer entree beverage) (food tastes good)))
 (lambda (n) '()))

;; how many types do you think there are?
;; we found six
   '((*const)
    (*quote)
    (*identifier)
    (*lambda)
    (*cond)
    (*application))

(defun expression-to-action (e)
  (cond
   ((atom? e) (atom-to-action e))
   ((list-to-action e))))

; Expressions to actions
;
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else
        (list-to-action e)))))

; Atom to action
;
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

; List to action
;
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))
 salsa))
