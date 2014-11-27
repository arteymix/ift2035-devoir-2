;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la première section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; être définie, et vous pouvez ajouter des définitions de fonction
;;; afin de bien décomposer le traitement à faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter reçoit en paramètre une liste de caractères
;;; contenant la requête lue et le dictionnaire.  La fonction retourne
;;; une paire contenant la liste de caractères qui sera imprimée comme
;;; résultat de l'expression entrée et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "go" qui se charge de cela.

;;; fonctions utiles

; assertion
(define assert (lambda (expr . message)
                 (let ((color (if expr "\x1b[32m" "\x1b[31m")) (reset "\x1b[0m"))
                     (display color)
                     (display (if expr "pass" "fail"))
                     (display (if (not (null? message)) (string-append " " (car message)) ""))
                     (display "\n")
                     (display reset))))

;;; prend n élément de la liste l
(define take (lambda (n l)
               (if (= n 0)
                 '()
                 (cons (car l) (take (- n 1) (cdr l))))))

(assert (equal? (take 0 '(1 2 3)) '()) "take no element") ; prendre aucun éléments
(assert (equal? (take 2 '(1 2 3)) '(1 2)) "take few elements") ; prendre quelque éléments
(assert (equal? (take 3 '(1 2 3)) '(1 2 3)) "take all elements") ; prendre tous les éléments

;;; split la liste l en sous-listes sur element
(define split (lambda (l element)
                (if (not (member element l))
                  (list l)
                  (let ((right (cdr (member element l))))
                    (let ((left (take (- (length l) (length right) 1) l)))
                      (cons left (split right element)))))))

(assert (equal? (split '(1 2 3) 2) '((1) (3))) "regular split")
(assert (equal? (split '() 2) '()) "splitting an empty list")

;;; applique la sortie de f sur l'entrée de f avec (car lst) left est utilisé
;;; comme valeur initiales
(define fold-left (lambda (f left lst)
                    (if (null? lst)
                      left ; termine avec ce qui a été foldé
                      (fold-left f (f left (car lst)) (cdr lst)))))

(assert (equal? (fold-left + 1 '(1 2 3)) 7) "fold-left an addition")
(assert (equal? (fold-left string-append "" '("a" "b" "c")) "abc") "fold-left a string")
(assert (equal? (fold-left string-append "" '()) "") "fold-left an empty list")

;;; fold-right est simple a définir avec un fold-left et un reverse
(define fold-right (lambda (f right lst)
                     (fold-left f right (reverse lst))))

(assert (equal? (fold-right + 1 '(1 2 3)) 7))
(assert (equal? (fold-right string-append "" '("a" "b" "c")) "cba"))
(assert (equal? (fold-right string-append "" '()) ""))

;;;----------------------------------------------------------------------------

;;; fonctions pour traiter les arbres

;;; accesseurs pour les noeuds
(define node-left (lambda (n) (car n)))
(define node-term (lambda (n) (cadr n)))
(define node-definitions (lambda (n) (caddr n)))
(define node-right (lambda (n) (cadddr n)))

(assert (equal? '() (node-left '(() "term" ("def1" "def2") ()))) "get the left child of a node")
(assert (equal? "term" (node-term '(() "term" ("def1" "def2") ()))) "get the term of a node")
(assert (equal? '("def1" "def2") (node-definitions '(() "term" ("def1" "def2") ()))) "get the definitions of a node")
(assert (equal? '() (node-right '(() "term" ("def1" "def2") ()))) "get the right child of a node")

;;; rotation zig avec node et son parent
(define node-zig (lambda (parent node)
                   (list
                     (node-left node)
                     (node-term node)
                     (node-definitions node)
                     (list
                       (node-right node)
                       (node-term parent)
                       (node-definitions parent)
                       (node-right parent)))))

;;; rotation zig-zig avec
(define node-zig-zig (lambda (g parent node)
  (list
    (node-left node)
    (node-term node)
    (node-definitions node)
    (list
      (node-right node)
      (node-term parent)
      (node-definitions parent)
      (list
        (node-right parent)
        (node-term g)
        (node-definitions g)
        (node-right g))))))

;;; rotation zig-zag de gauche à droite
(define node-zig-zag (lambda (g parent node)
  (list
    (list
      (node-left parent)
      (node-term parent)
      (node-definitions parent)
      (node-left node))
    (node-term node)
    (node-definitions node)
    (list
      (node-right node)
      (node-term g)
      (node-definitions g)
      (node-right g)))))

(define node-zag-zig (lambda (g parent node)
                       (list)))

(define node-zag-zag)

;(assert (equal?
;          '(
;            (parent-left parent-term parent-definitions node-left)
;            node-term
;            node-definition
;            (node-right g-term g-definitions g-right))
;          (node-zig-zag
;                          '(
;                           parent-left
;                           parent-term
;                           parent-definitions
;                           (node-left node-term node-definitions node-right))
;                          'g-term
;                          'g-definitions
;                          'g-right)))

(define node-splay (lambda (root node)
  (cond
    ; zig
    ((and (equal? g parent) (equal? (node-left parent) node))
     (node-zig parent node))
    ((and (equal? g parent) (equal? (node-right parent) node))
     (node-zig parent node))
    ; zig-zig
    ((and (equal? (node-left g) parent) (equal? (node-left parent) node))
     (node-zig-zig g parent node))
    ((and (equal? (node-right g) parent) (equal? (node-right parent) node))
     (node-zig-zig g parent node))
    ; zig-zag
    ((and (equal? (node-left g) parent) (equal? (node-right parent) node))
     (node-zig-zig g parent node))
    ((and (equal? (node-right g) parent) (equal? (node-left parent) node))
     (node-zig-zig g parent node)))))

;;; splay 3 noeuds
;;; applique une des rotations d'arbre, ramenant node à g
(define splaytree (lambda (root node)
                     (if (equal? (node-term root) (node-term node))
                       root
                       (splaytree (node-splay root node) node))))

;;; recherche un noeud possédant un terme donné
(define node-search
  (lambda (node term)
    (if(null? node)
      #f ; non trouvé
      (let ((t (node-term node)))
        (cond
          ((string-ci=? term t) node)
          ((string-ci<? term t) (node-search root (node-left node) term))
          ((string-ci>? term t) (node-search root (node-right node) term)))
        ))))

;;; insère dans l'arbre
;;; retourne l'arbre inséré et splayé
(define node-insert (lambda (parent node)
                      (if (null? parent) node ; insertion
                        (let ((parent-term (node-term parent)) (term (node-term node)))
                          (cond
                            ((string-ci=? term parent-term) ; substitition des définitions
                             (list
                               (node-left parent)
                               (node-term parent)
                               (node-definitions node)
                               (node-right parent)))
                            ((string-ci<? term parent-term) ; insertion à gauche
                             (list
                               (node-insert (node-left parent) node)
                               (node-term parent)
                               (node-definitions parent)
                               (node-right parent)))
                            ((string-ci>? term parent-term) ; insertion à droite
                             (list
                               (node-left parent)
                               (node-term parent)
                               (node-definitions parent)
                               (node-insert (node-right parent) node))))))))

(assert (equal?
          '(() "a" () (() "b" () ()))
          (node-insert '(() "a" () ()) '(() "b" () ()))) "insertion à droite de la racine")
(assert (equal?
          '(() "b" () (() "a" () ()))
          (node-insert '(() "b" () ()) '(() "a" () ()))) "insertion à droite de la racine")

;;; supprime un noeud de l'arbre
;;;
;;; retourne une liste contenant l'arbre sans le noeud avec le parent du noeud
;;; supprimé.
(define node-delete (lambda (parent term)
                      (if (null? parent) parent
                        (let ((t (node-term node)))
                          (cond
                            ((string-ci=? term t) (node-splay  parent node))
                            ((string-ci<? term t) (node-delete (node-left parent) node) term)
                            ((string-ci>? term t) (node-delete (node-right parent) node) term))
                          ))))

;;;
;;; construit récursivement la définition d'un terme
;;;
;;; ne doit pas être récursif
(define node-build-definitions (lambda (root node term)
                          (if null? node)
                            term ;; le terme n'est pas trouvé, alors c'est au moins la définition
                            (let (
                                  (t (node-term node))
                                  (node-build-definitions (lambda (term) (node-build-definitions root node term))))
                              (cond
                                ((string-ci=? term t) (map node-build-definitions (node-definitions node))
                                ((string-ci<? term t) (node-build-definitions root (node-left node) term))
                                ((string-ci>? term t) (node-build-definitions root (node-right node) term)))))))

;;;----------------------------------------------------------------------------

;;; Fonctions pour traiter les liste chainées de définitions

;;;----------------------------------------------------------------------------

;;; dict        :== (root . definitions)
;;; root        :== (left term definition right)
;;; definitions :== ((definition1 . count1) (definition2 . count2) ...)
(define traiter
  (lambda (expr root)
    (let ((definition (member #\= expr)))
      (if definition ; cas d'insertion et de suppression
        (let (
              (term (list->string (take (- (length expr) (length expr)) expr))) ; le terme est la partie qui précède le caractère #\=
              (definition (list->string (cdr definition))))                                    ; on extrait le caractère #\=
          (if (null? definition)
            (apply splaytree (node-delete root term)) ; suppression
            (let ((node (list '() term (node-build-definitions term))))
              (splaytree (node-insert root node) node)))) ; insertion
        (let ((node (node-search root expr))) ; noeud correspondant à la recherche
          (if node
            (cons (fold-left string-append "" (node-definitions node)) (splaytree root node)) ; on imprime la concaténation des definitions
            (cons (string->list "terme inconnu\n") root) ; terme inconnu si node-search retourne #f
            )))))) ; recherche

;;;----------------------------------------------------------------------------

;;; Ne pas modifier cette section.

(define go
  (lambda (dict)
    (print "? ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (let ((r (traiter-ligne ligne dict)))
            (for-each write-char (car r))
            (go (cdr r)))))))

(define traiter-ligne
  (lambda (ligne dict)
    (traiter (string->list ligne) dict)))

(go '()) ;; dictionnaire initial est vide

;;;----------------------------------------------------------------------------
