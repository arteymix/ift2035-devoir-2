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
(define node-term (lambda (n) (car (cdr n))))
(define node-definitions (lambda (n) (car (cddr n))))
(define node-right (lambda (n) (car (cdddr n))))

;;; construit un arbre avec n comme racine et tous les noeuds de root
;;; n est élément de root
(define node-splay (lambda (root n) (n)))

;;; recherche un noeud possédant un terme donné
;;; retourne #f si aucun noeud ne correspond au terme donné
(define node-search (lambda (root node term)
                      (if (null? node)
                        #f
                        (let ((t (node-term node)))
                          (cond
                            ((string-ci=? term t) (node-splay root node))
                            ((string-ci<? term t) (node-search root (node-left node) term))
                            ((string-ci>? term t) (node-search root (node-right node) term)))))))

;;; insère un nouveau noeud dans l'arbre
(define node-insert (lambda (root node)
                      (if (null? n)
                        (let ((term (node-term node)))
                          (cond
                            ((string-ci=? term t) (node-splay root node))
                            ((string-ci<? term t) (node-search root (node-left node) term))
                            ((string-ci>? term t) (node-search root (node-right node) term)))
                          ('())))))

;;; construit la définition d'un noeud
(define node-definition (lambda (node)
                          (fold-left string-append "" (node-definitions node))))

(assert (equal? (node-definition '(() "term" ("a" "b" "c") ())) "abc") "node-defintion concatenation")

;;; supprime un noeud de l'arbre
(define node-delete (lambda (n term) n))

;;;----------------------------------------------------------------------------

;;; Fonctions pour traiter les liste chainées de définitions

;;;----------------------------------------------------------------------------

;;; dict        :== (root . definitions)
;;; root        :== (left term definition right)
;;; definitions :== ((definition1 . count1) (definition2 . count2) ...)
(define traiter
  (lambda (expr dict)
    (if (null? dict)
      (traiter expr '(() ())) ; initialise le dictionnaire et les définitions
      (let ((root (car dict)) (definitions (cdr dict)) (definition (member #\= expr)))
        (if definition
          (let ((term (take (- (length expr) (length expr)) expr))) ; insertion, on récupère le terme
            (if (equal? (string-length definition) 0)
              (node-delete root term) ; suppression
              (node-insert root))) ; insertion
          (#t (let ((term expr)) ; les autres cas sont des recherches
                (cons (node-definition root (node-search root term)) dict) ; la recherche n'altère par le dictionnaire, alors on le retourne
                ))))))) ; recherche

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
