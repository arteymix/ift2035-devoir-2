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

; active le mode de débug avec les assertions
(define debug #f)

; assertion
(define assert (lambda (expr . message)
                 (if debug
                   (let ((color (if expr "\x1b[32m" "\x1b[31m")) (reset "\x1b[0m"))
                     (display color)
                     (display (if expr "pass" "fail"))
                     (display (if (not (null? message)) (string-append " " (car message)) ""))
                     (display "\n")
                     (display reset)
                     ))))

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

(assert (equal? '((1) (3)) (split '(1 2 3) 2) ) "regular split")
(assert (equal? '(()) (split '() 2)) "splitting an empty list")

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

(assert (equal? (fold-right + 1 '(1 2 3)) 7) "fold-right" "fold-right an addittion")
(assert (equal? (fold-right string-append "" '("a" "b" "c")) "cba") "fold-right reverse a string")
(assert (equal? (fold-right string-append "" '()) "") "fold-right an empty list")

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

;rotation zag
(define node-zag (lambda (root)
                   (let ((root-term (node-term (node-right root)))
                         (root-definitions (node-definitions (node-right root)))
                         (splayed-node (node-right root)))
                     (list
                       (list ; la racine est à gauche
                         (node-left root)
                         (node-term root)
                         (node-definitions root)
                         (node-left splayed-node))
                       root-term
                       root-definitions
                       (node-right splayed-node))
                     )))

(assert (equal?
          '((root-left root-term root-definitions node-left) node-term node-definitions node-right)
          (node-zag '(root-left root-term root-definitions (node-left node-term node-definitions node-right)))) "node-zag")

;;; rotation zig
(define node-zig (lambda (root)
                   (let ((splayed-node (node-left root)))
                     (list
                       (node-left splayed-node)
                       (node-term splayed-node)
                       (node-definitions splayed-node)
                       (list ; racine à droite
                         (node-right splayed-node)
                         (node-term root)
                         (node-definitions root)
                         (node-right root)))
                   )))

(assert (equal?
          '(node-left node-term node-definitions (node-right root-term root-definitions root-right))
          (node-zig '((node-left node-term node-definitions node-right) root-term root-definitions root-right))) "node-zig")

;;; rotation zag-zag
(define node-zag-zag (lambda (root)
                       (let ((root-term (node-term (node-right (node-right root))))
                             (root-definitions (node-definitions (node-right (node-right root))))
                             (splayed-node (node-right (node-right root))))
                         (list
                           (list ; noeud de gauche
                             (list
                               (node-left root)
                               (node-term root)
                               (node-definitions root)
                               (node-left (node-right root)))
                             (node-term (node-right root))
                             (node-definitions (node-right root))
                             (node-left splayed-node))
                           root-term
                           root-definitions
                           (node-right splayed-node)
                           ))))

(assert (equal?
          '(((root-left root-term root-definitions node-left) node-term node-definitions child-left) child-term child-definitions child-right)
          (node-zag-zag '(root-left root-term root-definitions (node-left node-term node-definitions (child-left child-term child-definitions child-right))))) "node-zag-zag")



;;; rotation zig-zig
(define node-zig-zig (lambda (root)
                       (let ((root-term (node-term (node-left (node-left root))))
                             (root-definitions (node-definitions (node-left(node-left root))))
                             (splayed-node (node-left (node-left root))))
                         (list
                           (node-left splayed-node)
                           root-term
                           root-definitions
                           (list ; noeud de droite
                             (node-right splayed-node)
                             (node-term (node-left root))
                             (node-definitions (node-left root))
                             (list
                               (node-right (node-left root))
                               (node-term root)
                               (node-definitions root)
                               (node-right root))))
                         )))

(assert (equal?
          '(child-left child-term child-definitions (child-right node-term node-definitions (node-right root-term root-definitions root-right)))
          (node-zig-zig '(((child-left child-term child-definitions child-right) node-term node-definitions node-right) root-term root-definitions root-right))) "node-zig-zig")

(define node-zag-zig (lambda (root)
                       (let ((root-term (node-term (node-left (node-right root))))
                             (root-definitions (node-definitions (node-left (node-right root))))
                             (splayed-node (node-left (node-right root))))
                         (list
                           (list ; noeud de gauche
                             (node-left root)
                             (node-term root)
                             (node-definitions root)
                             (node-left splayed-node))
                           root-term
                           root-definitions
                           (list ; noeud de droite
                             (node-right splayed-node)
                             (node-term (node-right root))
                             (node-definitions (node-right root))
                             (node-right (node-right root))))
                         )))

(assert (equal?
          '((root-left root-term root-definitions child-left) child-term child-definitions (child-right node-term node-definitions node-right))
          (node-zag-zig '(root-left root-term root-definitions ((child-left child-term child-definitions child-right) node-term node-definitions node-right)))) "zag-zig")

;;; rotation zig-zag
(define node-zig-zag (lambda (root)
                       (let ((root-term (node-term (node-right (node-left root))))
                             (root-definitions (node-definitions (node-right (node-left root))))
                             (splayed-node (node-right (node-left root))))
                         (list
                           (list ; noeud de gauche
                             (node-left (node-left root))
                             (node-term (node-left root))
                             (node-definitions (node-left root))
                             (node-left splayed-node))
                           root-term
                           root-definitions
                           (list ; noeud de droite
                             (node-right splayed-node)
                             (node-term root)
                             (node-definitions root)
                             (node-right root)))
                         )))

(assert (equal?
          '((node-left node-term node-definitions child-left) child-term child-definitions (child-right root-term root-definitions root-right))
          (node-zig-zag '((node-left node-term node-definitions (child-left child-term child-definitions child-right)) root-term root-definitions root-right))) "zag-zig")

;;; applique et sélectionne une rotation appropriée en reconstruisant l'arbre.
(define node-splay (lambda (root node)
                     (let ((term (node-term node)))
                       (cond

                         ;left-child
                         ((and
                            (not (null? (node-left root)))
                            (equal? term (node-term (node-left root))))
                          (node-zig root))

                         ;left-left child
                         ((and
                            (not (null? (node-left root)))
                            (not (null? (node-left (node-left root))))
                            (equal? term (node-term (node-left (node-left root)))))
                          (node-zig-zig root))

                         ;left-right child
                         ((and
                            (not (null? (node-left root)))
                            (not (null? (node-right (node-left root))))
                            (equal? term (node-term (node-right (node-left root)))))
                          (node-zig-zag root))

                         ;right-child
                         ((and
                            (not (null? (node-right root)))
                            (equal? term (node-term (node-right root))))
                          (node-zag root))

                         ;right-right child
                         ((and
                            (not (null? (node-right root)))
                            (not (null? (node-right (node-right root))))
                            (equal? term (node-term (node-right (node-right root)))))
                          (node-zag-zag root))

                         ;right-left child
                         ((and
                            (not (null? (node-right root)))
                            (not (null? (node-left (node-right root))))
                            (equal? term (node-term (node-left (node-right root)))))
                          (node-zag-zig root))

                         ; splay à gauche de root
                         ((string-ci<? (node-term node) (node-term root))
                          (list (node-splay (node-left root) node) (node-term root) (node-definitions root) (node-right root)))

                         ; splay à droite de root
                         ((string-ci>? (node-term node) (node-term root))
                          (list (node-left root) (node-term root) (node-definitions root) (node-splay (node-right root) node)))

                         ; déjà splayed
                         ((string-ci=? (node-term node) (node-term root))
                          root)
                         ))))

;;; applique node-splay tant que l'arbre n'est pas splayé.
(define splaytree (lambda (root node)
                    (if (null? root) root
                      (if (equal? (node-term root) (node-term node))
                        root
                        (splaytree (node-splay root node) node)))))

(assert (equal? '() (splaytree '() '())) "splaytree an empty node")

;;; recherche un noeud possédant un terme donné
;;; retourne le noeud correspondant au terme, sinon #f
(define node-search
  (lambda (node term)
    (if (null? node)
      #f ; non trouvé
      (let ((t (node-term node)))
        (cond
          ((string-ci=? term t) node)
          ((string-ci<? term t) (node-search (node-left node) term))
          ((string-ci>? term t) (node-search (node-right node) term)))
        ))))

(assert (equal? #f (node-search '() "term")) "search in an empty tree")

(assert (equal? '(node-left "a" node-definitions node-right)
                (node-search '((node-left "a" node-definitions node-right) "b" root-definitions root-right) "a")) "search left")

(assert (equal? '(node-left "b" node-definitions node-right)
                (node-search '(root-left "a" root-definitions (node-left "b" node-definitions node-right)) "b")) "search right")

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
          '(root-left "a" root-definitions (node-left "b" node-definitions node-right))
          (node-insert '(root-left "a" root-definitions ()) '(node-left "b" node-definitions node-right))) "insertion à droite de la racine")

(assert (equal?
          '((node-left "a" node-definitions node-right) "b" root-definitions root-right)
          (node-insert '(() "b" root-definitions root-right) '(node-left "a" node-definitions node-right))) "insertion à gauche de la racine")

(assert (equal?
          '(root-left "a" node-definitions root-right)
          (node-insert '(root-left "a" root-definitions root-right) '(node-left "a" node-definitions node-right))) "insertion avec substitution")

;;; supprime un noeud de l'arbre
;;;
;;; retourne une paire contenant l'arbre sans le noeud et le parent du noeud
;;; supprimé
(define node-delete (lambda (parent term)
                      (if (null? parent) #f ; noeud non trouvé
                        (let ((parent-term (node-term parent)))
                          (cond

                            ; noeud à gauche
                            ((and (not (null? (node-left parent)))
                                  (string-ci=? term (node-term (node-left parent))))
                             (cons
                               (list
                                 (if (null? (node-left (node-left parent)))
                                   (node-right (node-left parent))
                                   (node-insert (node-left (node-left parent)) (node-right (node-left parent))))
                                 (node-term parent)
                                 (node-definitions parent)
                                 (node-right parent))
                               parent ; splay sur le parent
                               )) ; reconstruit l'arbre sans le noeud

                            ; noeud à droite
                            ((and (not (null? (node-right parent)))
                                  (string-ci=? term (node-term (node-right parent))))
                             (cons
                               (list
                                 (node-left parent)
                                 (node-term parent)
                                 (node-definitions parent)
                                 (if (null? (node-left (node-right parent)))
                                   (node-right (node-right parent))
                                   (node-insert (node-left (node-right parent)) (node-right (node-right parent))))
                                 )
                               parent ; splay sur le parent
                               )) ; reconstruit l'arbre sans le noeud

                            ; noeud est la racine courante
                            ((string-ci=? term parent-term)
                             (cons
                               (cond
                                 ((null? (node-left parent)) (node-right parent))
                                 ((null? (node-right parent)) (node-left parent))
                                 (#t (node-insert (node-left parent) (node-right parent))))
                               parent ; splay sur le parent
                             ))

                            ; reconstruire à gauche
                            ((string-ci<? term parent-term)
                             (let ((pair (node-delete (node-left parent) term)))
                               (if pair
                                 (cons
                                   (list
                                     (car pair)
                                     (node-term parent)
                                     (node-definitions parent)
                                     (node-right parent))
                                   (cdr pair))
                                 #f
                                 )))

                            ; reconstruire à droite
                            ((string-ci>? term parent-term)
                             (let ((pair (node-delete (node-right parent) term)))
                               (if pair
                                 (cons
                                   (list
                                     (node-left parent)
                                     (node-term parent)
                                     (node-definitions parent)
                                     (car pair))
                                   (cdr pair))
                                 #f
                                 )))

                            )))))

(assert (equal?
          #f
          (node-delete '() "test")) "delete sur un noeud vide")

(assert (equal?
          (cons '(() "a" left-definitions (right-left "c" right-definitions right-right)) '((() "a" left-definitions ()) "b" node-definitions (right-left "c" right-definitions right-right)))
          (node-delete '((() "a" left-definitions ()) "b" node-definitions (right-left "c" right-definitions right-right)) "b")) "delete du parent")

;(assert (equal?
;          (cons '(() "a" left-definitions (right-left "c" right-definitions right-right)) '((() "a" left-definitions ()) "b" node-definitions (right-left "c" right-definitions right-right)))
;          (node-delete '((() "a" left-definitions ()) "b" node-definitions (right-left "c" right-definitions right-right)) "a")) "delete à gauche")
;
;(assert (equal?
;          (cons '(() "a" left-definitions (right-left "c" right-definitions right-right)) '((() "a" left-definitions ()) "b" node-definitions (right-left "c" right-definitions right-right)))
;          (node-delete '((() "a" left-definitions ()) "b" node-definitions (right-left "c" right-definitions right-right)) "c")) "delete à droite")

;;; construit une liste de définitions à partir d'une liste de termes
;;;
;;; l'utilisateur soumet une liste de terme pour définir un terme donné, il faut
;;; donc récupérer la liste de définitions qui y correspond.
(define node-build-definitions (lambda (root terms)
                                 (let ((node-search (lambda (term) (node-search root term)))) ; redéfinit la recherche pour capturer la racine dans une fermeture
                                   (let ((nodes (map node-search terms)))
                                     (if (member #f nodes)
                                       (if (equal? 1 (length nodes))
                                         (list (car terms)) ; pour un seul terme, on retourne le terme
                                         #f)
                                       (apply append (map node-definitions nodes)) ; sinon on retourne toutes les définitions
                                       )))))

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
        (let ((term (list->string (take (- (length expr) (length definition)) expr))) ; le terme est la partie qui précède le caractère #\=
              (definition (cdr definition)) ; on pop le caractère #\=
              (definitions (map list->string (split (cdr definition) #\+)))) ; split sur les symboles #\+
          (if (null? definition)
            (let ((pair (node-delete root term))) ; node-delete retourne une paire contenant l'arbre et le parent du noeud supprimé
              (if pair
                (cons '() (if (null? (cdr pair)) (splaytree (car pair) (cdr pair)) (car pair))); splay du parent du noeud supprimé
                (cons (string->list "terme inconnu\n") root) ; non trouvé
                ))             (let ((new-node-definitions (node-build-definitions root definitions)))
              (if new-node-definitions
                (let ((new-node (list '() term new-node-definitions '())))
                  (cons '() (splaytree (node-insert root new-node) new-node)))
                (cons (string->list "terme inconnu\n") root) ; on insère une définition composée de termes inconnus
                )))) ; insertion
        (let ((node (node-search root (list->string expr)))) ; noeud correspondant à la recherche
          (if node
            (cons (string->list (apply string-append (append (node-definitions node) '("\n")))) (splaytree root node)) ; on imprime la concaténation des definitions
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
