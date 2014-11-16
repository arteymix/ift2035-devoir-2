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
(define assert (lambda (expr)
                 (if expr
                   (begin
                     (display "\x1B[32m")
                     (display "pass\n")
                     (display "\x1b[0m"))
                   (begin
                     (display "\x1b[31m")
                     (display "fail\n")
                     (display "\x1b[0m")))))

;;; prend n élément de la liste l
(define take (lambda (n l)
               (if (= n 0)
                 '()
                 (cons (car l) (take (- n 1) (cdr l))))))

(assert (equal? (take 0 '(1 2 3)) '())) ; prendre aucun éléments
(assert (equal? (take 2 '(1 2 3)) '(1 2))) ; prendre quelque éléments
(assert (equal? (take 3 '(1 2 3)) '(1 2 3))) ; prendre tous les éléments

;;; split la liste l en sous-listes sur element
(define split (lambda (l element)
  (if (not (member element l))
    (list l)
    (let ((right (cdr (member element l))))
      (let ((left (take (- (length l) (length right) 1) l)))
                 (cons left (split right element)))))))

(assert (equal? (split '(1 2 3) 2) '((1) (3))))

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
(define node-search (lambda (root n term)
  (if (null? n) 
	#f
    (let ((t (node-term n)))
      (cond
        ((string-ci=? term t) (node-splay root n))
        ((string-ci<? term t) (node-search root (node-left n) term))
        ((string-ci>? term t) (node-search root (node-right n) term)))))))

;;; insère dans l'arbre
(define node-insert (lambda (p n)
	(if (null? n)
		(let ((t (node-term n)))
			(cond
				((string-ci=? term t) (node-splay root n))
				((string-ci<? term t) (node-search root (node-left n) term))
			((string-ci>? term t) (node-search root (node-right n) term)))
		('())
	))))

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
          (if (eq? definition #f)
        ;;;si pas '=', recherche de la définition
        ((result expr dict))
        ;;si lexpression contient un '=', modification ou ajout d'un noeud dans l'arbre
        ((result definition dict))
      )))))
;;;(cons (append (string->list "*** le programme est ")
;;;              '(#\I #\N #\C #\O #\M #\P #\L #\E #\T #\! #\newline)
;;;              (string->list "*** la requete lue est: ")
;;;              expr
;;;              (string->list "\n*** nombre de caractères: ")
;;;              (string->list (number->string (length expr)))
;;;              '(#\newline))
;;;      dict))))

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
