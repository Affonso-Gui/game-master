;; global variables
(defvar *turn* 0)
(defvar *cards* nil)
(defvar *players* nil)
(defvar *active-player* nil)
(defconstant +rarrow+ #\>)
(defconstant +larrow+ #\<)
(defconstant +underscore+ #\_)
(defvar *stream* (make-string-output-stream))
(defvar *nums* '((a . 1) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6) (seven . 7) (eight . 8) (nine . 9) (ten . 10)))

;; set/get functions
(defun add-card (name card) (push (cons name card) *cards*))
(defun get-card (name) (let ((card (cdr (assoc name *cards* :test #'equal))))
			 (if card card (error "No such card: ~a" name))))
(defun get-player (name) (cdr (assoc (string-upcase name) *players* :test #'equal)))
(defun get-creature (player name)
  (let ((c-lst (remove-if-not #'(lambda (c) (equal (slot-value c 'type) (get-card name))) (slot-value player 'field))))
    ;;select creatures that can attack first
    (if (cdr c-lst)
	(or (find-if #'(lambda (c) (slot-value c 'can-attack)) c-lst) (car c-lst))
	(car c-lst))))
(defun get-creature-name (creature) (car (find (slot-value creature 'type) *cards* :key #'cdr)))

;; class definitions
(defclass player ()
  ((life :initform 20)
   (field :initform nil)
   (mana :initform 0)
   (val :initform 0)))

(defclass spell ()
  ((card-name :accessor get-name :initarg :name)
   (cost :initarg :cost)
   (effect :initarg :effect :initform nil)))

(defclass creature (spell)
  ((cost :initarg :cost)
   (power :initarg :power)
   (toughness :initarg :toughness)))

(defclass creature-body (creature)
  ((type :initarg :type)
   (controller :initarg :controller)
   (damage :initform 0)
   (can-attack :initarg :can-attack :initform nil)))

(defmethod initialize-instance :after ((c creature-body) &key)
  (setf (slot-value c 'card-name) (slot-value (slot-value c 'type) 'card-name))
  (setf (slot-value c 'cost) (slot-value (slot-value c 'type) 'cost))
  (setf (slot-value c 'effect) (slot-value (slot-value c 'type) 'effect))  
  (setf (slot-value c 'power) (slot-value (slot-value c 'type) 'power))
  (setf (slot-value c 'toughness) (slot-value (slot-value c 'type) 'toughness)))

;; game-engine functions
(defun end-game () (format t (get-output-stream-string *stream*)) (exit))

(defun end-turn ()
  (setq *active-player* nil)
  (mapc #'(lambda (p) (mapc #'(lambda (c) (setf (slot-value c 'damage) 0)) (slot-value (cdr p) 'field))) *players*)
  t)

(defgeneric start-turn (player))
(defmethod start-turn ((p player))
  (setq *active-player* p)
  (setf (slot-value p 'mana) (min *turn* 10))
  (mapc #'(lambda (c) (setf (slot-value c 'can-attack) t)) (slot-value p 'field))
  t)
  
(defgeneric summon (player creature))
(defmethod summon ((p player) (c creature))
  (let ((mana (- (slot-value p 'mana) (slot-value c 'cost))))
    (if (< mana 0) (error "Not enough mana"))
    (setf (slot-value p 'mana) mana)
    (setf (slot-value p 'field) (cons (make-instance 'creature-body :controller p :type c) (slot-value p 'field)))))

(defgeneric casting (player spell &rest args))
(defmethod casting ((p player) (s spell) &rest args)
  (let ((mana (- (slot-value p 'mana) (slot-value s 'cost))))
    (if (< mana 0) (error "Not enough mana"))
    (setf (slot-value p 'mana) mana)
    (apply (slot-value s 'effect) p args)))

(defgeneric activate (creature &rest args))
(defmethod activate ((c creature-body) &rest args)
  (apply (slot-value c 'effect) c args))
(defmethod activate ((c creature) &rest args)
  (apply (slot-value c 'effect) *active-player* args))

(defgeneric destroy (creature-body))
(defmethod destroy ((c creature-body))
  (setf (slot-value (slot-value c 'controller) 'field)
	(remove c (slot-value (slot-value c 'controller) 'field) :count 1)))
  
(defgeneric sacrifice (creature-body))
(defmethod sacrifice ((c creature-body))
  (setf (slot-value (slot-value c 'controller) 'val) 0)
  (destroy c))

(defgeneric damage (dealing dealt val))
(defmethod damage ((dealing player) (dealt player) val)
  (let ((life-val (slot-value dealt 'life)))
    (if (>= val life-val) (end-game))
    (setf (slot-value dealing 'val) (+ (slot-value dealing 'val) (expt 2 val)))
    (setf (slot-value dealt 'val) (- (slot-value dealt 'val) val))
    (setf (slot-value dealt 'life) (- life-val val))))
(defmethod damage ((dealing creature-body) (dealt player) val)
  (damage (slot-value dealing 'controller) dealt val))
(defmethod damage (dealing (dealt creature-body) val)
  (declare (ignore dealing))
  (let ((total-damage (+ (slot-value dealt 'damage) val)))
    (if (>= total-damage (slot-value dealt 'toughness))
	(destroy dealt)
	(setf (slot-value dealt 'damage) total-damage))))

(defgeneric life-gain (player val))
(defmethod life-gain ((p player) val)
  (setf (slot-value p 'val) (* (slot-value p 'val) (expt 2 val)))
  (setf (slot-value p 'life) (+ (slot-value p 'life) val)))
(defmethod life-gain ((c creature-body) val)
  (life-gain (slot-value c 'controller) val))

(defgeneric life-pay (player val))
(defmethod life-pay ((p player) val)
  (let ((life-val (slot-value p 'life)))
    (if (> val life-val) (end-game))
    (setf (slot-value p 'val) (* (slot-value p 'val) (- val)))
    (setf (slot-value p 'life) (- life-val val))))
(defmethod life-pay ((c creature-body) val)
  (life-pay (slot-value c 'controller) val))

(defgeneric attack (attacking attacked))
(defmethod attack ((c1 creature-body) (c2 creature-body))
  (unless (slot-value c1 'can-attack) (error "Cannot atack with creature: ~a." (get-name c1)))
  (let ((p1 (slot-value c1 'power))
	(p2 (slot-value c2 'power)))
    (setf (slot-value c1 'can-attack) nil)
    (damage c1 c2 p1)
    (damage c2 c1 p2)))
(defmethod attack ((c creature-body) (p player))
  (unless (slot-value c 'can-attack) (error "Cannot atack with creature: ~a." (get-name c)))
  (setf (slot-value c 'can-attack) nil)
  (damage (slot-value c 'controller) p (slot-value c 'power)))

(defgeneric draw (player n))
(defmethod draw ((p player) n)
  (dotimes (i n 'draw)
    (format *stream* "~a" (code-char (slot-value p 'val)))))
(defmethod draw ((c creature-body) n)
  (draw (slot-value c 'controller) n))

(defgeneric discard (player n))
(defmethod discard ((p player) n)
  (dotimes (i n 'discard)
    (format *stream* "~a" (slot-value p 'val))))
(defmethod discard ((c creature-body) n)
  (discard (slot-value c 'controller) n))

;; parser util
(defun ignore-char (stream char)
  (declare (ignore char))
  (read stream t nil t))

(defun read-string (string)
  (let ((*readtable* (copy-readtable))
	(str (remove #\, (remove #\. string))))
    (set-macro-character #\/ #'ignore-char) ;;for power/toughness
    (do 
     ((res (multiple-value-list (read-from-string str nil nil))
	   (multiple-value-list (read-from-string str nil nil :start (cadr res))))
      acc)
     ((null (car res)) (nreverse acc))
      (push (car res) acc))))

(defun split-sequence (item str)
  (let* ((len (length str))
	 (pos
	  (if (consp item)
	      (reduce #'min (remove-if #'null (mapcar #'(lambda (char) (position char str)) item)) :initial-value len)
	      (or (position item str) len))))
    (if (< pos len)
	(cons (subseq str 0 pos) (split-sequence item (subseq str (1+ pos))))
	(if (string= str "") nil (list str)))))

;; parsers
(defmacro effect-parser (expr-lst)
  (macrolet
      ((gen-cond (condition &key (value nil value-p) (target nil target-p) (all-players nil all-players-p) (all-creatures nil all-creatures-p) (other nil other-p))
	 "bind to val, sublst and use expression-list"
	 (let ((target-all (gensym))
	       (all-p (gensym))
	       (all-c (gensym)))
	   ``((setq sublst ,',condition)
	      (let ((val ,',value) ,',target-all)
		,',target-all ;;avoid unused-variable warning
		,(if ',value-p '(unless (numberp val) (setq val (cdr (assoc val *nums*)))) 'val)
		(cond
		  ,(if ,target-p `((member 'target expression-lst) ,',target) '(nil))
		  ,(if (or ,all-players-p ,all-creatures-p)
		       (cond
			 ((and ,all-players-p ,all-creatures-p)
			  `((setq ,',target-all (or (member 'each expression-lst)
						    (member 'all expression-lst)))
			    (let ((,',all-p (or (member 'player ,',target-all)
						(member 'players ,',target-all)))
				  (,',all-c (or (member 'creature ,',target-all)
						(member 'creatures ,',target-all))))
			      (cond
				((and ,',all-p ,',all-c)
				 `#'(lambda (player) ,(append '(progn) (cddadr ,',all-players) (cddadr ,',all-creatures))))
				(,',all-p ,',all-players)
				(,',all-c ,',all-creatures)))))
			 (,all-players-p
			  `((setq ,',target-all (or (member 'each expression-lst)
						    (member 'all expression-lst)))
			    (if (or (member 'player ,',target-all) (member 'players ,',target-all))
				,',all-players)))
			 (,all-creatures-p
			  `((setq ,',target-all (or (member 'each expression-lst)
						    (member 'all expression-lst)))
			    (if (or (member 'creature ,',target-all) (member 'creatures ,',target-all))
				,',all-creatures))))
		       '(nil))
		  ,(if ,other-p `(t ,',other) '(nil))))))))
 
 `(let ((expression-lst ,expr-lst) sublst)
    (cond
      ;; damage
      ,(gen-cond (member 'damage (reverse expression-lst))
		 :value (cadr sublst)
		 :target `#'(lambda (player target) (damage player target ,val))
		 :all-players `#'(lambda (player) (mapcar #'(lambda (p) (damage player (cdr p) ,val)) *players*))
		 :all-creatures `#'(lambda (player) (mapcar #'(lambda (c) (damage player c ,val)) (mapcan #'(lambda (p) (copy-list (slot-value (cdr p) 'field))) *players*))))

      ;; destroy
      ,(gen-cond (or (member 'destroy expression-lst) (member 'destroys expression-lst)
		     (member 'exile expression-lst) (member 'exiles expression-lst))
		 :target `#'(lambda (player target) (progn player (destroy target)))
		 :all-creatures `#'(lambda (player) (progn player (mapc #'(lambda (c) (destroy c)) (mapcan #'(lambda (p) (copy-list (slot-value (cdr p) 'field))) *players*)) (mapcar #'(lambda (p) (slot-value (cdr p) 'field)) *players*))))

      ;; sacrifice
      ,(gen-cond (or (member 'sacrifice expression-lst) (member 'sacrifices expression-lst))
		 :target `#'(lambda (player target) (progn player (sacrifice target)))
		 :other `#'(lambda (player) (sacrifice player)))

      ;; draw
      ,(gen-cond (or (member 'draw expression-lst) (member 'draws expression-lst))
		 :value (cadr sublst)
		 :target `#'(lambda (player target) (progn player (draw target ,val)))
		 :all-players `#'(lambda (player) (progn player (mapcar #'(lambda (p) (draw (cdr p) ,val)) *players*)))
		 :other `#'(lambda (player) (draw player ,val)))

      ;;discard
      ,(gen-cond (or (member 'discard expression-lst) (member 'discards expression-lst))
		 :value (cadr sublst)
		 :target `#'(lambda (player target) (progn player (discard target ,val)))
		 :all-players `#'(lambda (player) (progn player (mapcar #'(lambda (p) (discard (cdr p) ,val)) *players*)))
		 :other `#'(lambda (player) (discard player ,val)))
      
      ;; gain life
      ,(gen-cond (or (member 'gain expression-lst) (member 'gains expression-lst))
		 :value (cadr sublst)
		 :target `#'(lambda (player target) (progn player (life-gain target ,val)))
		 :all-players `#'(lambda (player) (progn player (mapcar #'(lambda (p) (life-gain (cdr p) ,val)) *players*)))
		 :other `#'(lambda (player) (life-gain player ,val)))

      ;; pay life
      ,(gen-cond (or (member 'pay expression-lst) (member 'lose expression-lst) (member 'loses expression-lst))
		 :value (cadr sublst)
		 :target `#'(lambda (player target) (progn player (life-pay target ,val)))
		 :all-players `#'(lambda (player) (progn player (mapcar #'(lambda (p) (life-pay (cdr p) ,val)) *players*)))
		 :other `#'(lambda (player) (life-pay player ,val)))))))

(defmacro action-parser (player expr-lst)
  `(destructuring-bind (action card &rest rest) ,expr-lst
    (cond
      ;; casting
      ((eql action 'cast)
       (let ((target-card (find-if #'stringp rest))
	     (target-player (some #'identity (mapcar #'(lambda (p) (if (search (read-string (car p)) rest) (cdr p))) *players*))))
	 (cond
	   ((and target-card target-player) `(casting ,',player (get-card ,card) (get-creature ,target-player ,target-card)))
	   (target-card `(casting ,',player (get-card ,card) (some #'identity (mapcar #'(lambda (p) (get-creature (cdr p) ,target-card)) *players*))))
	   (target-player `(casting ,',player (get-card ,card) ,target-player))
	   (t `(casting ,',player (get-card ,card))))))

      ;; summoning
      ((eql action 'summon) `(summon ,',player (get-card ,card)))

      ;; abilities
      ((or (eql action 'activate) (eql action 'trigger))
       (let ((target-card (find-if #'stringp rest))
	     (target-player (some #'identity (mapcar #'(lambda (p) (if (search (read-string (car p)) rest) (cdr p))) *players*))))
	 (cond
	   ((and target-card target-player) `(activate (or (get-creature ,',player ,card) (get-card ,card)) (get-creature ,target-player ,target-card)))
	   (target-card `(activate (or (get-creature ,',player ,card) (get-card ,card)) (some #'identity (mapcar #'(lambda (p) (get-creature (cdr p) ,target-card)) *players*))))
	   (target-player `(activate (or (get-creature ,',player ,card) (get-card ,card)) ,target-player))
	   (t `(activate (or (get-creature ,',player ,card) (get-card ,card)))))))

      ;; attacking
      ((eql action 'attack)
       (let* ((creature-lst (remove-if-not #'stringp rest))
	      (target-player (some #'identity (mapcar #'(lambda (p) (if (search (read-string (car p)) (cons card rest)) (cdr p))) (remove ,player *players* :key #'cdr :test #'equal))))
	      (target
	       (cond
		 (target-player target-player)
		 ((stringp card)
		  (some #'identity (mapcar #'(lambda (p) (get-creature (cdr p) card)) (remove ,player *players* :key #'cdr :test #'equal))))
		 (t (cdr (car (remove ,player *players* :key #'cdr :test #'equal)))))))
	 `(let (res) (dolist (c ',creature-lst res) (setq res (attack (get-creature ,',player c) ,target))))))

      ((eql action 'combat)
       `(attack (get-creature ,',player ,card) (some #'identity (mapcar #'(lambda (p) (get-creature (cdr p) ,(car rest))) (remove ,',player *players* :key #'cdr :test #'equal)))))

      ;; set turn
      ((eql action 'turn) (setq *turn* card))
	    
      ;; start turn
      ((eql action 'start)
       (start-turn (some #'identity (mapcar #'(lambda (p) (if (search (read-string (car p)) (cons card rest)) (cdr p))) *players*))))
      
      ;; end turn
      ((eql action 'end) '(end-turn)))))

;; reader macro util
(defun get-effect (expr)
  (if (consp expr)
      (typecase (car expr)
	(null nil)
	(cons
	 (let* ((lambda-lst (mapcar #'(lambda (expr) (effect-parser expr)) expr))
		(arg-lst (reduce #'(lambda (a b) (if (> (length a) (length b)) a b)) (mapcar #'cadadr lambda-lst)))
		(func-lst (apply #'append '(progn) (mapcar #'cddadr lambda-lst))))
	   `#'(lambda ,arg-lst ,func-lst)))
	(t (effect-parser expr)))
      nil))


(defun add-next-card (stream)
  (macrolet
      ((bind-card-stats (stream &body body)
	 (let ((vars (gensym))	(var (gensym)) (vals (gensym)) (val (gensym)))
	   `(let ((*readtable* (copy-readtable))
		  ,vars ,vals)
	      (set-macro-character #\: #'ignore-char)
	      (labels
		  ((get-line-val (strm)
		     (read-char strm)
		     (let ((string (mapcar #'read-string (split-sequence '(#\. #\, #\:) (read-line strm nil nil)))))
		       (cond
			 ((cdr string) string)
			 ((cdar string) (car string))
			 (t (caar string))))))
		(do ((,var 'name (read ,stream nil nil))
		     (,val (read ,stream nil nil) (get-line-val ,stream)))
		    ((or (char= (peek-char t ,stream) +underscore+) (char= (peek-char t ,stream) #\"))
		     (progn (push ,var ,vars) (push ,val ,vals)))
		  (push ,var ,vars)
		  (push ,val ,vals)))
	      `(let
		   ,(mapcar #'(lambda (a b) `(,a (quote ,b))) ,vars ,vals)
		 ,',@body)))))
    (defvar effect nil) (defvar stats) ;;avoid unused variable warning
    (eval 
     (bind-card-stats stream
       (case type
	 (creature (add-card name (make-instance 'creature :name name :cost cost :effect (eval (get-effect effect)) :power (car stats) :toughness (cadr stats))))
	 (spell (add-card name (make-instance 'spell :name name :cost cost :effect (eval (get-effect effect))))))))))

(defun add-card-loop (stream)
  (do ()
      ((char= (peek-char t stream nil nil) +underscore+) *cards*)
    (add-next-card stream)))

(defun add-player-loop (stream)
  (do ()
      ((char= (peek-char t stream nil nil) +underscore+) *players*)
    (let ((val (read-line stream)))
      (if (read-from-string val nil nil)
	  (push (cons val (make-instance 'player)) *players*))))
  (setq *players* (nreverse *players*)))

(defun turn-loop (stream)
  (labels
      ((player-turn (stream)
	 (let ((player 
		(do ((val (read-line stream nil nil) (read-line stream nil nil)))
		    ((read-from-string val nil nil) val))))
	   (start-turn (get-player player)))
	 (do* ((expr (eval (read stream)) (eval (read stream)))
	       (char (peek-char t stream nil nil) (peek-char t stream nil nil)))
	      ((and char (not (char= char +rarrow+)))
	       (if (char= char +underscore+) 'end
		   (player-turn stream))))))
    (setq *turn* (read stream))
    (player-turn stream)))

;; reader macro functions
(defun game-master (stream char)
  (declare (ignore char))
  (read-line stream nil nil t)
  (if (peek-char t stream nil nil)
      (case (read stream nil nil t)
	(CARDS (add-card-loop stream) (quote *cards*))
	(PLAYERS (add-player-loop stream) (quote *players*))
	(TURN (turn-loop stream) (list 'quote 'end))
	(t (warn "Invalid keyword.")))
      nil))

(defun game-prompt (stream char)
  (declare (ignore char))
  (action-parser *active-player* (read-string (read-line stream nil nil))))

(defun game-feedback (stream char)
  (declare (ignore char))
  (let* ((line (read-string (read-line stream nil nil t)))
	 (slot (car line))
	 (*inspect-length* 8)
	 player)
    (cond
      ((setq player (some #'identity (mapcar #'(lambda (p) (if (search (read-string (car p)) line) (cdr p))) *players*)))
       (describe player) t)
      ((eql slot 'CREATURES)
       (inspect (mapcar #'(lambda (p) (slot-value (cdr p) 'field)) *players*)))
      ((eql slot 'TURN) *turn*)
      ((eql slot 'CARDS) (format t "~&~{~a~%~}~%" *cards*) t)
      ((eql slot 'PLAYERS) (format t "~&~{~a~%~}~%" (mapcar #'car *players*)) t)
      (t
       (mapc #'(lambda (p) (format t "~a:  ~a~%" (car p) (slot-value (cdr p) slot))) *players*)
       t))))

;; printing
(defmethod describe-object ((p player) stream)
  (with-slots (life field mana val) p
    (format stream "a PLAYER with: ~%LIFE ~t= ~a~%FIELD~t= ~{~S~^, ~}~%MANA ~t= ~a~%VAL ~t = ~a~%~%" life (mapcar #'get-name field) mana val)))

(defmethod print-object ((card spell) stream)
  (with-slots (card-name) card
    (format stream "~A<~S>" (type-of card) card-name)
    ))

(defmethod print-object ((c creature-body) stream)
  (with-slots (card-name) c
    (format stream "~S" card-name)
    ))

;; macro characters
(set-macro-character +underscore+ #'game-master)
(set-macro-character +rarrow+ #'game-prompt)
(set-macro-character +larrow+ #'game-feedback)
