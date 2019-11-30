; `((H a A) (A c C) (A a B) (B d D) (B c E) (D a B) (D c C) (E a B) (E c C) (C e S))
; (main `((H b S) (H a B) (H a A) (A b B) (A a A)))
; (main `((H a B) (H "0" A) (A c H) (A "1" B) (B b B) (B "0" S) (C "3" B) (C a H)))
; (determine `((H "1" S) (H "0" B) (H "0" A) (A "1" B) (A "0" A)))
; (construct-process `((H "1" S) (H "0" B) (H "0" A) (A "1" B) (A "0" A)) `(H) `(H) ())
; (determine `((H a A) (H b B) (H c C) (A a A) (A a B) (A c S) (B b H) (B b S) (C c C) (C a A)))
; (determine `((H a A) (H a B) (H b B) (A a A) (A a S) (B a A) (B a B) (B b S)))


(defun main (g)
	(let* 
		((str (stringify-grammar (process g ()))))
		(subseq str 0 (- (length str )1))))

(defun process (g res)
	(cond 
		((null g) res)
		(T (process (cdr g) (insert-append res (caar g) (concatenate 'string (string-downcase (string (cadar g))) (string (caddar g))))))))




; Достать значение по ключу из словаря
(defun find-l (k l)
  (cond ((null l) NIL)
        ((equal (caar l) k) (cadar l))
        (T (find-l k (cdr l)))))


; Вставить в список по ключу k в словаре l значение v (если оно там ещё не присутствует!; иначе не делает ничего)
(defun insert-append (l k v)
   (cond ((null l) (list (list k (list v))))
		 ((equal (caar l) k) 
		 	(cond
		 		((member v (cadar l)) l)
		 		(T (cons (list k (cons v (cadar l))) (cdr l)))))
		 (T (cons (car l) (insert-append (cdr l) k v)))))


; Перевести всю грамматику из словаря в строку для печати
(defun stringify-grammar (g)
	(cond 
		((null g) "")
		(T (concatenate 'string (stringify-node (car g)) ";" (stringify-grammar (cdr g))))))


; Переводит в строку одну вершину
(defun stringify-node (node)
	(let* 
		((str (stringify-list (cadr node))))
	(concatenate 'string (string (car node)) "=" (subseq str 0 (- (length str )1)))))


; Переводит в строку список вершин и ребёр одной вершины
(defun stringify-list (l)
	(cond 
		((null l) "")
		(T (concatenate 'string (replace-all (car l) "S" "") "|" (stringify-list (cdr l))))))
		

; Функция, которая проверяет, содержит ли string2 подстроку string1
(defun string-include (string1 string2)
  (let* ((string1 (string string1)) (length1 (length string1)))
    (if (zerop length1)
        nil 
        (labels ((sub (s)
                   (cond
                    ((> length1 (length s)) nil)
                    ((string= string1 s :end2 (length string1)) string1)
                    (t (sub (subseq s 1))))))
          (sub (string string2))))))
		  

; Функция, заменяющая в строке подстроку на заданную
(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))


; Возвращает последний элемент очереди (по FIFO)
(defun get-last (q)
	(cond
		((null q) NIL)
		(T (cond 
				((null (cdr q)) (car q))
				(T (get-last (cdr q)))))))

; Возвращает очередь без последнего элемента
(defun strike-last (q)
	(cond
		((null q) NIL)
		(T (cond
				((null (cdr q)) NIL)
				(T (cons (car q) (strike-last (cdr q))))))))


; Строит ДКА
(defun determine (l)
	; (let* ((nodes (sort (get-determine-nodes l () `(H) `()) #'(lambda (a b) (> (length (string a)) (length (string b)))))))
	; 	(construct-determine l nodes nodes)))
	(construct-process l `(H) `() ()))
	
; ; Возвращает ДКА по НКА и списку новых состояний
; (defun construct-determine (l nodes nodes-const)
; 	(cond 
; 		((null nodes) nil)
; 		(T (append (construct-process l (car nodes) nodes-const) (construct-determine l (cdr nodes) nodes-const)))))
		

; (defun construct-process (l node nodes-const)
; 	(cond 
; 		((null l) NIL)
; 		((string-include (caar l) node) (cons (list (caar l) (nth 1 (car l)) (check-new-node (nth 2 (car l)) nodes-const)) (construct-process (cdr l) node nodes-const)))
; 		(T (construct-process (cdr l) node nodes-const))))


(defun construct-process (l q processed new)
	;(print q)
	; (print (update-q (get_new_nodes l q processed (get-last q) ()) (strike-last q) processed))
	; (print (get_new_nodes l q processed (get-last q) ()))
	; (print (get-auto (get-last q) (get_new_nodes l q processed (get-last q) ())))
	; (print (cons (get-auto (get-last q) (get_new_nodes l q processed (get-last q) ())) ()))
	(cond 
		((null q) new)
		(T (let* (
			(node (get-last q))
			(d (get_new_nodes l q (cons node processed) node ())))
			(construct-process l (update-q d (strike-last q) (cons node processed)) (cons node processed) (append (get-auto d node) new))
		))))


; Строит часть автомата по словарю d из construct-process и имени состояния node
(defun get-auto (d node)
	(cond 
		((null d) NIL)
		(T (cons (list node (caar d) (concate-nodes (cadar d))) (get-auto (cdr d) node)))
		))


; Ищет новую вершину, соответствующую старой ( "А" -> "AB" )
(defun check-new-node (node nodes)
	;(print nodes)
	(cond 
		((null nodes) ("ERR"))
		((string-include (string node) (string (car nodes))) (car nodes))
		(T (check-new-node node (cdr nodes)))))


; Строит список вершин ДКА по заданому автомату
(defun get-determine-nodes (l new q processed)
	(let*
		((node (get-last q)))
		(cond
			((null q) new)
			;(T (print (get_new_nodes l (strike-last q) (cons node processed) node ()))))))
			(T (get-determine-nodes l (cons node new) (get_new_nodes l (strike-last q) (cons node processed) node ()) (cons node processed))))))


; Добавляет в очередь q необработанные состояния, в которые можно попасть из состояния node
(defun get_new_nodes (l q processed node d)
	(cond
		((null l) d)
		;((c (A B)) (d (A S)))
		;((equal (caar l) node)
		((string-include (string (caar l)) (string node))
			(get_new_nodes (cdr l) q processed node (insert-append d (string (nth 1 (car l))) (nth 2 (car l)))))
		(T (get_new_nodes (cdr l) q processed node d))))


; Принимает список из состояний, в которые можно попасть, добавляет их в очередь
(defun update-q (d q processed)
	(cond 
		((null d) q)
		((not (or (member (concate-nodes (cadar d)) processed :test #'string=) (member (concate-nodes (cadar d)) q :test #'string=))) (update-q (cdr d) (cons (concate-nodes (cadar d)) q) processed))
		(T (update-q (cdr d) q processed))))

; Принимает список из состояний, возвращает строку (новое состояние), состоящее из объединения всех `(A B C) -> "ABC"
(defun concate-nodes (l)
	(cond 
		((null l) "")
		(T (concatenate 'string (string (car l)) (concate-nodes (cdr l))))))


; с 11 в 758
