; `((H a A) (A c C) (A a B) (B d D) (B c E) (D a B) (D c C) (E a B) (E c C) (C e S))
; (main `((H b S) (H a B) (H a A) (A b B) (A a A)))
; (main `((H a B) (H "0" A) (A c H) (A "1" B) (B b B) (B "0" S) (C "3" B) (C a H)))
; (determine `((H "1" S) (H "0" B) (H "0" A) (A "1" B) (A "0" A)))


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


; Вставить в список по ключу k в словаре l значение v
(defun insert-append (l k v)
   (cond ((null l) (list (list k (list v))))
		 ((equal (caar l) k) (cons (list k (cons v (cadar l))) (cdr l)))
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
	(let* ((nodes (sort (get-determine-nodes l () `(H) `()) #'(lambda (a b) (> (length (string a)) (length (string b)))))))
		(construct-determine l nodes nodes)))
	
; Возвращает ДКА по НКА и списку новых состояний
(defun construct-determine (l nodes nodes-const)
	(cond 
		((null nodes) nil)
		(T (append (construct-process l (car nodes) nodes-const) (construct-determine l (cdr nodes) nodes-const)))))
		

(defun construct-process (l node nodes-const)
	(cond 
		((null l) NIL)
		((string-include (caar l) node) (cons (list (caar l) (nth 1 (car l)) (check-new-node (nth 2 (car l)) nodes-const)) (construct-process (cdr l) node nodes-const)))
		(T (construct-process (cdr l) node nodes-const))))


; Ищет новую вершину, соответствующую старой ( "А" -> "AB" )
(defun check-new-node (node nodes)
	;(print nodes)
	(cond 
		((null nodes) ("ERR"))
		((string-include (string node) (string (car nodes))) (car nodes))
		(T (check-new-node node (cdr nodes)))))
				
#|
; Строит элементы ДКА по одному состоянию		
(defun construct-routes (l node nodes-const routes)
	(cond
		((null l) routes)
		((string-include (string (caar l)) node) (construct-routes (cdr l) node nodes-const (cons (list node (cadar l) (search-node (caddar l) nodes-const)))))
		(T (construct-routes (cdr l) node nodes-const routes))))


; Ищет подходящую вершину в новых по старой
(defun search-node (node nodes)
	(cond 
		((null nodes) NIL)
		((string-include (string node) (car nodes)) (car nodes))
		(T (search-node node (car nodes)))))
|#

; Строит список вершин ДКА по заданому автомату
(defun get-determine-nodes (l new q processed)
	(let*
		((node (get-last q)))
		(cond
			((null q) new)
			;(T (print (get_new_nodes l (strike-last q) (cons node processed) node ()))))))
			(T (get-determine-nodes l (cons node new) (get_new_nodes l (strike-last q) (cons node processed) node ()) (cons node processed))))))


; Добавляет в очередь q необработанные состояния, в которые можно попасть из состояния node
;(defun get_new_nodes (l q processed node)
;	(cond
;		((null l) q)
;		((equal (caar l) node)
;			(cond 
;				((not (or (member (caddar l) processed) (member (caddar l) q))) (get_new_nodes (cdr l) (cons (caddar l) q) processed node))
;				(T (get_new_nodes (cdr l) q processed node))))
;		(T (get_new_nodes (cdr l) q processed node))))


; Добавляет в очередь q необработанные состояния, в которые можно попасть из состояния node
(defun get_new_nodes (l q processed node d)
	(cond
		((null l) (update-q d q processed))
		;((c (A B)) (d (A S)))
		;((null l) d)
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


; 758