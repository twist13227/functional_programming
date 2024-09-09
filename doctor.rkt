; заготовка "Доктора". Сентябрь 2023
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name)
)


; основная функция, запускающая "Доктора"
; параметр stop-word - стоп-слово, после которого доктор прекращает работу (вводится вместо имени)
; параметр max-patients-number - максималное количество пациентов, после которого доктор прекращает работу
(define (visit-doctor-v2 stop-word max-patients-number) (
  if (< max-patients-number 1)
     (println '(time to go home))
  (let ((name (ask-patient-name))) (
      if (equal? name stop-word)
         (println '(time to go home))
      (
        begin
        (printf "Hello, ~a!\n" name)
        (print '(what seems to be the trouble?))
        (doctor-driver-loop-v2 name)
        (visit-doctor-v2 stop-word (- max-patients-number 1))
      )
   ))
))


; функция, которая запрашивает имя пациента
(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (println '(see you next week)))
            (else (print (reply-v2 user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop-v2 name)(
    let loop ((name name) (history #())) (
        begin
        (newline)
        (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
        (let ((user-response (read)))
            (cond 
	        ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
                 (printf "Goodbye, ~a!\n" name)
                 (print '(see you next week)))
                (else
                 (
                  begin
                  (print (reply-v2 answer-types user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                    (cond ((= (vector-length history) 5) 
                        (
                            loop name (vector-append (vector-drop history 1) (vector user-response))
                                 ; если сохранено уже 5 реплик пользователя, то нужно стереть первую, прежде чем записать новую
                        ))
                    (else (loop name (vector-append history (vector user-response))))
                                 ; иначе просто добавляем новую реплику в список
                    )
                ))
            )
        )
    )
))


; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history)
      (case (random (if (can-use-keyword-answer user-response) -1 0) (if (equal? history #()) 2 3))
      ; с равной вероятностью выбирается один из трёх способов построения ответа
      ; учитывая тот факт, что 3 способ можно выбрать только если история ответов пользователя не пуста
          ((-1) (keyword-answer user-response)) ; 0й способ
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history)) ; 3й способ

      )
)


; генерация ответной реплики по реплике от пользователя и вектора прошлых реплик
; работает с подаваемой на вход структурой answer-types, хранящей информацию о возможных стратегиях ответа
(define (reply-v2 answer-types user-response history-vctr) (
  let ((enabled-types (filter (lambda (x) ((car x) user-response history-vctr)) answer-types)))
   ((caddr (pick-random-list-with-weight enabled-types)) user-response history-vctr)
))


; структура, описывающая стратегии ответа (список из функции-чеккера, веса и функции-тела)
; аргументы: user-response и history-vctr + какие-то опциональные (если понадобятся для других стратегий)
(define answer-types (
  list
  ; 1-й способ (hedge-answer)
  (
    list
      (lambda args #t)
      1
      (lambda args (let ((user-response (car args)) (history-vctr (cadr args)))
          (hedge-answer)
      ))
  )
  ; 2-й способ (qualifier-answer)
  (
    list
      (lambda args #t)
      3
      (lambda args (let ((user-response (car args))(history-vctr (cadr args))) 
          (qualifier-answer user-response)
      ))
  )
  ; 3-й способ (history-answer)
  (
    list
      (lambda args (let ((user-response (car args))(history-vctr (cadr args)))
          (not (equal? history-vctr #()))
      ))
      3
      (lambda args (let ((user-response (car args))(history-vctr (cadr args))) 
          (history-answer history-vctr)
      ))
  )
  ; 4-й способ (keyword-answer)
  (
    list
      (lambda args (let ((user-response (car args))(history-vctr (cadr args)))
          (can-use-keyword-answer user-response)
      ))
      13
      (lambda args (let ((user-response (car args))(history-vctr (cadr args))) 
          (keyword-answer user-response)
      ))
  )
))


; выбор случайного элемента из списка списков, где вес каждого списка есть его второй элемент
(define (pick-random-list-with-weight lsts) (
  let* (
    (weights-sum (foldl (lambda (lst total) (+ total (cadr lst))) 0 lsts))
    (point (random 1 (+ weights-sum 1)))
  ) (
      call/cc (lambda (exit-cc) (
          foldl (lambda (lst cur-sum) (
              let ((cur (+ cur-sum (cadr lst)))) (
                  if (>= cur point)
                      (exit-cc lst)
                      cur
              )
          )) 0 lsts
      ))
      
  )
))


; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (tell me more about it please)
                              (this problem is very familiar to me from life experience)
                              (does this problem hurt your self-esteem?)
                              )
         )
)


; случайный выбор одного из элементов непустого списка
(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst)))
)


; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (are you sure that)
                                       (how did you come to the fact that)
                                       (why are you afraid that)
                                       )
                )
                (change-person user-response)
        )
 )

; 3й способ генерации ответной реплики — использовать одну из последних 5 фраз, которые сказал пользователь
(define (history-answer history-vctr) (
  append '(earlier you said that) (change-person (pick-random-vector history-vctr))
))


(define (can-use-keyword-answer user-response)
  (ormap (lambda (keyword) (vector-member keyword all-keywords))
         user-response))


; 4й способ генерации ответной реплики — зависящий от ключевых слов в реплике пациента 
(define (keyword-answer user-response) (
  let* (
    (keyword (pick-random-list (filter-keywords user-response)))
    (template (pick-random-vector (all-keyword-templates keyword)))
  ) (many-replace-v3 (list (list '* keyword)) template)
))

; оставляем только ключевые слова из входной строки
(define (filter-keywords user-response) (
  filter (lambda (word) (vector-member word all-keywords)) user-response
))


; получаем единый вектор шаблонов, сопоставленных ключевому слову
; (на случай если ключевое слово соответствует разным шаблонам)
(define (all-keyword-templates keyword) (
  vector-foldl (lambda (idx result x) (vector-append result x)) #()
     (vector-map (lambda (v) (vector-ref v 1))(vector-filter (lambda (t) (vector-member keyword (vector-ref t 0))) keywords-structure))
))

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		'((am are)
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
; это рекурсивная версия, которая не используется после упражнения 2
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
; это итеративная версия, реализованная в упражнении 2
(define (many-replace-v2 replacement-pairs lst)(
    let loop ((lst lst) (res '())) (
        cond ((null? lst) (reverse res)) ; переворачиваем результат, потому что он строится задом наперёд
            (else (let ((pat-rep (assoc (car lst) replacement-pairs))) (; Доктор ищет первый элемент списка в ассоциативном списке замен
                loop (cdr lst) (
                    cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                         ) res
                )
             )))
    )
))

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
; это версия, реализованная в упражнении 3
(define (many-replace-v3 replacement-pairs lst)(
    map (
        lambda (keyword) (
            let ((pat-rep (assoc keyword replacement-pairs))) (; Доктор ищет первый элемент списка в ассоциативном списке замен
                if pat-rep (cadr pat-rep) ; если поиск был удачен, то слово заменяется на нужное
                    keyword ; иначе слово остаётся без изменений
            )
        )) lst
))


; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))


; структура с шаблонами
(define keywords-structure '#(
  #(
    #(depressed suicide exams university)
    #(
      (when you feel depressed, go out for curd cake)
      (depression is a disease that can be treated)
      (try to find things that give you happiness)
      (ask your close person to hug you and say that everything would be ok)
      (go to a supermarket and buy yoursels something tasty you like)
	)
  )
  #(
    #(mother father parents brother sister uncle aunt grandma grandpa)
    #(
      (tell me more about your * i want to know all about your *)
      (why do you feel that way about your * ?)
      (tell me about the best moment in your life with * please)
      (i am sure that * loves you!)
	)
  )
  #(
    #(university scheme lections study)
    #(
      (your education is important)
      (how much time do you spend on your studies ?)
      (studying should not take away the joy of life!)
      (tell me more about your problems with * please)
    )
	)
  #(
    #(cry love useless worthless bad terrible toxic)
    #(
      (the suns should not be sad, their smile must shine!)
      (everything is better than you think)
      (tell me more about the situation that happenned)
      (you live only once, live in the moment and everything will get better)
    )
  )
  #(
    #(afraid fear worried scared)
    #(
      (everything will be fine! fears need to be looked in the eye and to be overcame)
      (try to remember something good when you feel *)
      (ask for help from a loved one and he will definitely help you)
      (try to concentrate and bad thoughts will recede)
    )
  )
))


; список уникальных ключевых слов
(define all-keywords (
  vector-foldl (lambda (index res i) (
            vector-append res (vector-filter-not (lambda (word) (vector-member word res)) (vector-ref i 0))
                          )
    ) '#() keywords-structure ;func init vector 
))