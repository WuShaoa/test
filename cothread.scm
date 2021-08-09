 ;;; 用于线程调度的一个朴素的队列。它持有“等待运行”的协程的列表。
   (define *queue* '())
   (define (empty-queue?)
     (null? *queue*))
   (define (enqueue x)
     (set! *queue* (append *queue* (list x))))
   (define (dequeue)
     (let ((x (car *queue*)))
       (set! *queue* (cdr *queue*))
       x))
   ;;; (fork prpc)启动一个新线程来运行(proc)。
   (define (fork proc)
     (call/cc
      (lambda (k)
        (enqueue k)
        (proc))))
   ;;; (yield)将处理器让给下一个线程，如果有的话。
   (define (yield)
     (call/cc
      (lambda (k)
        (enqueue k)
        ((dequeue)))))
   ;;; (thread-exit)终止当前线程，或整个程序如果没有其他线程剩下的话。
   (define (thread-exit)
     (if (empty-queue?)
         (exit)
         ((dequeue))))
   ;;; 典型的Scheme线程的做事的函数体。
   (define (do-stuff-n-print str)
     (lambda ()
       (let loop ((n 0))
         (format #t "~A ~A\n" str n)
         (yield)
         (loop (+ n 1)))))
   ;;; 建立两个线程，并启动运行它们。
   (fork (do-stuff-n-print "This is AAA"))
   (fork (do-stuff-n-print "Hello from BBB"))
   (thread-exit)