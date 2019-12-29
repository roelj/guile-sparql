;;; Copyright © 2018, 2019 Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (sparql util)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (web response)

  ;; The following functions are used in the macros for ‘query-results->list’
  ;; and ‘query-results->alist’.  We re-export them from this module so that
  ;; these modules need not to be imported when using these macros.
  #:re-export (response-code receive read-line)

  #:export (display-query-results
            display-query-results-of
            query-results->list
            query-results->alist
            csv-read-entry
            uri-suffix
            uri-base))

(define* (display-query-results port #:optional (is-header? #t))
  "Writes CSV data from PORT to the standard output port."
  (let* ((line   (read-line port)))
    (if (eof-object? line)
        #t
        (let ((tokens (map (lambda (item) (string-trim-both item #\"))
                           (string-split line #\,))))
          (format #t "~{~a~/~}~%" tokens)
          (when is-header?
            (format #t "~{~a~/~}~%"
                    (map (lambda (token)
                           (make-string (string-length token) #\-))
                         tokens)))
          (display-query-results port #f)))))

(define-syntax-rule
  (display-query-results-of query)
  (receive (header port)
      query
    (if (= (response-code header) 200)
        (display-query-results port)
        (format #t "Error (~a): ~a~%"
                (response-code header)
                (read-line port)))))

(define* (query-results-to-list port #:optional (skip-first-line? #f)
                                                (output '()))
  "Returns a list of data read from PORT."
  (when skip-first-line? (csv-read-entry port))
  (let* [(entry (csv-read-entry port))]
    (if (null? entry)
        (reverse output)
        (query-results-to-list port #f (cons entry output)))))

(define-syntax-rule
  (query-results->list query skip-first-line?)
  (receive (header port)
      query
    (if (= (response-code header) 200)
        (query-results-to-list port skip-first-line?)
        (begin
          (format #t "Error (~a): ~a~%"
                  (response-code header)
                  (read-line port))
          #f))))

(define* (query-results-to-alist port #:optional (header '())
                                      (output '()))
  (let [(entry (csv-read-entry port))]
    (if (null? entry)
        (reverse output)
        (if (null? header)
            (query-results-to-alist port entry output)
            (query-results-to-alist port header
              (cons
               (map (lambda (t) `(,(list-ref t 0) . ,(list-ref t 1)))
                    (zip header entry))
               output))))))

(define-syntax-rule
  (query-results->alist query)
  (receive (header port)
      query
    (if (= (response-code header) 200)
        (query-results-to-alist port)
        (begin
          (format #t "Error (~a): ~a~%"
                  (response-code header)
                  (read-line port))
          #f))))

(define* (csv-read-entry port
                         #:optional
                         (delimiter #\,)
                         (current-token '())
                         (tokens '())
                         (in-quote #f))
  (let [(character (read-char port))]
    (cond
     [(or (eof-object? character)
          (and (eq? character #\newline)
               (not in-quote)))
      (if (and (null? current-token)
               (null? tokens))
          current-token
          (map (lambda (item)
                 (let [(item-length (string-length item))]
                   (cond
                    [(and (> item-length 1)
                          (eq? (string-ref item 0) #\")
                          (eq? (string-ref item (- item-length 1)) #\"))
                     (substring item 1 (- item-length 1))]
                    [else item])))
               (reverse (cons (list->string (reverse current-token)) tokens))))]
     [(eq? character #\")
      (if (and (not (null? current-token))
               (or (eq? (car current-token) #\\)
                   (eq? (car current-token) #\")))
          (csv-read-entry port delimiter (cons character (cdr current-token)) tokens in-quote)
          (csv-read-entry port delimiter (cons character current-token) tokens (not in-quote)))]
     [(and (eq? character delimiter)
           (not in-quote))
      (csv-read-entry port delimiter '()
                      (cons (list->string (reverse current-token)) tokens)
                      in-quote)]
     [else
      (csv-read-entry port delimiter (cons character current-token) tokens in-quote)])))

(define (uri-suffix input)
  (catch #t
    (lambda ()
      (string-trim-both (string-drop input (1+ (string-rindex input #\/))) #\"))
    (lambda (key . args)
      input)))

(define (uri-base input)
  (string-append (dirname input) "/"))
