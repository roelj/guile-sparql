;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
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
  #:use-module (web response)
  #:export (display-query-results
            display-query-results-of
            query-results->list
            split-line
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

(define* (query-results-to-list port #:optional (output '()))
  "Returns a list of data read from PORT."
  (let* ((line   (read-line port)))
    (if (eof-object? line)
        (reverse output)
        (query-results-to-list port
         (cons (map (lambda (item) (string-trim-both item #\"))
                    (string-split line #\,))
               output)))))

(define-syntax-rule
  (query-results->list query)
  (receive (header port)
      query
    (if (= (response-code header) 200)
        (query-results-to-list port)
        (format #t "Error (~a): ~a~%"
                (response-code header)
                (read-line port)))))

(define* (split-line line #:optional (delimiter #\,))
  "Splits LINE where DELIMITER is the separator, properly handling quotes."

  (define (iterator line length token-begin position in-quote tokens)
    (cond
     ((= position length)
      (reverse (cons (string-drop line token-begin) tokens)))
     ((eq? (string-ref line position) delimiter)
      (if in-quote
          (iterator line length token-begin (1+ position) in-quote tokens)
          (iterator line length (1+ position) (1+ position) in-quote
                    (cons (substring line token-begin position)
                          tokens))))
     ((eq? (string-ref line position) #\")
      (iterator line length token-begin (1+ position) (not in-quote) tokens))
     (else (iterator line length token-begin (1+ position) in-quote tokens))))

  (iterator line (string-length line) 0 0 #f '()))

(define (uri-suffix input)
  (catch #t
    (lambda ()
      (string-trim-both (string-drop input (1+ (string-rindex input #\/))) #\"))
    (lambda (key . args)
      input)))

(define (uri-base input)
  (string-append (dirname input) "/"))
