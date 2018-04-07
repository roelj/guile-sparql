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

(define-module (sparql lang)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (prefix select create))

;;
;; PREFIX
;; ----------------------------------------------------------------------------
;;
;; This macro implements the SPARQL's PREFIX syntax.
;;

(define-syntax-rule
  (prefix uri)
  (lambda (suffix) (string-append "<" uri suffix ">")))

;;
;; SELECT
;; ----------------------------------------------------------------------------
;;
;; Below is the implementation of SPARQL's SELECT syntax.
;;

(define* (select columns pattern #:optional (suffix #f))

  (define (variabilize item)
    (cond
     ((string? item)    item)
     ((list?   item)    item)
     ((symbol? item)    (string-append "?" (symbol->string item)))
     (else              (format #f "?~a" item))))

  (define (suffix-processor keyword)
    (if (list? keyword)
        (format #f "~{~a ~}" (map suffix-processor keyword))
        (match keyword
          ('group 'GROUP)
          ('by    'BY)
          ('sort  'SORT)
          (_      (variabilize keyword)))))

  (string-append
   (format #f "SELECT ~{~a ~}~%{~%~{~a~}}~%"
           ;; Translate the columns into SPARQL-like selectors.
           (map (cut string-append "?" <>)
                (map symbol->string columns))
           ;; Translate the triples into SPARQL-like patterns.
           (map (lambda (triple)
                  (format #f "  ~{~a ~}.~%" (map variabilize triple)))
                pattern))

   ;; Translate the suffixes into valid SPARQL.
   (if suffix
       (format #f "~{~a~%~}" (map suffix-processor suffix))
       "")))

;;
;; CREATE
;; ----------------------------------------------------------------------------
;;
;; The following macro implements SPARQL's CREATE syntax.
;;

(define-syntax-rule
  (create pattern)
  (let ((p (quote pattern)))
    (format #f "CREATE ~a <~a>" (car p) (cadr p))))
