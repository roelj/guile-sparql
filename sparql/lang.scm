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
  #:export (prefix select insert-data delete-data create))

;;
;; UTILITIES
;; ----------------------------------------------------------------------------
;;

(define (variabilize item)
    (cond
     ((string? item)    item)
     ((symbol? item)    (string-append "?" (symbol->string item)))
     (else              (format #f "?~a" item))))

(define (keyword-processor keyword)
  (if (list? keyword)
      (format #f "~{~a ~}" (map keyword-processor keyword))
      (match keyword
        ('and         'AND)
        ('ascending   'ASCENDING)
        ('ask         'ASK)
        ('by          'BY)
        ('construct   'CONSTRUCT)
        ('datatype    'DATATYPE)
        ('descending  'DESCENDING)
        ('describe    'DESCRIBE)
        ('distinct    'DISTINCT)
        ('filter      'FILTER)
        ('from        'FROM)
        ('graph       'GRAPH)
        ('group       'GROUP)
        ('isliteral   'ISLITERAL)
        ('lang        'LANG)
        ('langmatches 'langmatches)
        ('limit       'LIMIT)
        ('named       'NAMED)
        ('offset      'OFFSET)
        ('optional    'OPTIONAL)
        ('or          'OR)
        ('order       'ORDER)
        ('regex       'REGEX)
        ('sameterm    'SAMETERM)
        ('str         'STR)
        ('where       'WHERE)
        (_           (variabilize keyword)))))

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

(define* (select columns pattern #:optional (suffix #f) #:key (graph #f))

  (string-append
   (format #f "SELECT ~{~a ~}~a~%{~%~{~a~}}~%"

           ;; Translate the columns into SPARQL-like selectors.
           (map variabilize columns)

           ;; When the graph is known, add it to the query.
           (if graph
               (string-append "FROM <" graph "> ")
               "")

           ;; Translate the triples into SPARQL-like patterns.
           (map (lambda (triple)
                  (format #f "  ~{~a ~}.~%" (map variabilize triple)))
                pattern))

   ;; Translate the suffixes into valid SPARQL.
   (if suffix
       (format #f "~{~a~%~}" (map keyword-processor suffix))
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

;;
;; INSERT/DELETE DATA
;; ----------------------------------------------------------------------------
;;
;; The following functions implement the INSERT DATA and DELETE DATA
;; constructs.

(define* (modify-data pattern action #:key (graph #f))

  (when (not graph)
    (throw 'wrong-number-of-args "Expected value for 'graph"))

  (format #f "~a DATA {~%  GRAPH <~a> {~%~{~a~}  }~%}~%"
          action
          graph
          (map (lambda (triple)
                 (format #f "    ~{~a ~}.~%" (map variabilize triple)))
               pattern)))

(define* (insert-data pattern #:key (graph #f))
  (modify-data pattern 'INSERT #:graph graph))

(define* (delete-data pattern #:key (graph #f))
  (modify-data pattern 'DELETE #:graph graph))
