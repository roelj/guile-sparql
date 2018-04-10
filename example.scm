;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

;; We must add the (sparql driver) module to GNU Guile's load path
;; before we can load it.  This means we have to add the directory
;; that leads to the sparql/ directory to the %load-path variable.
; (add-to-load-path "/path/to/the/root/of/the/repository")

;; We need the following modules to query and process the query.
(use-modules (sparql driver)
             (web response)
             (ice-9 receive)
             (ice-9 rdelim)
             (ice-9 format))

(define %example-query "SELECT DISTINCT ?Concept WHERE {[] a ?Concept} LIMIT 9")

;;
;; Simple query response data reader.
;; ----------------------------------------------------------------------------
;; The response data reader takes the port on which the response can be read.
;; This function simply reads the response line-by-line, and turns the output
;; into a list.
;;
(define* (read-query-response port #:optional (triplets '()))
  (let ((line (read-line port)))
    (if (eof-object? line)
        ;; Consing prepends to the list.  So, to get the order
        ;; in which the data was sent, we have to reverse the list.
        (reverse triplets)
        (read-query-response port (cons
                                   (string-split
                                    (string-delete #\" line) #\,) triplets)))))

;;
;; Query the SPARQL endpoint.
;; ----------------------------------------------------------------------------
;; The 'sparql-query' function returns two values that we can capture with
;; the (ice-9 receive) 'receive' function.  We then check whether the response
;; is OK (status code 200), and use the 'read-query-response' function for
;; further processing.
;;
(define triplets
  (receive (header port)
      (sparql-query %example-query #:type "text/csv")
    (if (= (response-code header) 200)
        (read-query-response port)
        '())))

;; Display the nine concepts we have queried.
(format #t "~:{ * ~a ~%~}" triplets)
