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
            display-query-results-of))

(define* (display-query-results port #:optional (is-header? #t))
  "Writes CSV data from PORT to the standard output port."
  (let* ((line   (read-line port)))
    (if (eof-object? line)
        #t
        (let ((tokens (map (lambda (item) (string-trim-both item #\"))
                           (string-split line #\,))))
          (format #t "~{~a~/~}~%" tokens)
          (when is-header?
            (do ((index 1 (1+ index)))
                ;; XXX: The length calculation over-estimates because it
                ;; doesn't know the actual length of the tab.
                ((> index (+ (* (1- (length tokens)) 8)
                             (apply + (map string-length tokens)))))
              (display "-"))
            (newline))
          (display-query-results port #f)))))

(define-syntax-rule
  (display-query-results-of query)
  (receive (header port)
      query
    (if (= (response-code header) 200)
        (show-query-results port)
        (format #t "Error: HTTP response code was ~a~%"
                (response-code header)))))
