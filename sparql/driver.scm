;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (sparql driver)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:export (sparql-query
            sparql-query-4store
            sparql-query-virtuoso))

;;;
;;; SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------
(define* (sparql-query query
                       #:key
                       (store-backend 'virtuoso)
                       (host "localhost")
                       (port 8890)
                       (type "text/csv")
                       (token #f))
  "Send QUERY to STORE-BACKEND, which can be either '4store or 'virtuoso."
  (cond
   ((eq? store-backend '4store)
    (sparql-query-4store
     query #:host host #:port port #:type type #:token token))
   ((eq? store-backend 'virtuoso)
    (sparql-query-virtuoso
     query #:host host #:port port #:type type #:token token))
   (else #f)))

;;;
;;; Virtuoso-specific SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------
(define* (sparql-query-virtuoso query
                                #:key
                                (host "localhost")
                                (port 8890)
                                (type "text/csv")
                                (token #f))
  (let ((post-url (if (string? token)
                      (format #f "http://~a:~a/sparql-oauth/" host port)
                      (format #f "http://~a:~a/sparql" host port))))
    (http-post post-url
               #:body query
               #:streaming? #t
               #:headers
               (delete #f
                `((user-agent . "GNU Guile")
                  (content-type . (application/sparql-update))
                  (accept . ((,(string->symbol type))))
                  ;; "Bearer" authorization isn't implemented in (web client),
                  ;; so we work around that by capitalizing the header key.
                  ,(if (string? token)
                       `(Authorization . ,(string-append "Bearer " token))
                       #f))))))

;;;
;;; 4store-specific SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------

(define* (old-url-encoding input #:optional (index 0) (output ""))
  "Return S1 with WORD replaced by REPLACEMENT."
  (if (< (string-length input) 3)
      (string-append output input)
      (let ((triple (substring/read-only input index (+ index 3))))
        (if (string= triple "%20")
            (old-url-encoding (string-drop input 3)
                              0
                              (string-append output "+"))
            (old-url-encoding (string-drop input 1)
                              0
                              (string-append output
                                             (string
                                              (string-ref input index))))))))

(define* (sparql-query-4store query
                              #:key
                              (host "localhost")
                              (port 8080)
                              (type "text/csv")
                              (token #f))
  (let ((post-url (format #f "http://~a:~a/sparql/" host port)))
    (http-post post-url
               #:body (string-append "query=" (old-url-encoding
                                               (uri-encode query))
                                     ;; 4store includes comments in its output
                                     ;; when the soft limit has been hit.
                                     ;; This breaks the validity of the CSV
                                     ;; output, so we work around this by
                                     ;; setting a very high soft limit.
                                     "&soft-limit=9000000"
                                     ;; Because the web interface only
                                     ;; uses the text/csv, we hardcoded 'csv'
                                     ;; here.
                                     "&output=csv"
                                     ;; The 'apikey' parameter is specific to
                                     ;; 4store.
                                     (if (string? token)
                                         (string-append "&apikey=" token)
                                         ""))
               #:streaming? #t
               #:headers
               (delete #f
                `((user-agent . "GNU Guile")
                  (content-type . (application/x-www-form-urlencoded))
                  (accept . ((,(string->symbol type)))))))))
