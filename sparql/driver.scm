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
  #:use-module (ice-9 receive)
  #:use-module (sparql md5)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (sparql-query
            sparql-query-4store
            sparql-query-virtuoso))

;;;
;;; UTILITY FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (random-ascii length)
  "Returns a random string of ASCII characters of length LENGTH."
  (list->string
   (map (lambda _ (integer->char (+ (random 255) 0)))
        (iota length))))

;;;
;;; SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------
(define* (sparql-query query
                       #:key
                       (store-backend 'virtuoso)
                       (uri #f)
                       (host "localhost")
                       (port 8890)
                       (type "text/csv")
                       (namespace "kb")
                       (token #f)
                       (digest-auth #f))
  "Send QUERY to STORE-BACKEND, which can be either '4store or 'virtuoso."
  (cond
   ((eq? store-backend '4store)
    (sparql-query-4store
     query #:uri uri #:host host #:port port #:type type #:token token))
   ((eq? store-backend 'virtuoso)
    (sparql-query-virtuoso
     query #:uri uri #:host host #:port port #:type type #:token token
           #:digest-auth digest-auth))
   ((eq? store-backend 'blazegraph)
    (sparql-query-blazegraph
     query #:uri uri #:host host #:port port #:type type #:namespace namespace))
   (else #f)))

;;;
;;; Virtuoso-specific SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------
(define* (sparql-query-virtuoso query
                                #:key
                                (uri #f)
                                (host "localhost")
                                (port 8890)
                                (type "text/csv")
                                (token #f)
                                (digest-auth #f))
  (let* ((post-uri (cond
                    ((string? token) "/sparql-oauth")
                    ((string? digest-auth) "/sparql-auth")
                    (else "/sparql")))
         (post-url (if uri
                       uri
                       (format #f "http://~a:~a~a" host port post-uri))))
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
                       #f)
                  ,(if (string? digest-auth)
                       (catch #t
                         (lambda _
                           (receive (header port)
                               (http-post post-url)
                             (if (= (response-code header) 401)
                                 (let* ((tokens    (string-split digest-auth #\:))
                                        (username  (car tokens))
                                        (password  (cadr tokens))
                                        (auth      (response-www-authenticate header))
                                        (digest    (assoc-ref auth 'digest))
                                        (realm     (assoc-ref digest 'realm))
                                        (nonce     (assoc-ref digest 'nonce))
                                        (opaque    (assoc-ref digest 'opaque))
                                        (qop       (assoc-ref digest 'qop))
                                        (algorithm (assoc-ref digest 'algorithm)))
                                   (if (and (string= algorithm "MD5")
                                            (string= qop       "auth"))
                                       (let* ((ha1      (md5-from-string
                                                         (string-append
                                                          username ":" realm
                                                          ":" password)))
                                              (ha2      (md5-from-string
                                                         (string-append
                                                          "POST:" post-uri)))
                                              (cnonce   (md5-from-string
                                                         (random-ascii 32)))
                                              (nc       "00000001")
                                              (response (md5-from-string
                                                         (string-append
                                                          ha1 ":" nonce  ":"
                                                          nc  ":" cnonce ":"
                                                          qop ":" ha2)))
                                              (auth-response
                                               (string-append
                                                "Digest username=\"" username
                                                "\", realm=\"" realm
                                                "\", nonce=\"" nonce
                                                "\", uri=\"" post-uri
                                                "\", qop=\"" qop
                                                "\", nc=\"" nc
                                                "\", cnonce=\"" cnonce
                                                "\", response=\"" response
                                                "\", opaque=\"" opaque "\"")))
                                         `(Authorization . ,auth-response))
                                       #f))
                                 #f)))
                         (lambda (key . args) (begin (format #t "Error: ~a: ~a~%" key args) #f)))
                       #f))))))

;;;
;;; 4store-specific SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------

(define* (old-url-encoding input #:optional (index 0) (output ""))
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
                              (uri #f)
                              (host "localhost")
                              (port 8080)
                              (type "text/csv")
                              (token #f))
  (let ((post-url (if uri
                      uri
                      (format #f "http://~a:~a/sparql/" host port))))
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

;;;
;;; BlazeGraph-specific SPARQL-QUERY using a POST request.
;;; ---------------------------------------------------------------------------

(define* (sparql-query-blazegraph query
                                  #:key
                                  (uri #f)
                                  (host "localhost")
                                  (port 9999)
                                  (type "text/csv")
                                  (token #f)
                                  (namespace "kb")
                                  (digest-auth #f))
  (let ((post-url (if uri
                      uri
                      (format #f "http://~a:~a/blazegraph/namespace/~a/sparql"
                              host port namespace))))
    (http-post post-url
               #:body (string-append "query=" (old-url-encoding
                                               (uri-encode query)))
               #:streaming? #t
               #:headers
               `((user-agent   . "GNU Guile")
                 (content-type . (application/x-www-form-urlencoded))
                 (accept       . ((,(string->symbol type))))))))
