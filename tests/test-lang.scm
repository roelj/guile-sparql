;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (tests lang)
  #:use-module (srfi srfi-64)
  #:use-module (sparql lang)
  #:use-module (tests runner))

(test-runner-factory sparql:test-runner)

;; Triples visitor

(define triples->pattern
  (@@ (sparql lang) triples->pattern))

(with-test-group "triples->pattern-tests"
 (lambda _
   (test-equal "\n{\n}\n"
     (triples->pattern '()))

   (test-equal
       "\n{\n  ?s rdf:type ?o .\n}\n"
     (triples->pattern
      '((s "rdf:type" o))))

   (test-equal
       "\n{\n  OPTIONAL \n  {\n    ?s rdf:type ?o .\n  }\n}\n"
     (triples->pattern
      '((optional
         ((s "rdf:type" o))))))

   (test-equal
       "\n{\n  MINUS \n  {\n    ?s rdf:type ?o .\n  ?o rdf:type ?s .\n  }\n}\n"
     (triples->pattern
      '((minus
         ((s "rdf:type" o)
          (o "rdf:type" s))))))))

;; Prefix

(with-test-group "prefix-test"
 (lambda _
   (test-equal "<http://purl.org/dc/elements/1.1/subject>"
     ((prefix "http://purl.org/dc/elements/1.1/") "subject"))))

;; Where

(define where (@@ (sparql lang) where))

(with-test-group "where-tests"
 (lambda _
   (test-equal "WHERE \n{\n}\n"
     (where '()))

   (test-equal "FROM NAMED <http://www.example.org#graph> WHERE \n{\n}\n"
     (where '()
            #:graph "http://www.example.org#graph"
            #:named #t))

   (test-equal "WHERE \n{\n}\nLIMIT 20 \nGROUP BY ?country \n"
     (where '()
            '((limit "20")
              (group by country))))))

;; Select

(with-test-group "select-tests"
 (lambda _
   (test-equal "SELECT DISTINCT \nWHERE \n{\n}\n"
     (select '() '() #:distinct #t))))

;; Construct

(with-test-group "construct-tests"
 (lambda _
   (test-equal "CONSTRUCT WHERE \n{\n}\n"
     (construct '() '()))

   (test-equal "CONSTRUCT \n{\n  ?s rdf:type ?o .\n}\nWHERE \n{\n}\n"
     (construct '((s "rdf:type" o)) '()))))

;; Ask

(with-test-group "ask-tests"
 (lambda _
   (test-equal "ASK \n{\n}\n"
     (ask '()))

   (test-equal "ASK FROM <http://www.example.org#graph> \n{\n}\n"
     (ask '()
          #:graph "http://www.example.org#graph"))))

;; Modify Data

(define modify-data (@@ (sparql lang) modify-data))

(with-test-group "modify-data-tests"
 (lambda _
   (test-equal "DELETE DATA {\n  GRAPH <> {\n  }\n}\n"
     (modify-data '() 'DELETE #:graph ""))

   (test-equal "INSERT DATA {\n  GRAPH <http://www.example.org#graph> {\n    ?s rdf:type ?o .\n  }\n}\n"
     (modify-data '((s "rdf:type" o)) 'INSERT
                  #:graph "http://www.example.org#graph"))

   (test-error #t (modify-data 'DELETE '((s "rdf:type" o))))))

(exit (zero? (test-runner-fail-count (test-runner-current))))
