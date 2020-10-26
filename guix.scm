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

;; GNU Guix development package. This has been copied from GNU Mes.
;; To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;

(use-modules (ice-9 popen)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages texinfo)
             (guix gexp)
             (guix git-download)
             (guix packages)
             (guix utils)) ;; for substitute-keyword-arguments

(define %source-dir (getcwd))

(define-public guile-sparql.git
  (let ((version "0.0.8")
        (revision "0")
        (commit (symbol->string
                 (read (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ)))))
    (package
      (inherit guile-sparql)
      (name "guile-sparql.git")
      (version (git-version version revision commit))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo)
         ,@(package-native-inputs guile-sparql))))))

guile-sparql.git

