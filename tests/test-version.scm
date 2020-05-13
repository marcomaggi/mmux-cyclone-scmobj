;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMUX Cyclone Scmobj
;;;Contents: test program for version functions
;;;Date: May 12, 2020
;;;
;;;Abstract
;;;
;;;	This program tests version functions.
;;;
;;;Copyright (C) 2020 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(import (scheme base)
	(scheme write)
	(scheme cyclone pretty-print)
	(srfi 28)
	(mmux.cyclone.scmobj.version))


;;;; helpers

(define check-failed-tests #f)

(define (check-report)
  (if check-failed-tests
      (exit 1)
      (exit 0)))

(define-syntax check
  (syntax-rules (=>)
    ((_ ?expr => ?expected-result)
     (let ((expected-result	?expected-result)
	   (result		?expr))
       (unless (equal? result expected-result)
	 (set! check-railed-tests #t)
	 (display "Failed test:\n")
	 (pretty-print (quote ?expr))
	 (newline)
	 (display (format "Expected ~a, got ~a.\n" expected-result result)))))
    ))

(define-syntax check-for-true
  (syntax-rules ()
    ((_ ?expr)
     (check ?expr => #t))
    ))


;;;; tests

(check-for-true		(number? (mmux-cyclone-scmobj-package-major-version)))
(check-for-true		(number? (mmux-cyclone-scmobj-package-minor-version)))
(check-for-true		(number? (mmux-cyclone-scmobj-package-patch-level)))
(check-for-true		(string? (mmux-cyclone-scmobj-package-prerelease-tag)))
(check-for-true		(string? (mmux-cyclone-scmobj-package-build-metadata)))
(check-for-true		(string? (mmux-cyclone-scmobj-package-version)))
(check-for-true		(string? (mmux-cyclone-scmobj-package-semantic-version)))


;;;; done

(check-report)

;;; end of file
