;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMUX Cyclone Scmobj
;;;Contents: helpers module
;;;Date: May 13, 2020
;;;
;;;Abstract
;;;
;;;	This unit defines the helpers module.
;;;
;;;Copyright (C) 2020 Marco Maggi <mrc.mgg@gmail.com>
;;;All rights reserved.
;;;
;;;This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;;;GNU Lesser General Public License as published  by the Free Software Foundation, either version 3
;;;of the License, or (at your option) any later version.
;;;
;;;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;;;even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;;;Lesser General Public License for more details.
;;;
;;;You should have received a copy of the GNU Lesser General Public License along with this program.
;;;If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(define-library (mmux.cyclone.scmobj.helpers)
  (export assert
	  assertion-violation
	  define-syntax-rule
	  case-define
	  receive-and-return
	  begin0
	  internal-body
	  define-auxiliary-syntaxes)
  (import (scheme base)
	  (scheme write)
	  (scheme cyclone pretty-print))
  (begin


;;;; syntaxes

(define-syntax assert
  (syntax-rules ()
    ((_ ?expr)
     (unless ?expr
       (error "assertion failed" ?expr)))))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (?name . ?args) ?body0 ?body ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ . ?args)
	  (begin ?body0 ?body ...)))))))

(define-syntax case-define
  (syntax-rules ()
    ((_ ?who (?formals ?body0 ?body ...) ...)
     (define ?who
       (case-lambda (?formals ?body0 ?body ...) ...)))
    ))

(define-syntax internal-body
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (let () ?body0 ?body ...))
    ))

(define-syntax define-auxiliary-syntaxes
  (syntax-rules ()
    ((_ ?name0)
     (define-syntax ?name0 (syntax-rules ())))
    ((_ ?name0 ?name1 ?name ...)
     (begin
       (define-auxiliary-syntaxes ?name0)
       (define-auxiliary-syntaxes ?name1 ?name ...)))
    ))


;;;; more syntaxes

(define-syntax receive-and-return
  (syntax-rules ()
    ((_ (?var0 ?var ...) ?expr ?body0 ?body ...)
     (call-with-values
	 (lamba () ?expr)
       (lambda (?var0 ?var ...)
	 (begin ?body0 ?body ...)
	 (values ?var0 ?var ...))))

    ((_ (?var) ?expr ?body0 ?body ...)
     (let ((?var ?expr))
       (begin ?body0 ?body ...)
       ?var))

    ;;FIXME Is this not supported by Cyclone 0.17?  (Marco Maggi; May 12, 2020)
    ;;
    ;; ((_ (?var0 ?var ... . ?vars) ?expr ?body0 ?body ...)
    ;;  (call-with-values
    ;; 	 (lamba () ?expr)
    ;;    (lambda (?var0 ?var ... . ?vars)
    ;; 	 (begin ?body0 ?body ...)
    ;; 	 (apply values ?var0 ?var ... ?vars))))

    ((_ ?vars ?expr ?body0 ?body ...)
     (call-with-values
	 (lamba () ?expr)
       (lambda ?vars
	 (begin ?body0 ?body ...)
	 (apply values ?vars))))
    ))

(define-syntax begin0
  (syntax-rules ()
    ((_ ?form0 ?form1 ?form ...)
     (receive-and-return args
	 ?form0
       (begin ?form1 ?form ...)))
    ))


;;;; miscellaneous

(define (assertion-violation who message . irritants)
  (error message (cons who irritants)))

(define (debug-print . args)
  (pretty-print args (current-error-port)))


;;;; done

#| end of module |# ))

;;; end of file

