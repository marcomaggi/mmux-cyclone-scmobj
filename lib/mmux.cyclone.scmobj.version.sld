;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMUX Cyclone Scmobj
;;;Contents: version functions
;;;Date: May 13, 2020
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
;;;
;;;Copyright (C) 2020 Marco Maggi <mrc.mgg@gmail.com>
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

(define-library (mmux.cyclone.scmobj.version)
  (export mmux-cyclone-scmobj-package-major-version
	  mmux-cyclone-scmobj-package-minor-version
	  mmux-cyclone-scmobj-package-patch-level
	  mmux-cyclone-scmobj-package-prerelease-tag
	  mmux-cyclone-scmobj-package-build-metadata
	  mmux-cyclone-scmobj-package-version
	  mmux-cyclone-scmobj-package-semantic-version)
  (import (scheme base))
  (include "build-lib/config.scm")
  (begin


;;;; version functions

(define (mmux-cyclone-scmobj-package-major-version)	MMUX_PKG_MAJOR_VERSION)
(define (mmux-cyclone-scmobj-package-minor-version)	MMUX_PKG_MINOR_VERSION)
(define (mmux-cyclone-scmobj-package-patch-level)	MMUX_PKG_PATCH_LEVEL)
(define (mmux-cyclone-scmobj-package-prerelease-tag)	MMUX_PKG_PRERELEASE_TAG)
(define (mmux-cyclone-scmobj-package-build-metadata)	MMUX_PKG_BUILD_METADATA)
(define (mmux-cyclone-scmobj-package-version)		MMUX_PKG_VERSION)
(define (mmux-cyclone-scmobj-package-semantic-version)	MMUX_PKG_SEMANTIC_VERSION)


;;;; done

#| end of library |# ))

;;; end of file
;; Local Variables:
;; mode: scheme
;; End:
