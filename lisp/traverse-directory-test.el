;;; traverse-directory-test - Functional tests of traverse-directory.

;; Copyright (C) 2020 TakesxiSximada

;; This code is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This code distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)

(ert-deftest traverse-directory-test-super-root-p-is-true ()
  (should
   (equal t (traverse-directory-super-root-p "/"))))

(ert-deftest traverse-directory-test-super-root-p-is-nil ()
  (should
   (equal nil (traverse-directory-super-root-p "/a"))))

(ert-deftest traverse-directory-exist-readme-md-p-is-true ()
  (should
   (equal nil (traverse-directory-exist-readme-md-p "/a"))))

(provide 'traverse-directory-test)
;;; osx-keychain.el ends here
