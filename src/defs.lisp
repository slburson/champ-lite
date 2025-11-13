(in-package :cl-user)

(defpackage :com.sympoiesis.champ
  (:nicknames :champ)
  (:use :cl)
  (:export #:make-champ-map #:champ-map-size #:champ-map-with #:champ-map-less
	   #:champ-map-lookup #:do-map #:make-map-iterator))

