(defsystem Champ-Lite
  :description "A lightweight implementation of persistent functional maps and
iteration-safe mutable tables using Michael Steindorfer's CHAMP data structure."
  :author "Scott L. Burson"
  :version "1.1.0"
  :license "Unlicense"  ; i.e., public domain
  :serial t
  :components
  ((:module "src" :serial t
    :components
    ((:file "defs")
     (:file "macros")
     (:file "champ")
     (:file "tests")))))
