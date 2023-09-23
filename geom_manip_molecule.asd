;;;; geom_manip_molecule.asd

(asdf:defsystem #:geom_manip_molecule
  :description "Describe geom_manip_molecule here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:numcl)
  :components ((:file "package")
               (:file "geom_manip_molecule")))
