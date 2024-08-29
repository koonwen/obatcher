(lang dune 3.9)

(name obatcher)
(generate_opam_files)
(source
 (github koonwen/obatcher))
(authors "Lee Koon Wen" "Kiran Gopinathan" "Arthur Wendling")
(maintainers "Lee Koon Wen")
(license "ISC")
(documentation https://github.com/koonwen/obatcher/)

(package
 (name obatcher)
 (synopsis "A Framework for building Batched Concurrent Data Structures")
 (description "")
 (depends
  (ocaml (>= 5.0.0))
  (picos
   (>= 0.4.0))
  (containers :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (alcotest :with-test)
  (logs :with-test)))
