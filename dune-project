(lang dune 3.9)

(name obatcher)
(generate_opam_files)
(source
 (github koonwen/obatcher))
(authors "Lee Koon Wen" "Kiran Gopinathan" "Arthur Wendling")
(maintainers "koonwen@gmail.com")
(license "ISC")
(documentation https://koonwen.github.io/obatcher)

(package
 (name obatcher)
 (synopsis "A Framework for building Batched Concurrent Data Structures")
 (description "")
 (depends
  (ocaml (>= 5.0.0))
  (picos (= 0.5.0))
  picos_std
  picos_aux
  picos_mux
  (containers :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (alcotest :with-test)
  (logs :with-test)))
