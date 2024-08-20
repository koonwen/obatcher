(lang dune 3.0)

(name obatcher)
(generate_opam_files)
(source
 (github koonwen/obatcher))
(authors "Lee Koon Wen" "Kiran Gopinathan" "Arthur Wendling")
(maintainers "Lee Koon Wen")
(documentation https://github.com/koonwen/obatcher/)

(package
 (name obatcher)
 (synopsis "A Framework for building Batched Concurrent Data Structures")
 (description "")
 (depends
  (ocaml (>= 5.0.0))
  (picos
   (>= 0.4.0))
  (alcotest :with-test)))
