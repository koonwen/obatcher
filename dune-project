(lang dune 3.0)

(name obatcher)

(generate_opam_files)

(source
 (github koonwen/obatcher))

(authors "Lee Koon Wen" "Kiran Gopinathan")

(maintainers "Lee Koon Wen")

(package
 (name obatcher)
 (synopsis "A Framework for building Batched Concurrent Data Structures")
 (description "")
 (depends
  (ocaml
   (>= 5.0.0))
  domainslib
  kcas_data))
