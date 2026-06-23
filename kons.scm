(package
  (name (conduit))
  (version "0.1.1")
  (license "BSD-3-Clause")
  (description "Process creation and communication through ports")
  (keywords)
  (owner "playx")
  (authors "Adel Prokurov <adel.prokurov@gmail.com>")
  (site "https://github.com/playx18/conduit")
  (repo "https://github.com/playx18/conduit.git")
  (docs "https://github.com/playx18/conduit/blob/main/README.md")
  (readme "https://github.com/playx18/conduit/blob/main/README.md")
  (dialects r7rs)
  (source-path "src")
  (main "src/main.scm")
  (tests "tests/main.scm")
  (benches "benches/main.scm")
  (examples "examples/main.scm"))

(dependencies
  (system (scheme base) (scheme write)))

(dev-dependencies)

(overrides)
