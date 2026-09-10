(ns soranoha.ori.fixture
  "Shared inputs for tests that render publishable TEI.

  The rights grant is read from the committed publication policy rather than
  restated here: a test that renders TEI and validates it against the profile
  is asserting the file is publishable, and a published file carries the
  terms the policy states."
  (:require [babashka.fs :as fs]
            [soranoha.core.rights :as rights]))

(def grant
  "The terms a rendered test work publishes under: the policy's release-wide
  grant joined to a standing, which is what a header states. Tests render
  public-domain works unless they build their own terms."
  (delay (rights/work-terms
          (rights/grant-from-bytes (fs/read-all-bytes "data/publication-policy.edn"))
          "public-domain")))
