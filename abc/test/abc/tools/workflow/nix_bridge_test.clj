(ns abc.tools.workflow.nix-bridge-test
  (:require [abc.tools.files :as files]
            [abc.tools.workflow.nix-bridge :as nix-bridge]
            [abc.tools.workflow.target :as target]
            [clojure.test :refer [deftest is testing]]))

(def flake-lock-value
  (files/read-json "fixtures/v0/flake-lock/sample.flake.lock"))

(def expected-lock-nodes
  [{:node "nixpkgs" :type "github" :owner "NixOS" :repo "nixpkgs"
    :rev "1111111111111111111111111111111111111111"
    :narHash "sha256-AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJKKK="}])

(def fake-runner-out
  "[{\"drvPath\":\"/nix/store/x.drv\",\"outputs\":{\"out\":\"/nix/store/abc-name\"}}]")

(deftest default-runner-captures-nonzero-result-test
  (let [{:keys [exit out err]}
        (nix-bridge/default-runner
         ["sh" "-c" "printf stdout; printf stderr >&2; exit 7"])]
    (is (= 7 exit))
    (is (= "stdout" out))
    (is (= "stderr" err))))

(deftest read-lock-nodes-test
  (testing "known node names resolve to their locked info"
    (is (= expected-lock-nodes
           (nix-bridge/read-lock-nodes flake-lock-value ["nixpkgs"]))))
  (testing "unknown node names are skipped"
    (is (= [] (nix-bridge/read-lock-nodes flake-lock-value ["does-not-exist"])))))

(deftest realize-flake-output-with-fake-runner-test
  (let [captured-args (atom nil)
        fake-runner   (fn [args]
                        (reset! captured-args args)
                        {:exit 0 :out fake-runner-out :err ""})
        result (nix-bridge/realize-flake-output!
                {:flake-output ".#checks.x86_64-linux.contract-surface"
                 :runner fake-runner
                 :flake-lock flake-lock-value
                 :lock-node-names ["nixpkgs"]})]
    (is (= "/nix/store/abc-name" (:store-path result)))
    (is (= ".#checks.x86_64-linux.contract-surface" (:flake-output result)))
    (is (= expected-lock-nodes (:lock-nodes result)))
    (is (= [{:role "out" :store-path "/nix/store/abc-name"}] (:outputs result)))
    (is (vector? (:messages result)))
    (let [args @captured-args]
      (is (= "nix" (first args)))
      (is (some #(= ".#checks.x86_64-linux.contract-surface" %) args))
      (is (some #(= "--json" %) args)))))

(deftest realize-flake-output-non-zero-exit-throws-test
  (let [fake-runner (fn [_args] {:exit 1 :out "" :err "boom"})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (nix-bridge/realize-flake-output!
                  {:flake-output ".#checks.x86_64-linux.contract-surface"
                   :runner fake-runner
                   :flake-lock flake-lock-value
                   :lock-node-names ["nixpkgs"]})))))

(deftest nix-bridge-leaf-integrates-with-evaluator-test
  (let [fake-runner (fn [_args] {:exit 0 :out fake-runner-out :err ""})
        leaf (nix-bridge/nix-bridge-leaf
              {:deps [] :impl-id :realize-check
               :flake-output ".#checks.x86_64-linux.contract-surface"
               :runner fake-runner
               :flake-lock flake-lock-value
               :lock-node-names ["nixpkgs"]})
        g {:check leaf}
        {:keys [value nodes]} (target/eval-target g :check {})]
    (is (= "/nix/store/abc-name" (:store-path value)))
    (is (= ".#checks.x86_64-linux.contract-surface" (:flake-output value)))
    (is (= expected-lock-nodes (:lock-nodes value)))
    (is (= [{:role "out" :store-path "/nix/store/abc-name"}] (:outputs value)))
    (let [check-node (first (filter #(= :check (:key %)) nodes))]
      (is (= "passed" (:status check-node))))))
