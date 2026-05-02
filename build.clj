(ns build
  (:require [clojure.java.io :as io]
            [clojure.tools.build.api :as b]
            [clojure.tools.build.util.file :as file]))

(def basis (b/create-basis {:project "deps.edn"}))

(def doc-code
  '(doseq [ns-sym '[erdos.algo.leapfrog.treejoin]]
      (require ns-sym)
      (println (str "## Namespace " ns-sym "\n"))
      (println)
      (when-let [doc (:doc (meta (the-ns ns-sym)))]
        (println doc)
        (println))
      (doseq [[v-name v] (sort (ns-publics ns-sym))]
        (let [{:keys [doc arglists macro]} (meta v)]
           (println (str "### `" v-name "`" (when macro " *(macro)*") "\n"))
           (when arglists
              (println "**Arities:**")
              (doseq [args arglists]
                 (println (str "- `" args "`")))
              (println))
           (when doc (println doc) (println))))))

(defn doc [opts]
  (let [expr (pr-str doc-code)]
    (-> {:basis basis
         :main "clojure.main"
         :main-args ["-e" expr]}
        (b/java-command)
        (b/process)))
  opts)
