(ns kaiser.core
  (:require [clojure.browser.repl :as repl]
            reagent.core
            kaiser.ai.bid
            kaiser.game.interface
            [kaiser.ui.root :as root]))

;; (repl/connect "http://localhost:9000/repl")

(enable-console-print!)

(println "Hello world!")

(reagent.core/render-component [root/test-div]
                               (.getElementById js/document "reagent"))
