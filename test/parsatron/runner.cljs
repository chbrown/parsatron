(ns parsatron.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [parsatron.test.core]
            [parsatron.test.languages.bencode]
            [parsatron.test.languages.bf]))

(doo-tests 'parsatron.test.core
           'parsatron.test.languages.bencode
           'parsatron.test.languages.bf)
