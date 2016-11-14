;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})

(defn title-length [book]
  (count (get book :title))
)


(title-length cities)   ;=> 21
(title-length wild-seed)      ;=> 9
(title-length little-schemer) ;=> 18

(defn author-count [book]
  (count (get book :authors))
  )

(author-count cities)         ;=> 1
(author-count wild-seed)      ;=> 1
(author-count little-schemer) ;=> 2


(defn multiple-authors? [book]
  (if (> (count (get book :authors)) 1)
    true
    false
  )
)


(multiple-authors? cities)         ;=> false
(multiple-authors? wild-seed)      ;=> false
(multiple-authors? little-schemer) ;=> true


(defn add-author [book new-author]
  (let [ authors (get book :authors)
         new-authors (conj authors new-author) ]
    (assoc book :authors new-authors))
  )

(add-author little-schemer {:name "Gerald J. Sussman"})
;=> {:title "The Little Schemer"
;    :authors [{:birth-year 1944, :name "Daniel Friedman"}
;              {:name "Matthias Felleisen"}
;              {:name "Gerald J. Sussman"}]}
(add-author {:authors [{:name "Juhana"}]} {:name "Jani"})
;=> {:authors [{:name "Juhana"} {:name "Jani"}]}


(defn alive? [author]
  (if (nil? (get author :death-year))
    true
    false
    )
  )

(alive? china)   ;=> true
(alive? octavia) ;=> false


(get china :birteh-year)
















