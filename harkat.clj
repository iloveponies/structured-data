(defn element-lengths [collection]
  (map count collection)
) 

(element-lengths ["foo" "bar" "" "quux"])
(element-lengths ["x" [:a :b :c] {:y 42}])

(defn second-elements [collection]
  (let [ second-element (fn [elem] (get elem 1)) ]
    (map second-element collection))
)

(second-elements [[1 2] [2 3] [3 4]]) ;=> (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]])
;=> (2 nil "s")

(defn titles [books]
  (map :title books)
)

(titles [cities]) ;=> ("The City and the City" )
(titles books)
;=> ("The City and the City" "Wild Seed"
;    "Embassytown" "The Little Schemer")

(defn stars [num]
  (let [tmp_stars (repeat num "*") ]
    (apply str tmp_stars))
)

(defn monotonic? [seq] 
  (if (or (apply <= seq) (apply >= seq))
       true
       false )
)

(monotonic? [1 2 3])     ;=> true
(monotonic? [0 1 10 11]) ;=> true
(monotonic? [3 2 0 -3])  ;=> true
(monotonic? [3 2 2])     ;=> true    Not strictly monotonic
(monotonic? [1 2 1 0])   ;=> false

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
) 

(toggle #{:a :b :c} :d) ;=> #{:a :c :b :d}
(toggle #{:a :b :c} :a) ;=> #{:c :b}


(defn contains-duplicates? [sequence]
  (if (= (count(set sequence))(count sequence))
    false
    true )
)

(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true

(defn all-author-names [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
    (set (apply concat (map author-names books)))))

; exercise 22
(defn old-book->new-book [book]
  (let [ uniq-auths (set (:authors book)) ]
    (assoc book :authors uniq-auths) )
)


(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}
(old-book->new-book {:title "Wild Seed", :authors [octavia]})


