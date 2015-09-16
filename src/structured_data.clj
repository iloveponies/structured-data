(ns structured-data)

;(def china {:name "China Miéville", :birth-year 1972})
;(def octavia {:name "Octavia E. Butler"
;                            :birth-year 1947
;                            :death-year 2006})
;              (def friedman {:name "Daniel Friedman" :birth-year 1944})
;              (def felleisen {:name "Matthias Felleisen"})
;
;              (def cities {:title "The City and the City" :authors [china]})
;              (def wild-seed {:title "Wild Seed", :authors [octavia]})
;              (def embassytown {:title "Embassytown", :authors [china]})
;              (def little-schemer {:title "The Little Schemer"
;                                                        :authors [friedman, felleisen]})
;
;              (def books [cities, wild-seed, embassytown, little-schemer])

(defn do-a-thing [x]
  (let [xx (+ x x)]
      (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
  (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1] [x2]] rectangle] 
      (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] 
      (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [ [[x1 y1] [x2 y2]] rectangle 
         [xp yp] point] 
    (and (<= x1 xp x2) (<= y1 yp y2))
  ))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1 ) (contains-point? outer p2))
  ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (not(= (author-count book) 1)))

(defn add-author [book new-author]
  (let [authors (conj (get book :authors) new-author)]
    (assoc book :authors authors)
    ))

(defn alive? [author]
 (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection))

(defn titles [books]
  (map :title books ))

(defn monotonic? [a-seq]
  (let [mono-dir (fn [dir a-seq] (apply dir (concat a-seq)))]
    (or (mono-dir <= a-seq) (mono-dir >= a-seq))))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
      (assoc book :authors (set authors))
      ))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map (fn [book] (get book :authors)) books )))

(defn all-author-names [books]
  (set (map (fn [author] (get author :name)) (authors books))))

(defn author->string [author]
  (let [aname (str (get author :name))
        lifespan (str "(" (str (get author :birth-year)) " - " (str (get author :death-year)) ")")
    ]
    (if (= lifespan "( - )") aname (str aname " " lifespan))
    ))
                      

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (get book :title) ", written by " (authors->string (get book :authors))))

(defn books->string [books]
  (let [cnt (count books)
        cnt-word (if (= cnt 1) "book." "books.")]
    (if (= cnt 0) (str "No " cnt-word) 
      (str cnt " " cnt-word " " (apply str (interpose ". " (map book->string books))) ".")
    )
  )
)

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (get author :name) name)) authors))
)

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
