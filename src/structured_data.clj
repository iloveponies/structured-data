(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)
  )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y z] v ]
    (+ x z )
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1 )
  )
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1 )
  )
)

(defn square? [rectangle]
  (= (height rectangle) (width rectangle))  
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
)

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (and (<= x1 xp x2) (<= y1 yp y2))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
  )
)

(defn title-length [book]
  (count (:title book))
)

(defn author-count [book]
  (count (:authors book))
)

(defn multiple-authors? [book]
  (> (author-count book) 1)
)

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))
)

(defn alive? [author]
    (not (contains? author :death-year))
)

(defn element-lengths [collection]
  (map count collection)
)

(defn second-elements [collection]
  (let [get-second (fn [vec] (get vec 1))]
    (map get-second collection)
    )
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
)

(defn contains-duplicates? [a-seq]
  ( > (count a-seq) (count (set a-seq)))
)

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors)))
)

(defn has-author? [book author]
  (contains? (get book :authors) author)  
)

;(defn all-author-names [books]
;  (let [author-names
;         (fn [book] (map :name (:authors book)))]
;    (set (apply concat (map author-names books))))
;)

(defn authors [books]
  (let [get-author
         (fn [book] (:authors book))]
    (set (apply concat (map get-author books))))
)

(defn all-author-names [books]
  (let [get-name (fn [author] (:name author))]
  (set (map get-name (authors books))
  ))
)

(defn author->string [author]
  (if (and (contains? author :birth-year) (alive? author))
    (str (:name author) " (" (:birth-year author) " - )") 
    (
      if (and (contains? author :birth-year) (not (alive? author)))
      (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")")
      (str (:name author))
    )
  )
)

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string (seq authors))))
)

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)))
)

(defn books->string [books]
  (if (empty? books)
    (str "No books.")
    (if (= (count books) 1)
      (str "1 book. " (apply str (interpose ". " (map book->string (seq books)))) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string (seq books)))) ".")
    )
  )
)

(defn books-by-author [author books] 
  (let [check (fn [bk] (has-author? bk author))]
    (filter check books)
  )
)

(defn author-by-name [name authors]
  (let [getname (fn [a] (:name a))
        checkname (fn [a] (= name (:name a)))]
;    (if (contains? (set (map getname authors)) name)
      (first (filter checkname (seq (set authors))))
;      ()
    )
;  )
)

(defn living-authors [authors]
  (filter alive? (seq (set authors)))
)

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
)

; Test data...
; (def china {:name "China Miéville", :birth-year 1972})
; (def octavia {:name "Octavia E. Butler"
;               :birth-year 1947
;               :death-year 2006})
; (def friedman {:name "Daniel Friedman" :birth-year 1944})
; (def felleisen {:name "Matthias Felleisen"})

; (def cities {:title "The City and the City" :authors #{china}})
; (def wild-seed {:title "Wild Seed", :authors #{octavia}})
; (def embassytown {:title "Embassytown", :authors #{china}})
; (def little-schemer {:title "The Little Schemer"
;                      :authors #{friedman, felleisen}})

; (def books [cities, wild-seed, embassytown, little-schemer])
; (def authors #{china, felleisen, octavia, friedman})
; (def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
; (def christopher {:name "Christopher Tolkien" :birth-year 1924})
; (def kay {:name "Guy Gavriel Kay" :birth-year 1954})

; (def silmarillion {:title "Silmarillion"
;                    :authors #{jrrtolkien, christopher, kay}})
; (def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
; (def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

; (def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})