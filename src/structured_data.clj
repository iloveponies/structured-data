(ns structured-data)
(defn do-a-thing [x]
  (let [value (+ x x)]
    (Math/pow value value)
    ))      ; this works

(defn spiff [v]
  (let [firstElement (get v 0)
        thirdElement (get v 2)]
    (+ firstElement thirdElement)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [ [ [x1 y1] [x2 y2] ] ]
  (- x2 x1)
  )

(defn height [rectangle]
  (let [ [ [x1 y1] [x2 y2] ] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true 
      false 
      ))

(defn area [rectangle]
  (let [width (width rectangle)
        height (height rectangle)]
    (* width height))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (if (and (<= x1 x x2) (<= y1 y y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[lowerLeftPoint upperRightPoint] inner]
    (and (contains-point? 
          outer 
          lowerLeftPoint)
         (contains-point? 
          outer 
          upperRightPoint))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) 
    true 
    false))

(defn add-author [book new-author]
  (let [authors (:authors book)
        new (conj authors new-author)]
    (assoc book :authors new) ; replace old authors list value with a new one
    ))

(defn alive? [author]
  (if (contains? author :death-year) 
    false
    true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [x] (first (rest x)))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
   (apply <= a-seq) true ;(1 <= 2 <= 3)
   (apply >= a-seq) true ;(3 >= 2 >= 1)
   :else false))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [seq-count (count a-seq)
        set (set a-seq)
        set-count (count set)]
    (if (> seq-count set-count)
      true
      false)))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(def furaido {:name "Daniel Friedman" :birth-year 1944})
(def chin {:name "China Miéville", :birth-year 1972})
(def octa {:name "Octavia E. Butler"
           :birth-year 1947
           :death-year 2006})
(def felle {:name "Matthias Felleisen"})

(def embassy {:title "Embassytown", :authors #{chin}})
(def city {:title "The City and the City" :authors #{chin}})
(def wild {:title "Wild Seed", :authors #{octa}})
(def schemer {:title "The Little Schemer"
              :authors #{furaido, felle}})
(def sakka #{chin, felle, octa, furaido})

(defn author->string [author]
  (let [name (:name author)
        birth-year (:birth-year author)
        death-year (:death-year author)
        name-years (str name " (" birth-year " - " death-year ")")]
    (cond 
     (contains? author :death-year) name-years
     (contains? author :birth-year) name-years
     :else name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (authors->string (:authors book))]
    (str title ", written by " authors )))

(defn books->string [books]
  (let [count (count books)
        book-list (interpose ". " (map book->string books)) ; add full stops between subsequent books
        books-string (apply str book-list)] ; convert book-list to string
      (cond 
       (= count 0) "No books."
       (= count 1) (str count " book. " books-string ".")
       :else (str count " books. " books-string "."))))

(defn books-by-author [author books]
  (let [contains-author? (fn [book] (has-author? book author))]
    (filter contains-author? books)))

(defn author-by-name [name authors]
  (let [has-name? (fn [x] (= (:name x) name))]
    (first (filter has-name? authors))))

(defn living-authors [authors]
  (filter alive? authors))

(def tolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def chris {:name "Christopher Tolkien" :birth-year 1924})
(def k {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmari {:title "Silmarillion"
                   :authors #{tolkien, chris, k}})

(def d {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zela {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus {:title "Deus Irae", :authors #{d, zela}})

(defn has-a-living-author? [book]
  (let [death-years (map :death-year (:authors book))
        living-authors (filter not death-years)] ; remove dead people
    (if (empty? living-authors) false true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

                                        ; %________%
