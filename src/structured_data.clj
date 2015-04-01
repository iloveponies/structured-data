(ns structured-data)

(defn do-a-thing [x]
  (let [e (+ x x)]
    (Math/pow e e)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x _ y]]
  (+ x y))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 _] [x2 _]]]
  (- x2 x1))

(defn height [[[_ y1] [_ y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [[[x1 y1] [x2 y2]] [px py]]
  (and (<= x1 px x2) (<= y1 py y2)))

(defn contains-rectangle? [outer [p1 p2]]
  (and (contains-point? outer p1) (contains-point? outer p2)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [original-authors (:authors book)
        new-authors (conj original-authors new-author)]
    (assoc book :authors new-authors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getter (fn [c] (get c 1))]
    (map getter collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply <= (reverse a-seq))))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [original-authors (:authors book)
        new-authors (set original-authors)]
    (assoc book :authors new-authors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (if (:birth-year author)
                (str " (" (:birth-year author) " - " (:death-year author) ")"))]
    (str name years)))

(defn authors->string [authors]
  (let [author-strs (map author->string authors)]
    (apply str (interpose ", " author-strs))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [total-books (count books)
        total (cond
                (= total-books 0) "No books."
                (= total-books 1) "1 book. "
                (> total-books 1) (str total-books " books. "))
        book-strs (apply str (interpose ". " (map book->string books)))]
    (if (= 0 total-books)
      total
      (str total book-strs "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

(let [china {:name "China Miéville", :birth-year 1972}
      octavia {:name "Octavia E. Butler"
               :birth-year 1947
               :death-year 2006}
      friedman {:name "Daniel Friedman" :birth-year 1944}
      felleisen {:name "Matthias Felleisen"}
      jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973}
      christopher {:name "Christopher Tolkien" :birth-year 1924}
      kay {:name "Guy Gavriel Kay" :birth-year 1954}
      dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982}
      zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995}

      authors-set #{china, felleisen, octavia, friedman}

      cities {:title "The City and the City" :authors #{china}}
      wild-seed {:title "Wild Seed", :authors #{octavia}}
      embassytown {:title "Embassytown", :authors #{china}}
      little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}}
      silmarillion {:title "Silmarillion"
                    :authors #{jrrtolkien, christopher, kay}}
      deus-irae {:title "Deus Irae", :authors #{dick, zelazny}}

      books [cities, wild-seed, embassytown, little-schemer]]
  (has-a-living-author? wild-seed)
  )


; %________%
