(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (let [fst (get v 0)
        thd (get v 2)] 
    (+ fst thd))) 


(defn cutify [v]
  (let [lv "<3"]
    (conj v lv)))


(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1][x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1][x2 y2]] rectangle [p1 p2] point] 
    (and 
      (and (>= p1 x1) (<= p1 x2)) 
      (and (>= p2 y1) (<= p2 y2)))
    ))

(defn contains-rectangle? [outer inner]
  (let [[[v1 w1][v2 w2]] outer [[x1 y1][x2 y2]] inner] 
    (and
      (and 
        (and (>= x1 v1) (>= y1 w1)) 
        (and (<= x1 v2) (<= y1 w2)))
      (and 
        (and (<= x2 v2) (<= y2 w2)) 
        (and (>= x2 v1) (>= y2 w1))))
    ))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [current (get book :authors)]
    (let [new-authors (conj current new-author)]
    (assoc book :authors new-authors))))

(defn alive? [author]
  (let [death (:death-year author)]
    (nil? death)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-two (fn [vec] (get vec 1))]
       (map get-two collection)))

(defn titles [books]
  (let [get-title (fn [book] (get book :title))]
    (map get-title books)))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (let [con (contains? a-set elem)]
    (if con (disj a-set elem)(conj a-set elem))))

(defn contains-duplicates? [a-seq]
  (let [seq-count (count a-seq) set-count (count (set a-seq))]
    (not (= seq-count set-count))))

(defn old-book->new-book [book]
  (let [current-author (:authors book)]
    (let [current-set (set current-author)]
    (assoc book :authors current-set))))

(defn has-author? [book author]
  (let [book-author (:authors book)]
    (contains? book-author author)))


(defn authors [books]
  (let [get-author (fn [book] (:authors book))
        gen (fn [books](map get-author books))
        ll (gen books)]
    (apply clojure.set/union ll)))


(defn all-author-names [books]
  (let [get-name (fn [auth] (:name auth))
        authors-list (authors books)]
    (set (map get-name authors-list))))

(defn author->string [author]
  (let [author-name (:name author) 
        birth (:birth-year author)
        death (:death-year author)]
    (if (nil? birth) 
      (str author-name) 
      (str author-name " (" birth " - " death ")"))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book) 
        author-detail (authors->string (:authors book))]
    (str book-title ", written by " author-detail)))

(defn books->string [books]
  (let [book-detail (map book->string books)
        book-count (count books)
        book-count-str (if (= book-count 1) (str book-count " book")(str book-count " books"))]
    (if (> book-count 0) 
      (str book-count-str ". " (apply str (interpose ", " book-detail)) ".")
      "No books." )))


(defn books-by-author [author books]
  (filter (fn[x] (has-author? x  author )) books))


(defn author-by-name [name authors]
  (let [result (filter (fn[x] (= (:name x) name)) authors)]
    (if (empty? result) nil (first result))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
