(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)] (Math/pow y y)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _  b] v] (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (- y2 y1)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        [px py] point
        ]
    (and (<= x1 px x2) (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and
      (contains-point? outer p1)
      (contains-point? outer p2)
      )))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [book-authors (get book :authors)]
    (assoc book :authors (conj book-authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection))

(defn second-elements [collection]
  (let [get-second-element (fn [col] (get col 1))]
    (map get-second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (let [op (if (contains? a-set elem) disj conj)]
    (op a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq)) (count a-seq)))

(defn old-book->new-book [book]
  (let [old-authors (get book :authors)]
    (assoc book :authors (set old-authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [
        name (get author :name)
        years (let [
                    by (get author :birth-year)
                    dy (get author :death-year)
                    ]
                (if (contains? author :birth-year)
                  (str " (" by " - " dy ")") ""))
        ]
    (str name years)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [bookname (get book :title)
        authors (get book :authors)]
    (str bookname ", written by " (authors->string authors))))

(defn books->string [books]
  (if (empty? books)
   "No books."
    (let [
          bookcount (count books)
          books (map book->string books)
          booklist (apply str (interpose " " (map (fn [x] (str x ".")) books)))
          ]
      (str bookcount " book" (if (= 1 bookcount) "" "s") ". " booklist))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (get author :name))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (get book :authors)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
