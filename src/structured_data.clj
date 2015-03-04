(ns structured-data)

(defn do-a-thing [x]
  (let [seppo (+ x x)]
    (Math/pow seppo seppo)
    ))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x _ z] v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (- x2 x1) (- y2 y1)) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (if (and (<= x1 p1 x2) (<= y1 p2 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
      (if (and (contains-point? outer [x1 y1])
               (contains-point? outer [x2 y2]))
        true false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [authors     (:authors book)
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)
  ))

(defn alive? [author]
  (if (not (:death-year author)) true false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [secs (fn [x] (get x 1))]
    (map secs collection)))

(defn titles [books]
  (let [get-titles (fn [book] (:title book))]
    (map get-titles books)))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true))

(defn old-book->new-book [book]
  (let [author-set (set (:authors book))]
    (assoc book :authors author-set)))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [a-name (:name author) 
        a-birth (if (:birth-year author) (str " (" (:birth-year author) " - ") "")
        a-death (if (:death-year author) (str (:death-year author)) "")
        end-colon (if (clojure.string/blank? a-birth) "" ")")
      ]
    (str a-name a-birth a-death end-colon)))

(defn authors->string [authorz]
  (apply str (interpose ", " (map author->string authorz))))

(defn book->string [book]
  (let [title (:title book)
        authorz (authors->string (:authors book))]
    (str title ", written by " authorz)))

(defn books->string [books]
  (let [book-count (count books)
        multiple (if (> book-count 1) (str "s") (str ""))
        book-data (map book->string books)
        dot (str ".")]
    (if (= book-count 0)
      (str "No books.")
      (str  book-count
            " book"
            multiple
            dot
            " "
            (apply str (interpose ". " book-data))
            dot
            ))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
    (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%