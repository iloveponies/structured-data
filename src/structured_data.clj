(ns structured-data)

(defn do-a-thing [x]
  (let [double (+ x x)]
    (Math/pow double double)))

(defn spiff [v]
  (+ (first v) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[first _ third]]
  (+ first third))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[left _] [right _]]]
  (- right left))

(defn height [[[_ bottom] [_ top]]]
  (- top bottom))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[left bottom] [right top]] rectangle
        [x y] point]
    (and (<= left x right) (<= bottom y top))))

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        more-authors (conj authors new-author)
        updated (assoc book :authors more-authors)]
    updated))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map #(count %) collection))

(defn second-elements [collection]
  (map #(get % 1) collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [orig-authors (:authors book)
        authors-set (set orig-authors)]
    (assoc book :authors authors-set)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map #(:authors %) books)))

(defn all-author-names [books]
  (set (map #(:name %) (authors books))))

(defn author->string [author]
  (let [{:keys [name birth-year death-year]} author
        year-string (str "(" birth-year " - " death-year ")")]
    (if (not (nil? birth-year))
      (str name " " year-string)
      name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map #(author->string %) authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [book-list (apply str (interpose ". " (map #(book->string %) books)))
        book-count (count books)
        book-count-string (str (if (= book-count 0) "No" book-count) " book" (when (not (= book-count 1)) "s") (when (> book-count 0) ". "))
        ]
    (str book-count-string book-list ".")))

(defn books-by-author [author books]
  (filter #(has-author? % author) books))

(defn author-by-name [name authors]
  (first (filter #(= name (:name %)) authors)))

(defn living-authors [authors]
  (filter #(alive? %) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
