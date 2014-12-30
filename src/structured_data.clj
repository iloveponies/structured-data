(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[left bottom] [right top]] rectangle]
    (- right left)))

(defn height [rectangle]
  (let [[[left bottom] [right top]] rectangle]
    (- top bottom)))

(defn square? [rectangle]
  (= (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[left bottom] [right top]] rectangle
        [x y] point]
    (and (<= left x right)
         (<= bottom y top))))

(defn contains-rectangle? [outer inner]
  (let [[leftbottom righttop] inner]
    (and (contains-point? outer leftbottom)
         (contains-point? outer righttop))))

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
        new-authors (conj authors new-author)]
    (assoc book :authors new-authors)))


(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [coll] (get coll 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))


(defn author->string [author]
  (let [name (:name author)
        years (str "(" (:birth-year author) " - " (:death-year author) ")" )]
    (if (contains? author :birth-year)
      (str name " " years)
      name)))

(defn authors->string [authors]
  (let [author-strings
        (interpose ", " (map author->string authors))]
    (apply str author-strings)))

(defn book->string [book]
  (let [title (:title book)
        authors-string (authors->string (:authors book))]
  (str title ", written by " authors-string)))

(defn books->string [books]
  (let [num-books (count books)
        books-string-seq (map book->string books)
        books-string (apply str (interpose ". " books-string-seq))]
    (cond
     (= num-books 1) (str "1 book. " books-string ".")
     (> num-books 0) (str num-books " books. " books-string ".")
     :else "No books.")))

(defn books-by-author [author books]
  (let [wrote? (fn [book] (has-author? book author))]
    (filter wrote? books)))

(defn author-by-name [name authors]
  (let [author-named (fn [author] (= name (:name author)))]
    (first (filter author-named authors))))

(defn living-authors [authors]
  (filter alive? authors))


(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (not (empty? (living-authors authors)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
