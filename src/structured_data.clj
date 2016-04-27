(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bottom-x bottom-y] [top-x top-y]] rectangle]
    (- top-x bottom-x)
    ))

(defn height [rectangle]
  (let [[[bottom-x bottom-y] [top-x top-y]] rectangle]
    (- top-y bottom-y)
    ))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[bottom-x bottom-y] [top-x top-y]] rectangle
        [point-x point-y] point]
    (and (<= bottom-x point-x top-x) (<= bottom-y point-y top-y))
  )
)

(defn contains-rectangle? [outer inner]
  (let [[[i-btm-x i-btm-y] [i-top-x i-top-y]] inner
        bottom-point [i-btm-x i-btm-y]
        top-point [i-top-x i-top-y]]
    (and (contains-point? outer bottom-point) (contains-point? outer top-point))
  )
)

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (count (:authors book))))

(defn add-author [book new-author]
    (assoc book :authors (conj (:authors book) new-author))) ; replace the whole authors-map with a new one

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [countItems (fn [x] (count x))]
  (map countItems collection)))

(defn second-elements [collection]
  (let [getSecond (fn [coll] (get coll 1))]
    (map getSecond collection)
  )
)

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (let [authors
         (fn [book] (:authors book))]
    (set (apply concat (map authors books)))))

(defn all-author-names [books]
   (set (map :name (authors books))))

(defn author->string [author]
  (let [authorname (:name author)
        life (str "(" (:birth-year author) " - " (:death-year author) ")" )]
  (if (contains? author :birth-year)
    (str authorname " " life)
    authorname)))

(defn authors->string [authors]
  ;/(let [authornames (map author->string authors)]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (if (= 1 (count books))
      (str (count books) " book. " (apply str (interpose ". " (map book->string books))) ".")
      (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (let [hasauth
        (fn [author] (= (:name author) name))]
  (first (filter hasauth authors))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
