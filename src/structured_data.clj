(ns structured-data)

(defn do-a-thing [x]
  (let [y (+ x x)]
    (Math/pow y y))
  )

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[first second third] v]
    (+ first third)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- x1 x2)))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (Math/abs (- y1 y2)))
  )

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle))
    true
    false)
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (and (<= x1 x x2) (<= y1 y y2)))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] inner]
    (let [left (point x1 y1)
          right (point x2 y2)]
      (and (contains-point? outer left) (contains-point? outer right)))
  )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
  (let [authors (conj (:authors book) new-author)]
    (assoc book :authors authors)
    )
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [seconds (fn [coll] (get coll 1))]
    (seq (map seconds collection)))
)

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq))
  )

(defn stars [n]
  (apply str (concat (repeat n "*")))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [a-set (set a-seq)]
    (not (== (count a-seq) (count a-set))))
  )

(defn old-book->new-book [book]
  (let [old-authors (:authors book)]
    (let [new-authors (set old-authors)]
      (assoc book :authors new-authors)
      )
    )
  )

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author))
  )

(defn authors [books]
  (let [all (map :authors books)]
    (apply clojure.set/union all))
  )

(defn all-author-names [books]
  (let [all (authors books)]
    (set (map :name all)))
  )

(defn author->string [author]
  (let [auth [(:name author) " (" (:birth-year author) " - " (:death-year author) ")"]]
    (if (:birth-year author)
      (apply str auth)
      (:name author))
    )
  )

(defn authors->string [authors]
  (let [strings (map author->string authors)]
    (apply str (interpose ", " strings)))
  )

(defn book->string [book]
  (let [titl (:title book)]
    (let [auth (authors->string (:authors book))]
      (apply str (interpose ", written by " [titl auth]))))
  )

(defn books->string [books]
  (let [strings (apply str (interpose ". " (map book->string books)))]
    (cond
     (== 0 (count books)) "No books."
     (== 1 (count books)) (apply str (conj [(str "1 book. ")] strings "."))
     :else (apply str (conj [(str (count books) " books. ")] strings "."))))
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (let [filtered (filter (fn [author] (== 0 (compare (:name author) name))) authors)]
    (if (== 0 (count filtered))
      nil
      filtered))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
  )

; %________%
