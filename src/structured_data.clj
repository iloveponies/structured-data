(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)
    )
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    )
  )

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (and (<= x1 a x2) (<= y1 b y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[point1 point2] inner]
    (and (contains-point? outer point1) (contains-point? outer point2))
    )
  )

(defn title-length [book]
  (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (< 1 (author-count book))
  )

(defn add-author [book new-author]
    (assoc book :authors (conj (get book :authors) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection)
  )

(defn second-elements [collection]
  (map (fn [vektori] (get vektori 1)) collection)
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false
    )
  )

(defn stars [n]
  (apply str (repeat n "*"))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
    (not (== (count a-seq) (count (set a-seq))))
    )

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors))))

(defn has-author? [book author]
  (contains? (get book :authors) author)
  )

(defn authors [books]
  (let [authorssit (fn [book] (:authors book))]
    (apply clojure.set/union (map authorssit books))
    )
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [nimi (:name author)
        byear (:birth-year author)
        dyear (:death-year author)]
    (cond
      (contains? author :birth-year) (str nimi " (" byear " - " dyear ")")
      :else (str nimi)
    )
    )
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [booktitle (:title book)
        authorssit (authors->string (:authors book))]
    (str booktitle ", written by " authorssit))
  )

(defn books->string [books]
  (let [n (count books)
        allBooks (apply str (interpose ". " (map book->string books)))]
    (cond
      (== 0 n) "No books."
      (== 1 n) (str "1 book. " allBooks ".")
      :else (str n " books. " allBooks ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors))
  )

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (< 0 (count (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
