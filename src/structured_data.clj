(ns structured-data)

(defn do-a-thing [x]
  (let [t (+ x x)]
    (Math/pow t t))
  )

(defn spiff [v]
  (if (vector? v) (if (> (count v) 2) (+ (get v 0) (get v 2)) nil) nil)
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
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (== (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x3 y3]] rectangle
		 [x2 y2] point]
		   (and (<= x1 x2 x3) (<= y1 y2 y3))
  ))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
	      (and (contains-point? outer p1) (contains-point? outer p2))
  ))

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (> (author-count book) 1)
  )

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second-element (fn [x] (first (rest x)))]
         (map second-element collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq))
  )

(defn stars [n]
  (apply str (repeat n \*))
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (let [author (fn [book] (:authors book))]
          (apply clojure.set/union (map author books)))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [n (:name author)
		by (:birth-year author)
		dy (:death-year author)]
          (cond
            (contains? author :death-year) (apply str [n " (" by " - " dy ")"])
			(contains? author :birth-year) (apply str [n " (" by " - )"])
			:else n
		  )
  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (apply str [(:title book) ", written by " (authors->string (:authors book))])
  )

(defn books->string [books]
  (cond
    (== (count books) 0) "No books."
	(== (count books) 1) (apply str ["1 book. " (apply str (map book->string books)) "."])
	(> (count books) 1) (apply str [(count books) " books. " (apply str (interpose ". " (map book->string books))) "."])
  ))

(defn books-by-author [author books]
  (let [has-this-author? (fn [book] (has-author? book author))]
          (filter has-this-author? books)
  ))

(defn author-by-name [name authors]
  (let [author-of-this-name? (fn [author] (= (:name author) name))]
          (first (filter author-of-this-name? authors))
  ))

(defn living-authors [authors]
  (filter alive? authors)
  )

(defn has-a-living-author? [book]
  (contains? (set (map alive? (:authors book))) true)
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books)
  )

; %________%
