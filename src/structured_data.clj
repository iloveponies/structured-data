(ns structured-data)

(defn do-a-thing [x]
  (let [dubl-x (+ x x)]
    (Math/pow dubl-x dubl-x)
    )
  )

(defn spiff [v]
  (let [frst (get v 0)
        thrd (get v 2)]
    (+ frst thrd)
    )
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[frst scnd thrd] v]
    (+ frst thrd)
    )
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
  (== (width rectangle) (height rectangle))
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [rectangle point]
  (let [[[[x1 y1] [x2 y2]] [px py]] [rectangle point]]
    (and
      (<= x1 px x2)
      (<= y1 py y2))
    )
  )

(defn contains-rectangle? [outer inner]
  (let [[outer [inner-bot-lft inner-top-rgt]] [outer inner]]
    (and
      (contains-point? outer inner-bot-lft)
      (contains-point? outer inner-top-rgt)
      )
    )
  )

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
;  (assoc (:authors book) :name (:name new-author))
  (assoc book :authors (conj (:authors book) new-author))
  )

(defn alive? [author]
  (not (contains? author :death-year))
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (map (fn [x] (get x 1)) collection)
  )

(defn titles [books]
  (map :title books)
  )

(defn stars [n]
  (apply str (concat (repeat n "*")))
  )

(defn monotonic? [a-seq]
  (or 
    (apply <= a-seq)
    (apply >= a-seq)
    )
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq))
     (count a-seq)
     )
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (apply clojure.set/union (map :authors books))
  )

(defn all-author-names [books]
  (set (map :name (authors books)))
  )

(defn author->string [author]
  (let [[auth-name birth-year death-year] [(:name author) (:birth-year author) (:death-year author)]]
    (if birth-year 
      (apply str (list auth-name " (" birth-year " - " death-year ")"))
      auth-name
      ))
  )

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (let [[title authors] [(:title book) (:authors book)]]
    (apply str (list title ", written by " (authors->string authors)))
    )
  )

(defn books->string [books]
  (cond 
    (> (count books) 1) (apply str (flatten (list (count books) " books. " (interpose ", " (map book->string books)) ".")))
    (= (count books) 1) (apply str (list "1 book. " (book->string (first books)) "."))
    :else "No books."
    )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors)
  )

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter (fn [x] (has-a-living-author? x)) books)
  )

; %________%
