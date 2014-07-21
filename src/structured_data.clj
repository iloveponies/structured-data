(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)
        y (get v 2)]
    (+ x y)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z]v]
    (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- x2 x1) ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (cond
     (= (width rectangle) (height rectangle)) true
     :else false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (width rectangle)(height rectangle))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [z1 z2] point]
    (cond
     (and (<= x1 z1 x2) (<= y1 z2 y2)) true
     :else false)
    ))

(defn contains-rectangle? [outer inner]
  (let [
        [[x1 y1] [x2 y2]] outer
        [[u1 v1][u2 v2]] inner
        ]
    (cond
    (and
     (contains-point? (rectangle [x1 y1] [x2 y2]) (point u1 v1) )
     ;(contains-point? (rectangle outer) (point u1 v1) )
     (contains-point? (rectangle [x1 y1] [x2 y2]) (point u2 v2) )) true
     :else false)))

(defn title-length [book]
 (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (cond
   (< 1 (count (get book :authors)))true
   :else false))

(defn add-author [book new-author]
  (let [authors (get book :authors)
        title (get book :title)]
       ;(assoc {} title (conj new-author authors))
    (assoc book :authors (conj authors new-author))
    ))

(defn alive? [author]
  (cond
   (contains? author :death-year) false
   :else true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [v] (get v 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (cond
  (or
   (apply < a-seq )
   (apply >= a-seq )) true
   :else false))


(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else  (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (cond
   (= (count a-seq) (count (set a-seq))) false
   :else true))

(defn old-book->new-book [book]
  (let [authors (get book :authors)]
    (assoc book :authors (set authors))
   ; (assoc book :authors (into #{} authors)
  ))

(defn has-author? [book author]
  (let [authors (get book :authors)]
    (contains? authors author)))

(defn authors [books]
  (let [author-names
        (fn [book]  (:authors book))]
    (set (apply clojure.set/union (map author-names books)))))


(defn all-author-names [books]
   (let [author-names
        (fn [book] (map :name (:authors book)))]
    (set (apply clojure.set/union (map author-names books)))))

(defn author->string [author]
  (let [name (get author :name)
        birth (get author :birth-year)
        death (get author :death-year)
        ]
    (cond
     (= (contains? author :birth-year) false) (str name)
     :else (str name " (" birth " - " death ")"))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (get book :title)
        authors (authors->string (:authors book))]
    (str title ", written by " authors)))

(defn books->string [books]
  (cond
   (= (count books) 0) (str "No books.")
   (= (count books) 1) (str "1 book. " (apply str (interpose ". " (map book->string books)))".")
   :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")))

(defn books-by-author [author books]
  (filter (fn[x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (cond
   (empty? authors) nil
   (= (get (first authors) :name) name) (first authors)
   :else (author-by-name name (rest authors))))

;(defn living-authors [authors]
;  (cond
;   (empty? authors) '()
;   (alive? (first authors))
;     (cons (first authors)
;           (living-authors (rest authors)))
;   :else (living-authors (rest authors))))

(defn living-authors [authors]
  (filter alive? authors))




(defn has-a-living-author? [book]
  (pos? (count (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))


; %________%
