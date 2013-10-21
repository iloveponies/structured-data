(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)])
  (Math/pow (+ x x) (+ x x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c d] v]
    (+ a c)))

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
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [px py] point]
    (if
      (and (<= x1 px x2) (<= y1 py y2))
      true false)))


(defn contains-rectangle? [outer inner]
  (let [[[ix1 iy1] [ix2 iy2]] inner]
    (if
      (and
       (contains-point? outer
                        (point ix1 iy1))
       (contains-point? outer
                        (point ix2 iy2)))
      true false)))




(defn title-length [book]
  (count
   (get book :title)))

(defn author-count [book]
  (count
   (get book :authors)))

(defn multiple-authors? [book]
  (if
    (>
     (author-count book)
     1)
    true false))


(defn add-author [book new-author]
  (let [authors (book :authors)]
    (assoc book :authors
      (conj authors new-author))))



(defn alive? [author]
  (if (author :death-year) false true))


(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second
       (seq collection)))

(defn titles [books]
  (map :title books))


(defn monotonic? [a-seq]
  (or
   (apply <= a-seq)
   (apply >= a-seq)))


(defn stars [n]
  (let [sekvenssi (repeat n "*")]
    (apply str sekvenssi)))


(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (>
   (count a-seq)
   (count
    (set a-seq))))

(defn old-book->new-book [book]
  (let [authors (book :authors)]
    (assoc book :authors
      (set authors))))

(defn has-author? [book author]
  (contains? (book :authors) author))


(defn authors [books]
  (apply clojure.set/union (set (map :authors books))))


(defn all-author-names [books]
  (set (map :name
       (authors books))))



(defn author->string [author]
  (let [name (:name author) birth (:birth-year author) death (:death-year author)]
    (cond
     death (str name " (" birth " - " death ")")
     birth (str name " (" birth " - " ")")
     :else (str name))))



(defn authors->string [authors] (apply str (interpose ", "(concat (map author->string authors)))))


(defn book->string [book]
  (str
   (:title book) ", written by "
   (authors->string
    (:authors book))))


(defn books->string [books]
  (let [kirjat
        (apply str
               (interpose ", "
                          (concat
                           (map book->string books))))]
    (cond
     (== (count books) 0)
     (str "No books.")
     (== (count books) 1)
     (str "1 book. " kirjat ".")
     :else (str
            (count books) " books. " kirjat "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books ))


(defn author-by-name [name authors]
  (let[kirjailija (filter (fn [author]
           (= (:name author) name))
          authors)]
    (if (not-empty kirjailija) (first kirjailija) nil)))


(defn living-authors [authors]
  (filter (fn [author]
             (alive? author))
           authors))

(defn has-a-living-author? [book]
  (if
    (empty?
     (living-authors
      (:authors book)))
    false true))

(defn books-by-living-authors [books]
  (filter (fn [book]
            (has-a-living-author? book))
          books))

; %________%



