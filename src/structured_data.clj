(ns structured-data)

(defn do-a-thing [x]
  (let [addX (+ x x)]
    (
      Math/pow addX addX)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v (str "<3")))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)
    ))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)
    ))

(defn square? [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (== h w)
    ))

(defn area [rectangle]
  (let [h (height rectangle)
        w (width rectangle)]
    (* h w)
    ))


  (defn contains-point? [rectangle point]
    (let [
          [[x1 y1] [x2 y2]] rectangle
          [px py] point
          ]
      (and
        (<= x1 px x2)
        (<= y1 py y2))))

(defn contains-rectangle? [outer inner]
  (let [
        [[x1 y1] [x2 y2]] inner
        ]
    (and
      (contains-point? outer [x1 y1])
      (contains-point? outer [x2 y2])
      )))

(defn title-length [book]
  (count (:title book)))


(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [oldAuthors (:authors book)
        newAuthors (conj oldAuthors new-author)]
    (assoc book :authors newAuthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [getSecondEl (fn [v] (get v 1))]
    (map getSecondEl collection)))

(defn titles [books]
  (map :title books))

(defn  monotonic? [a-seq]
  (let [
        incMon (apply <= a-seq)
        decMon (apply >= a-seq)
        ]
    (cond
      incMon true
      decMon true
      :else false
      )))

(defn stars [n]
  (apply str(repeat n "*")))

(defn toggle [a-set elem]
  (let [
        add (conj a-set elem)
        rem (disj a-set elem)
        ]
    (if (contains? a-set elem) rem add)
    ))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [
        name (:name author)
        birthYear (:birth-year author)
        deathYear (:death-year author)
        ]
    (cond
      (not (nil? deathYear)) (str name " (" birthYear " - " deathYear ")")
      (not (nil? birthYear)) (str name " (" birthYear " - )")
      :else (str name)
      )
    ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string  authors))))

(defn book->string [book]
  (let [btitle (:title book)]
    (str btitle ", written by " (authors->string (:authors book)))))

(defn books->string [books]
  (let [
        size (count books)
        printBooks (if (> size 1) " books. " " book. ")
        printDot (if (> size 1) "" ".")
        ]
    (cond
      (== size 0) (str "No books.")
      :else (str size printBooks (apply str (interpose ". " (map book->string books))) printDot)
      )))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author )) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
