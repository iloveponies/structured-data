(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [x (get v 0)]
  (let [b (get v 2)]
  (+ x b))))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
 (let [[x b][spiff v]]
   (x b)))

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
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (= (- x2 x1) (- y2 y1)) true false)))

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (let [[x y] point]
      (and (<= x1 x x2) (<= y1 y y2)))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (let [p1 (point x1 y1)]
      (let [p2 (point x2 y2)]
        (and (contains-point? outer p1) (contains-point? outer p2))))))


(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (if (> (count (book :authors)) 1) true false))

(defn add-author [book new-author]
  (let [auths (book :authors)]
    (assoc book :authors (conj auths new-author))))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (let [length
        (fn [x] (count x))]
    (map length collection)))

(defn second-elements [collection]
  (let [seconds
        (fn [x] (get x 1))]
    (map seconds collection)))


(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (let [stars (fn [x] (repeat x \*))]
    (apply str (stars n))))

(defn toggle [a-set elem]
  (if (= (if (contains? a-set elem) true false) false)
    (conj a-set elem) (disj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [elements (count (set a-seq))]
    (let [length (count a-seq)]
      (if (< elements length) true false))))

(defn old-book->new-book [book]
  (let [authors (set (:authors book))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (let [authors (set (book :authors))]
    (if (contains? authors author) true false)))

(defn authors [books]
  (let [authors (map :authors books)]
    (let [union (fn [x] (apply clojure.set/union x))]
      (union authors))))

(defn all-author-names [books]
  (let [authors
        (fn [x] (map :name (:authors x)))]
    (let [combine
          (fn [x] (set (apply concat (map authors books))))]
      (combine authors))))

(defn author->string [author]
  (let [nimi (:name author)]
    (let [birthday (:birth-year author)]
      (let [demise (:death-year author)]
      (if (> (count (str birthday)) 0)
        (str nimi " ("birthday " - "demise")")
        (str nimi))))))

(defn authors->string [authors]
  (let [queue (fn [authors] (apply str (interpose ", " authors)))]
  (queue (map author->string authors))))

(defn book->string [book]
  (let [nimi (:title book) authors (:authors book)]
  (apply str nimi ", written by " (authors->string authors))))

(defn books->string [books]
  (let [queue (fn [books] (apply str (interpose ". " books )))
        manybooks (count (set books))]
  (cond (> manybooks 1)
          (apply str manybooks " books. "(queue (map book->string books))".")
        (= manybooks 1)
          (apply str manybooks " book. "(queue (map book->string books))".")
        (= manybooks 0)
          (apply str "No books."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [kirjailija] (= name (:name kirjailija))) authors)))


(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))
; %________%
