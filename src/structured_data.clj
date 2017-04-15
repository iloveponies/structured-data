(ns structured-data)

(defn do-a-thing [x]
  (let [a (+ x x)]
    (Math/pow a a)))

(defn spiff [v]
  (+ (get v 0) (or (get v 2) 0) ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[q w e] v]
    (+ q e)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x0 y0][x1 y1]] rectangle]
    (- x1 x0)))

(defn height [rectangle]
  (let [[[x0 y0][x1 y1]] rectangle]
    (- y1 y0)))

(defn square? [rectangle]
  (== (height rectangle) (width rectangle) ))

(defn area [rectangle]
  (* (width rectangle) (height rectangle) ))

(defn contains-point? [rectangle point]
  (let [[[x0 y0][x1 y1]] rectangle
        [px py] point]
         (and (<= x0 px x1) (<= y0 py y1)) ))

(defn contains-rectangle? [outer inner]
  (let [[[x0 y0][x1 y1]] inner
        f (point x0 y0)
        s (point x1 y1)]
    (and (contains-point? outer f) (contains-point? outer s))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (if (vector? (:authors book))
    (count (:authors book))
    1))

(defn multiple-authors? [book]
  (let [a (:authors book)]
    (if (vector? a)
      (if (> (count a) 1)
        true
        false)
      false)
    ))

(defn add-author [book new-author]
  (let [auths (conj (:authors book) new-author)]
    (assoc book :authors auths)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [f (fn [c] (count c))]
    (map f collection)
    ))

(defn second-elements [collection]
  (let [f (fn [c] (get c 1))]
    (map f collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (let [a (disj a-set elem)]
    (if (= a-set a)
      (conj a-set elem)
      a)))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (clojure.set/union (authors books)))))

(defn author->string [author]
  (str (:name author)
       (if (contains? author :birth-year)
           (str " (" (:birth-year author)
                " - "
                (:death-year author)
                ")"))))

(defn authors->string [authors]
  (cond
   (empty? authors) ""
   :else (apply str (interpose ", " (map author->string authors)))
   ))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)) ))

(defn books->string [books]
  (cond
   (empty? books) "No books."
   :else (str (count books) " book" (if (> (count books) 1) "s") ". "
              (apply str (interpose ". " (set (map book->string books))))
              ".")))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [a] (= name (:name a))) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%



